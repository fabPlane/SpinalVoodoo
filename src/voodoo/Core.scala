package voodoo

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb._
import voodoo.core.{
  CoreCpuFrontdoor,
  FramebufferLayout,
  FramebufferMemSubsystem,
  PixelPipeline,
  TextureMemSubsystem
}
import voodoo.hdmi.{HdmiCdcFramebufferScanout, HdmiScanoutPort, VideoTiming}
import voodoo.texture.TextureMem

object Core {
  val cpuBmbParams = BmbParameter(
    addressWidth = 24,
    dataWidth = 32,
    sourceWidth = 4,
    contextWidth = 0,
    lengthWidth = 2,
    canRead = true,
    canWrite = true,
    alignment = BmbParameter.BurstAlignement.WORD
  )

  def fbMemBmbParams(c: Config) = BmbParameter(
    addressWidth = c.addressWidth.value,
    dataWidth = 32,
    sourceWidth = 1 + log2Up(5), // Room for framebuffer readers/writers plus scanout routing
    contextWidth = 0,
    lengthWidth = c.memBurstLengthWidth,
    canRead = true,
    canWrite = true,
    alignment = BmbParameter.BurstAlignement.BYTE
  )

  def texMemBmbParams(c: Config) = BmbParameter(
    addressWidth = c.addressWidth.value,
    dataWidth = 32,
    sourceWidth = 4 + log2Up(3), // max(srcW=1,4) + 2 route bits for 3 inputs = 6
    contextWidth = 0,
    lengthWidth = c.memBurstLengthWidth,
    canRead = true,
    canWrite = true,
    alignment = BmbParameter.BurstAlignement.BYTE
  )

  // Internal CPU texture write bus params
  val cpuTexBmbParams = BmbParameter(
    addressWidth = 26,
    dataWidth = 32,
    sourceWidth = 4,
    contextWidth = 0,
    lengthWidth = 6,
    canRead = true,
    canWrite = true,
    alignment = BmbParameter.BurstAlignement.BYTE
  )
}

case class Core(c: Config) extends Component {
  val io = new Bundle {
    // Unified CPU bus (24-bit address covers 16MB PCI BAR)
    val cpuBus = slave(Bmb(Core.cpuBmbParams))

    // Framebuffer memory buses
    val fbMemWrite = master(Bmb(Core.fbMemBmbParams(c)))
    val fbColorWriteMem = master(Bmb(Core.fbMemBmbParams(c)))
    val fbAuxWriteMem = master(Bmb(Core.fbMemBmbParams(c)))
    val fbColorReadMem = master(Bmb(Core.fbMemBmbParams(c)))
    val fbAuxReadMem = master(Bmb(Core.fbMemBmbParams(c)))

    // Texture memory bus (R/W, internal 2-port arbitration with texBaseAddr relocation)
    val texMem = master(Bmb(Core.texMemBmbParams(c)))

    // Status inputs (hardware state)
    val statusInputs = in(new Bundle {
      val vRetrace = Bool()
      val memFifoFree = UInt(16 bits)
      val pciInterrupt = Bool()
    })

    // SwapBuffer outputs (active buffer index and pending swap count)
    val swapDisplayedBuffer = out UInt (2 bits)
    val swapsPending = out UInt (3 bits)

    // Framebuffer base address
    val fbBaseAddr = in UInt (c.addressWidth)

    // Simulation-only framebuffer cache flush request
    val flushFbCaches = in Bool ()

    // Integrated scanout path. Board wrappers provide the physical HDMI transmitter/clocking.
    val hdmi = master(HdmiScanoutPort(c))

    // Board-level bring-up counters for the framebuffer scanout prefill path.
    val scanoutPrefetchCount = out UInt (16 bits)
    val scanoutReadReqCount = out UInt (16 bits)
    val scanoutReadRspCount = out UInt (16 bits)
    val scanoutCacheDebug = out Bits (64 bits)
    val scanoutCacheReadAddr = out UInt (c.addressWidth)
    val scanoutCacheExpectedAddr = out UInt (c.addressWidth)
    val scanoutCacheRemaining = out UInt (log2Up(c.maxFbDims._1 + 1) bits)
    val scanoutCacheOccupancy = out Bits (32 bits)
    val scanoutFillHits = out UInt (32 bits)
    val scanoutFillMisses = out UInt (32 bits)
    val scanoutFillBurstCount = out UInt (32 bits)
    val scanoutFillBurstBeats = out UInt (32 bits)
    val scanoutFillStallCycles = out UInt (32 bits)
    val scanoutSampleMeta = out Bits (32 bits)
    val scanoutSampleHash = out Bits (32 bits)
    val scanoutSampleLast = out Bits (32 bits)
    val scanoutSampleWords = out Vec(Bits(32 bits), 12)

    // One bit per ordered-pipeline busy source. Board diagnostics use this to
    // identify the stage preventing a queued synchronization command from
    // draining without needing an embedded logic analyzer.
    val pipelineBusySources = out Bits (32 bits)

  }
  val addressRemapper =
    AddressRemapper(RegisterBank.externalBmbParams(c), RegisterBank.bmbParams(c))
  val regBank = RegisterBank(c)
  val pciFifo = PciFifo(
    busParams = RegisterBank.bmbParams(c),
    categories = regBank.busif.getCategories,
    floatAliases = regBank.busif.getFloatAliases,
    commandAddresses = regBank.busif.getCommandStreamReady.keys.toSeq.sorted
  )

  val frontdoor = CoreCpuFrontdoor(c)
  val pixelPipeline = PixelPipeline(c)
  val framebufferMem = FramebufferMemSubsystem(c)
  val textureMem = TextureMemSubsystem(c)
  val hdmiScanout = HdmiCdcFramebufferScanout(
    c,
    timing = if (c.hdmiScanoutDmt640x480) VideoTiming.dmt640x480 else VideoTiming.cea720x480p,
    pixelRepeatX = c.hdmiScanoutPixelRepeatX
  )

  io.pipelineBusySources := pixelPipeline.io.debug.pipelineBusySources

  val controlPlane = new Area {
    val swapBuffer = SwapBuffer()
    val framebufferLayout =
      FramebufferLayout.fromRegisterBank(c, io.fbBaseAddr, regBank, swapBuffer)

    frontdoor.io.cpuBus <> io.cpuBus
    frontdoor.io.regBus <> addressRemapper.io.input
    frontdoor.io.lfbBus <> pixelPipeline.io.lfbBus
    frontdoor.io.texReadBus <> textureMem.io.cpuTexRead
    pciFifo.io.texWrite << frontdoor.io.texWrite
    frontdoor.io.texBaseAddr := regBank.tmuConfig.texBaseAddr

    addressRemapper.io.output <> pciFifo.io.cpuSide
    pciFifo.io.regSide <> regBank.io.bus
    regBank.io.floatShadow <> pciFifo.io.floatShadow
    regBank.io.statusInputs <> io.statusInputs
    regBank.commands.nopCmd.ready := True

    swapBuffer.io.cmd << regBank.commands.swapbufferCmd
    swapBuffer.io.vRetrace := io.statusInputs.vRetrace
    swapBuffer.io.vsyncEnable := regBank.commands.swapVsyncEnable
    swapBuffer.io.swapInterval := regBank.commands.swapInterval
    swapBuffer.io.swapCmdEnqueued := pciFifo.io.wasEnqueued

    regBank.io.swapDisplayedBuffer := framebufferLayout.displayedBuffer
    regBank.io.swapsPending := framebufferLayout.swapsPending
    regBank.io.drawRouting := framebufferLayout.draw
    io.swapDisplayedBuffer := framebufferLayout.displayedBuffer
    io.swapsPending := framebufferLayout.swapsPending

    hdmiScanout.io.regs.frontBase := framebufferLayout.front
    hdmiScanout.io.regs.backBase := framebufferLayout.back
    hdmiScanout.io.regs.pixelStride := framebufferLayout.draw.pixelStride
    hdmiScanout.io.regs.displayWidth := 640
    hdmiScanout.io.regs.displayHeight := 480
    hdmiScanout.io.regs.framebufferEnable := Bool(c.enableHdmiScanout)
    hdmiScanout.io.regs.testPatternEnable := False
    hdmiScanout.io.regs.gammaLut := regBank.io.gammaLut

    pixelPipeline.io.controls := PixelPipeline.Controls.fromRegisterBank(
      c,
      regBank,
      framebufferLayout
    )
    pixelPipeline.io.externalBusy := PixelPipeline.ExternalBusy.fromCore(regBank, swapBuffer)
    textureMem.io.downloadConfig := TextureMem.DownloadConfig.fromRegisterBank(c, regBank)
  }

  pixelPipeline.io.triangleCmd << regBank.commands.triangleCmd
  pixelPipeline.io.ftriangleCmd << regBank.commands.ftriangleCmd
  pixelPipeline.io.fastfillCmd << regBank.commands.fastfillCmd
  pixelPipeline.io.paletteWrite << regBank.io.paletteWrite
  pixelPipeline.io.tmuInvalidate := frontdoor.io.invalidate
  pixelPipeline.io.pciFifoEmpty := pciFifo.io.fifoEmpty
  pixelPipeline.io.fbStatus := framebufferMem.io.status
  pixelPipeline.io.fbStats := framebufferMem.io.stats

  framebufferMem.io.colorWrite << pixelPipeline.io.colorWrite
  framebufferMem.io.auxWrite << pixelPipeline.io.auxWrite
  framebufferMem.io.colorReadReq << pixelPipeline.io.colorReadReq
  pixelPipeline.io.colorReadRsp << framebufferMem.io.colorReadRsp
  framebufferMem.io.scanoutPrefetchReq << hdmiScanout.io.prefetchReq
  framebufferMem.io.scanoutReadReq << hdmiScanout.io.readReq
  hdmiScanout.io.readRsp << framebufferMem.io.scanoutReadRsp

  val scanoutPrefetchCount = Reg(UInt(16 bits)) init (0)
  val scanoutReadReqCount = Reg(UInt(16 bits)) init (0)
  val scanoutReadRspCount = Reg(UInt(16 bits)) init (0)
  val scanoutSampleWords = Vec(Reg(Bits(32 bits)) init (0), 12)
  val scanoutSampleLow = Reg(Bits(16 bits)) init (0)
  val scanoutSampleHalf = RegInit(False)
  val scanoutSampleIndex = Reg(UInt(4 bits)) init (0)
  val scanoutSampleNonzero = Reg(UInt(5 bits)) init (0)
  // Cumulative nonzero readback count. Unlike a rolling hash, this remains
  // unambiguous after power-up garbage has been cleared from SDRAM.
  val scanoutSampleHash = Reg(Bits(32 bits)) init (0)
  val scanoutSampleLast = Reg(Bits(16 bits)) init (0)
  when(hdmiScanout.io.prefetchReq.fire) {
    scanoutPrefetchCount := scanoutPrefetchCount + 1
  }
  when(hdmiScanout.io.readReq.fire) {
    scanoutReadReqCount := scanoutReadReqCount + 1
  }
  when(hdmiScanout.io.readRsp.fire) {
    scanoutReadRspCount := scanoutReadRspCount + 1
    scanoutSampleLast := hdmiScanout.io.readRsp.data
    when(hdmiScanout.io.readRsp.data =/= 0) {
      scanoutSampleHash := (scanoutSampleHash.asUInt + 1).asBits
    }
    when(!scanoutSampleHalf) {
      scanoutSampleLow := hdmiScanout.io.readRsp.data
      scanoutSampleHalf := True
      when(scanoutSampleIndex === 0) {
        scanoutSampleNonzero := Mux(
          hdmiScanout.io.readRsp.data =/= 0,
          U(1, 5 bits),
          U(0, 5 bits)
        )
      } elsewhen (hdmiScanout.io.readRsp.data =/= 0) {
        scanoutSampleNonzero := scanoutSampleNonzero + 1
      }
    }.otherwise {
      scanoutSampleWords(scanoutSampleIndex) := hdmiScanout.io.readRsp.data ## scanoutSampleLow
      scanoutSampleHalf := False
      when(hdmiScanout.io.readRsp.data =/= 0) {
        scanoutSampleNonzero := scanoutSampleNonzero + 1
      }
      when(scanoutSampleIndex === 11) {
        scanoutSampleIndex := 0
      }.otherwise {
        scanoutSampleIndex := scanoutSampleIndex + 1
      }
    }
  }
  io.scanoutPrefetchCount := scanoutPrefetchCount
  io.scanoutReadReqCount := scanoutReadReqCount
  io.scanoutReadRspCount := scanoutReadRspCount
  io.scanoutCacheDebug := framebufferMem.io.scanoutCacheDebug
  io.scanoutCacheReadAddr := framebufferMem.io.scanoutCacheReadAddr
  io.scanoutCacheExpectedAddr := framebufferMem.io.scanoutCacheExpectedAddr
  io.scanoutCacheRemaining := framebufferMem.io.scanoutCacheRemaining
  io.scanoutCacheOccupancy := framebufferMem.io.scanoutCacheOccupancy
  io.scanoutFillHits := framebufferMem.io.scanoutFillHits
  io.scanoutFillMisses := framebufferMem.io.scanoutFillMisses
  io.scanoutFillBurstCount := framebufferMem.io.scanoutFillBurstCount
  io.scanoutFillBurstBeats := framebufferMem.io.scanoutFillBurstBeats
  io.scanoutFillStallCycles := framebufferMem.io.scanoutFillStallCycles
  io.scanoutSampleMeta := B(0, 6 bits) ## scanoutSampleHalf.asBits ##
    scanoutSampleIndex.asBits ## scanoutSampleNonzero.asBits ## scanoutReadRspCount.asBits
  io.scanoutSampleHash := scanoutSampleHash
  io.scanoutSampleLast := framebufferMem.io.scanoutCacheReadAddr.resize(16 bits).asBits ## scanoutSampleLast
  io.scanoutSampleWords := scanoutSampleWords
  framebufferMem.io.auxReadReq << pixelPipeline.io.auxReadReq
  pixelPipeline.io.auxReadRsp << framebufferMem.io.auxReadRsp
  framebufferMem.io.prefetchColor <> pixelPipeline.io.prefetchColor
  framebufferMem.io.prefetchAux <> pixelPipeline.io.prefetchAux
  framebufferMem.io.lfbReadBus <> pixelPipeline.io.lfbReadBus
  framebufferMem.io.flush := io.flushFbCaches
  framebufferMem.io.fbMemWrite <> io.fbMemWrite
  framebufferMem.io.fbColorWriteMem <> io.fbColorWriteMem
  framebufferMem.io.fbAuxWriteMem <> io.fbAuxWriteMem
  framebufferMem.io.fbColorReadMem <> io.fbColorReadMem
  framebufferMem.io.fbAuxReadMem <> io.fbAuxReadMem

  textureMem.io.cpuTexDrain << pciFifo.io.texDrain
  textureMem.io.tmuTexRead <> pixelPipeline.io.texRead
  textureMem.io.texMem <> io.texMem

  io.hdmi.video := hdmiScanout.io.video
  io.hdmi.status := hdmiScanout.io.status
  io.hdmi.underflow := hdmiScanout.io.underflow
  io.hdmi.fifoLevel := hdmiScanout.io.fifoPushOccupancy.resized
  hdmiScanout.io.hdmiClock := io.hdmi.clock
  hdmiScanout.io.hdmiReset := io.hdmi.reset

  regBank.io.statistics := pixelPipeline.io.stats
  regBank.io.debug := pixelPipeline.io.debug

  pciFifo.io.pipelineBusy := pixelPipeline.io.debug.pipelineBusy
  pciFifo.io.commandReady := regBank.io.commandReady
  pciFifo.io.wasEnqueuedAddr := U(0x128, pciFifo.busAddrWidth bits)
  regBank.io.pciFifoEmpty := pciFifo.io.fifoEmpty
  regBank.io.pciFifoFree := pciFifo.io.pciFifoFree
  regBank.io.swapCmdEnqueued := pciFifo.io.wasEnqueued
  regBank.io.syncDrained := pciFifo.io.syncDrained
}
