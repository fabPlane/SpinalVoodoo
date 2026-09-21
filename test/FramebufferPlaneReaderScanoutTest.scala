//> using target.scope test

package voodoo

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

class FramebufferPlaneReaderScanoutTest extends AnyFunSuite {
  private val config = Config
    .voodoo1()
    .copy(
      addressWidth = 12 bits,
      memBurstLengthWidth = 6,
      maxFbDims = (16, 16),
      trace = TraceConfig(enabled = true)
    )

  test("scanout waits for prefetch instead of issuing a startup direct miss") {
    SimConfig.withIVerilog
      .compile(FramebufferPlaneReader(config, suppressStartupDirectMiss = true))
      .doSim { dut =>
        def waitUntil(label: String, limit: Int = 100)(condition: => Boolean): Unit = {
          var cycles = 0
          while (!condition && cycles < limit) {
            dut.clockDomain.waitSampling()
            cycles += 1
          }
          assert(condition, s"timed out waiting for $label after $limit cycles")
        }

        dut.clockDomain.forkStimulus(10)
        dut.io.prefetchReq.valid #= false
        dut.io.prefetchReq.startAddress #= 0
        dut.io.prefetchReq.endAddress #= 0
        dut.io.readReq.valid #= false
        dut.io.readReq.address #= 0
        dut.io.readRsp.ready #= true
        // Hold the command until after observing it.  If ready is high here,
        // the prefetch command can fire in the same cycle that prefetchReq is
        // accepted and the testbench would miss the one-cycle valid pulse.
        dut.io.mem.cmd.ready #= false
        dut.io.mem.rsp.valid #= false
        dut.io.mem.rsp.last #= true
        dut.io.mem.rsp.fragment.data #= 0
        dut.io.mem.rsp.fragment.source #= 0
        dut.io.mem.rsp.fragment.context #= 0
        dut.io.mem.rsp.fragment.opcode #= 0
        dut.clockDomain.waitSampling()

        dut.io.readReq.valid #= true
        dut.io.readReq.address #= 0x000
        for (_ <- 0 until 3) {
          assert(!dut.io.readReq.ready.toBoolean)
          assert(!dut.io.mem.cmd.valid.toBoolean)
          dut.clockDomain.waitSampling()
        }

        dut.io.prefetchReq.valid #= true
        dut.io.prefetchReq.startAddress #= 0x100
        dut.io.prefetchReq.endAddress #= 0x106
        waitUntil("prefetch request ready") { dut.io.prefetchReq.ready.toBoolean }
        dut.clockDomain.waitSampling()
        dut.io.prefetchReq.valid #= false
        waitUntil("frame-wrap recovery command") { dut.io.mem.cmd.valid.toBoolean }
        // Once a prefetch exists, the still-pending lower address is a real
        // backward/frame-wrap access and must recover through a direct miss.
        assert(dut.io.mem.cmd.fragment.address.toLong == 0L)
        assert(dut.io.mem.cmd.fragment.source.toLong == 1L)

        // Retire that recovery request, then confirm the queued cache fill is
        // issued normally rather than remaining blocked behind it.
        dut.io.mem.cmd.ready #= true
        dut.clockDomain.waitSampling()
        dut.io.readReq.valid #= false
        dut.io.mem.cmd.ready #= false
        dut.clockDomain.waitSampling()

        // The Console AXI/native bridge returns this response with source=0
        // even though the issued direct command used source=1.  Routing must
        // use the internal command-order FIFO and retire the direct-miss lane.
        dut.io.mem.rsp.valid #= true
        dut.io.mem.rsp.fragment.source #= 0
        dut.io.mem.rsp.fragment.data #= 0xabcd1234L
        dut.io.mem.rsp.last #= true
        waitUntil("source-less direct response ready") {
          dut.io.mem.rsp.ready.toBoolean
        }
        dut.clockDomain.waitSampling()
        dut.io.mem.rsp.valid #= false
        dut.clockDomain.waitSampling()
        assert(((dut.io.cacheDebugOccupancy.toLong >> 24) & 0xf) == 0)

        waitUntil("prefetch memory command after wrap recovery") {
          dut.io.mem.cmd.valid.toBoolean
        }
        assert(dut.io.mem.cmd.fragment.address.toLong == 0x100L)
        assert(dut.io.mem.cmd.fragment.source.toLong == 0L)
        dut.io.mem.cmd.ready #= true
        dut.clockDomain.waitSampling()
        dut.io.mem.cmd.ready #= false
    }
  }

  test("scanout decimates prefetched lanes before the consumer FIFO") {
    SimConfig.withIVerilog
      .compile(
        FramebufferPlaneReader(
          config,
          suppressStartupDirectMiss = true,
          laneStride = 4
        )
      )
      .doSim { dut =>
        def waitUntil(label: String, limit: Int = 100)(condition: => Boolean): Unit = {
          var cycles = 0
          while (!condition && cycles < limit) {
            dut.clockDomain.waitSampling()
            cycles += 1
          }
          assert(condition, s"timed out waiting for $label after $limit cycles")
        }

        dut.clockDomain.forkStimulus(10)
        dut.io.prefetchReq.valid #= false
        dut.io.prefetchReq.startAddress #= 0
        dut.io.prefetchReq.endAddress #= 0
        dut.io.readReq.valid #= false
        dut.io.readReq.address #= 0
        dut.io.readRsp.ready #= true
        dut.io.mem.cmd.ready #= false
        dut.io.mem.rsp.valid #= false
        dut.io.mem.rsp.last #= false
        dut.io.mem.rsp.fragment.data #= 0
        dut.io.mem.rsp.fragment.source #= 0
        dut.io.mem.rsp.fragment.context #= 0
        dut.io.mem.rsp.fragment.opcode #= 0
        dut.clockDomain.waitSampling()

        // Sixteen RGB565 lanes span eight 32-bit response beats. With a
        // stride of four, only lanes 0, 4, 8, and 12 should enter the cache.
        dut.io.prefetchReq.valid #= true
        dut.io.prefetchReq.startAddress #= 0x000
        dut.io.prefetchReq.endAddress #= 0x01e
        waitUntil("decimated prefetch acceptance") { dut.io.prefetchReq.ready.toBoolean }
        dut.clockDomain.waitSampling()
        dut.io.prefetchReq.valid #= false

        waitUntil("decimated fill command") { dut.io.mem.cmd.valid.toBoolean }
        assert(dut.io.mem.cmd.fragment.address.toLong == 0)
        assert(dut.io.mem.cmd.fragment.length.toInt == 31)
        dut.io.mem.cmd.ready #= true
        dut.clockDomain.waitSampling()
        dut.io.mem.cmd.ready #= false

        for (word <- 0 until 8) {
          val lo = 0x100 + word * 2
          val hi = lo + 1
          dut.io.mem.rsp.valid #= true
          dut.io.mem.rsp.fragment.data #= (hi.toLong << 16) | lo.toLong
          dut.io.mem.rsp.last #= word == 7
          dut.clockDomain.waitSamplingWhere(dut.io.mem.rsp.ready.toBoolean)
          dut.io.mem.rsp.valid #= false
          dut.clockDomain.waitSampling()
        }

        for ((address, expected) <- Seq(0x000 -> 0x100L, 0x008 -> 0x104L, 0x010 -> 0x108L, 0x018 -> 0x10cL)) {
          dut.io.readReq.valid #= true
          dut.io.readReq.address #= address
          waitUntil(f"decimated read 0x$address%x") { dut.io.readReq.ready.toBoolean }
          dut.clockDomain.waitSampling()
          dut.io.readReq.valid #= false
          waitUntil(f"decimated response 0x$address%x") { dut.io.readRsp.valid.toBoolean }
          assert(dut.io.readRsp.data.toLong == expected)
          dut.clockDomain.waitSampling()
        }

        assert(!dut.skipCachedLane.toBoolean, "decimated reads must not drain skipped lanes")
      }
  }
}
