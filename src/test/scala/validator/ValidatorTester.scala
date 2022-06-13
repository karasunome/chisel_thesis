package validator

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ValidatorSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val depth = 32

  def testFn[T <: Validator[_ <: Data]](dut: T) = {
    // Default values for all signals
    dut.io.enq.bits.asUInt.poke(0x0.U)
    dut.io.enq.valid.poke(false.B)
    dut.io.deq.ready.poke(false.B)

    //wait 12 clk cycle for cmac K1 key
    dut.clock.step(16)
  
    // Fill the whole buffer
    // memory depth available as dut.depth.
    // When the whole memory is full,
    // We expect deq.valid is false
    // until deq.ready is true
    var cnt = 1
    dut.io.enq.valid.poke(true.B)
    for (_ <- 0 until depth) {
      dut.io.enq.bits.asUInt.poke(cnt.U)
      if (dut.io.enq.ready.peek.litToBoolean)
        cnt += 1
      
      //wait 12 clk cycle for aes calculation
      dut.clock.step(16)
    }
    println(s"Wrote ${cnt-1} words")

    dut.io.enq.ready.expect(false.B)
    dut.io.deq.valid.expect(false.B)
    dut.io.deq.bits.asUInt.expect(0.U)
    dut.clock.step()

    println("Now read it back")
    // Now read it back
    var expected = 1
    dut.io.enq.valid.poke(false.B)
    dut.io.deq.ready.poke(true.B)

    for (_ <- 0 until depth) {
      if (dut.io.deq.valid.peek.litToBoolean) {
        //dut.io.deq.bits.asUInt.expect(expected.U)
        expected += 1
      }
      dut.clock.step()
    }
  }

  "Validator Module" should "pass" in {
    test(new Validator(UInt(128.W), depth)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      testFn(dut)
    }
  }
}