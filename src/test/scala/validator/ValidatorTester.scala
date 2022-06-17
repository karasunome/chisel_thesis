package validator

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ValidatorSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val depth = 4  //32*width
  private val width = 128 //bit
  private val key = Array(0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f)

  def testFn[T <: Validator[_ <: Data]](dut: T) = {
    // Default values for all signals
    dut.io.mode.poke(0.U)
    dut.io.enq.valid.poke(false.B)
    dut.io.deq.ready.poke(false.B)
    for (i <- 0 until Params.StateLength)
    {
      dut.io.input_key(i).poke(key(i))
    }

    //wait 8 cycle in idle state
    dut.clock.step(4)

    dut.io.mode.poke(1.U)
    dut.io.enq.ready.expect(false.B)
    dut.io.enq.valid.poke(false.B)

    dut.io.deq.ready.poke(false.B)
    dut.io.deq.valid.expect(false.B)

    //wait 12 clk cycle for cmac subkeys
    dut.clock.step(1)

    // wait for subkeys
    var cnt = 1
    dut.io.enq.valid.poke(true.B)

    dut.io.mode.poke(2.U)
    dut.io.enq.ready.expect(false.B)
    dut.io.deq.valid.expect(false.B)

    //wait 12 clk cycle for aes calculation
    dut.clock.step(4)

    println(s"Subkeys generated\n")
  
    // Fill the whole buffer
    // memory depth available as dut.depth.
    // When the whole memory is full,
    // We expect deq.valid is false
    // until deq.ready is true
    //var cnt = 1
    //dut.io.enq.valid.poke(true.B)
    //for (_ <- 0 until depth) {
    //  dut.io.enq.ready.expect(true.B)
    //  dut.io.enq.bits.asUInt.poke(cnt.U)
    //  cnt += 1
    //  //wait 12 clk cycle for aes calculation
    //  dut.clock.step(4)
    //}
    //println(s"Wrote ${cnt-1} words")
//
    //dut.io.enq.ready.expect(false.B)
    //dut.io.deq.valid.expect(false.B)
    //dut.io.deq.bits.asUInt.expect(0.U)
    //dut.clock.step(16)
//
    //println("Now read it back")
    //// Now read it back
    //var expected = 1
    //dut.io.enq.valid.poke(false.B)
    //dut.io.deq.ready.poke(true.B)
//
    //for (_ <- 0 until depth) {
    //  dut.io.deq.valid.expect(true.B)
    //  dut.io.deq.bits.asUInt.expect(expected.U)
    //  expected += 1
    //  dut.clock.step(16)
    //}
  }

  "Validator Module" should "pass" in {
    test(new Validator(width, depth)) { dut =>
      testFn(dut)
    }
  }
}