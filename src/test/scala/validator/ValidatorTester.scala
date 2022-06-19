package validator

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ValidatorSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val depth = 4  //32*width
  private val width = 128 //bit
  
  def testFn[T <: Validator](dut: T) = {
    println("Drive default values for all signals")
    dut.io.mode.poke(0.U)
    dut.io.enq.valid.poke(false.B)
    dut.io.deq.ready.poke(false.B)
    dut.clock.step(4)

    println("Send expanded key values to aes module")
    dut.io.mode.poke(1.U)
    for (i <- 0 until Params.Nrplus1) {
      for (j <- 0 until Params.StateLength) {
        dut.io.input_key(j).poke(Params.expandedKey(i)(j))
      }
      dut.clock.step(1)
    }
    dut.io.mode.poke(0.U)
    dut.clock.step(4)
    
    println("Calculate subkeys for CMAC")
    dut.io.mode.poke(2.U)
    dut.clock.step(1)

    dut.io.enq.valid.poke(false.B)
    for (i <- 1 until Params.Nrplus1) {
      dut.clock.step(1)
    }
    println(s"Subkeys generated\n")

    dut.io.mode.poke(3.U)
    dut.clock.step(4)
  
    //Fill the whole buffer
    //memory depth available as dut.depth.
    //When the whole memory is full,
    //We expect deq.valid is false
    //until deq.ready is true
    var cnt = 1
    dut.io.enq.valid.poke(true.B)
    for (_ <- 0 until depth) {
     dut.io.enq.bits.asUInt.poke(cnt.U)
     cnt += 1
     //wait 12 clk cycle for aes calculation
     dut.clock.step(4)
    }
    println(s"Wrote ${cnt-1} words")

    dut.io.enq.ready.expect(false.B)
    dut.clock.step(16)

    println("Now read it back")
    // Now read it back
    var expected = 1
    dut.io.deq.ready.poke(true.B)
    dut.io.enq.valid.poke(false.B)
    dut.clock.step(1)

    for (_ <- 0 until depth) {
     //Wdut.io.deq.bits.asUInt.expect(expected.U)
     expected += 1
     dut.clock.step(1)
    }
  }

  "Validator Module" should "pass" in {
    test(new Validator(width, depth)) { dut =>
      testFn(dut)
    }
  }
}