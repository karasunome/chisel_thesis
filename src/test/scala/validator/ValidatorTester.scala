package validator

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ValidatorSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val depth = 4  //32*width
  private val width = 16 //byte
  
  def testFn[T <: Validator](dut: T) = {
    
    println("Drive default values for all signals")
    dut.io.input_key_ready.poke(false.B)
    for (j <- 0 until Params.StateLength) {
      dut.io.input_key(j).poke(0)
    }
    dut.io.enq.valid.poke(false.B)
    dut.io.deq.ready.poke(false.B)
    dut.clock.step(1)

    println("Send expanded key values to aes module")
    dut.io.input_key_ready.poke(true.B)
    dut.clock.step(1)
    for (i <- 0 until Params.Nrplus1) {
      for (j <- 0 until Params.StateLength) {
        dut.io.input_key(j).poke(Params.expandedKey(i)(j))
      }
      dut.clock.step(1)
    }
    dut.clock.step(1)
    dut.io.input_key_ready.poke(false.B)
    dut.clock.step(1)
    
    println("Send subkey for CMAC")
    for (i <- 0 until Params.StateLength) {
      dut.io.input_key(i).poke(Params.K1_key(i))
    }
    dut.clock.step(2)
    dut.io.input_key_ready.poke(true.B)
    dut.clock.step(1)
  
    println(s"Subkeys sent\n")
    dut.io.enq.valid.poke(true.B)

    var i = 0
    while (i < InputElf.size) {
      if (dut.io.enq.ready.peek.litToBoolean) {
        for (j <- 0 until Params.StateLength) {
          dut.io.enq.bits(j).poke(InputElf.bytes(i)(j))
        }
        i = i + 1
      }
      dut.clock.step(1)
    }

  }

  "Validator Module" should "pass" in {
    test(new Validator(width, depth)) { dut =>
      testFn(dut)
    }
  }
}