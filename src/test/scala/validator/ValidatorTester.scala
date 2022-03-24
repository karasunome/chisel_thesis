package validator

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ValidatorSpec extends AnyFlatSpec with ChiselScalatestTester {
  def testFn[T <: Validator[_ <: Data]](dut: T) = {
    // Default values for all signals
    val data = 0

    for (data <- 12345678 until 12345678+1024) {
      dut.io.enq.bits.asUInt.poke(data)
      dut.io.enq.valid.poke(true.B)
      dut.io.deq.ready.poke(true.B)
      dut.clock.step()
    }
  }

  "Validator Module" should "pass" in {
    test(new Validator(UInt(64.W), 1024)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      testFn(dut)
    }
  }
}