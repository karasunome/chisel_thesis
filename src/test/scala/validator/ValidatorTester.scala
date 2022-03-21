package validator

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ValidatorSpec extends AnyFlatSpec with ChiselScalatestTester {
  def testFn[T <: Validator[_ <: Data]](dut: T) = {
    // Default values for all signals
    dut.io.enq.bits.asUInt.poke(1.U)
    dut.io.enq.valid.poke(false.B)
    dut.io.deq.ready.poke(false.B)
    dut.clock.step()

    // Write one value and expect it on the deq side
    dut.io.enq.bits.asUInt.poke(0x123.U)
    dut.io.enq.valid.poke(true.B)
    dut.clock.step()
    dut.io.enq.bits.asUInt.poke(0xab.U)
    dut.io.enq.valid.poke(false.B)
    dut.clock.step(12)
    dut.io.enq.ready.expect(true.B)
    dut.io.deq.valid.expect(true.B)
    dut.io.deq.bits.asUInt.expect(0x123.U)
    // Read it out
    dut.io.deq.ready.poke(true.B)
    dut.clock.step()
    dut.io.deq.valid.expect(false.B)
    dut.io.deq.ready.poke(false.B)
    dut.clock.step()
  }

  "Validator Module" should "pass" in {
    test(new Validator(UInt(16.W), 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      testFn(dut)
    }
  }
}