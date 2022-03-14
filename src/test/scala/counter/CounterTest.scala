/*
 * A simple counter example with configurable bit width and with a test bench.
 * 
 */

package counter

import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/**
 * Test the counter by printing out the value at each clock cycle.
 */

class CounterTester extends AnyFlatSpec with ChiselScalatestTester {

  "CounterTest test" should "pass" in {
    test(new Counter(32,2)) { dut =>
      dut.io.enable.poke(true)
      for (i <- 0 until 5) {
        println(i.toString + ": " + dut.io.dataOut.peek.toString())
        dut.clock.step(1)
      }
      dut.io.enable.poke(false)
      for (i <- 0 until 5) {
        println(i.toString + ": " + dut.io.dataOut.peek.toString())
        dut.clock.step(1)
      }
      dut.io.enable.poke(false)
      dut.io.dataIn.poke(32)
      for (i <- 0 until 5) {
        println(i.toString + ": " + dut.io.dataOut.peek.toString())
        dut.clock.step(1)
      }
      dut.io.enable.poke(true)
      for (i <- 0 until 5) {
        println(i.toString + ": " + dut.io.dataOut.peek.toString())
        dut.clock.step(1)
      }
    }
  }
}