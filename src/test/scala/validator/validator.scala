// See README.md for license details.

package validator

import chisel3._
import chisel3.tester._
import org.scalatest.FreeSpec
import chisel3.experimental.BundleLiterals._

class ValidatorSpec extends FreeSpec with ChiselScalatestTester {
  "Validator module tester application" in {
    test(new validator(8, 8)) { c =>
      c.io.wr.poke(true.B)
      c.io.rd.poke(false.B)
      c.io.datain.poke("h1".U(8.W))

      c.io.wr.poke(true.B)
      c.io.rd.poke(false.B)
      c.io.datain.poke("h2".U(8.W))
      //c.io.full.expect(true.B)
      //c.io.empty.expect(false.B)
    }
  }
}
