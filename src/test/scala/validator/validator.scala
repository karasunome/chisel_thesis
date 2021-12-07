// See README.md for license details.

package validator

import chisel3._
import chisel3.tester._
import org.scalatest.FreeSpec
import chisel3.experimental.BundleLiterals._

class ValidatorSpec extends FreeSpec with ChiselScalatestTester {
  in {
    test(new validator) { c =>
      c.io.rdBlk.poke(1.W)
      c.io.wrBlk.expect(0.W)   
    }
  }
}
