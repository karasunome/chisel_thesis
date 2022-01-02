// See README.md for license details.

package validator

import chisel3._
import chisel3.tester._
import org.scalatest.FreeSpec
import chisel3.experimental.BundleLiterals._

class ValidatorSpec extends FreeSpec with ChiselScalatestTester {
  in {
    test(new validator(64, 8)) { c =>
      c.io.wr.poke(true.B)
      c.io.rd.poke(false.B)
      c.io.datain.poke(0x1)
      c.io.datain.poke(0x2)
      c.io.datain.poke(0x3)
      c.io.datain.poke(0x4)
      c.io.datain.poke(0x5)
      c.io.datain.poke(0x6)
      c.io.datain.poke(0x7)
      c.io.datain.poke(0x8)
      
    }
  }
}
