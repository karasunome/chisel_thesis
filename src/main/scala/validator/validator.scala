// See README.md for license details.

package validator

import chisel3._

class validator extends Module {
  val io = IO(new Bundle {
    val rdBlk = Input(UInt(64.W))
    val wrBlk = Output(UInt(64.W))
  })
  
  val dataFifo = new myFifo(64, 8)

  when (dataFifo.io.full === true.B) {
    dataFifo.io.wr := false.B
    dataFifo.io.rd := true.B
  }.otherwise {
    dataFifo.io.wr := true.B
    dataFifo.io.rd := false.B
    
    dataFifo.io.datain := rdBlk
    wrBlk := 0.W
  }
}