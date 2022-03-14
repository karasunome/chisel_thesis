package counter

import chisel3._

class Counter(size: Int, inc: Int) extends Module {
    val io = IO(new Bundle {
        val enable = Input(Bool())
        val dataIn = Input(UInt(size.W))
        val dataOut = Output(UInt(size.W))
    })
    
    val reg = Reg(UInt(size.W))
    when (io.enable) {
        reg := reg + inc.U
    } .otherwise {
        reg := io.dataIn
    }
    io.dataOut := reg
}