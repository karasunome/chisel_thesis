package validator

import chisel3._
import chisel3.util._

class validator(val w : Int, val depth : Int) extends Module {
  val io = IO(new Bundle {
    val datain = Input(UInt(w.W)) /* input data */
    val dataout = Output(UInt(w.W)) /* output data */
    val wr = Input(Bool()) /* true when fetching data */
    val rd = Input(Bool()) /* true when out data */
    val full = Output(Bool()) /* true when memory full */
    val empty = Output(Bool()) /* true when memory empty */
  })
  val count = RegInit(0.U(depth.W))
  val mem = Mem(depth, UInt(w.W))
  val wp = RegInit(0.U(depth.W))
  val rp = RegInit(0.U(depth.W))
  val dataout = RegInit(0.U(w.W))

  def IndexAdd(index : UInt) : UInt = {
    val temp = RegInit(index)
    when(index === (depth - 1).U) { temp := 0.U }
    .otherwise { temp := index + 1.U }
    temp
  }

  when(io.wr === true.B && io.rd === true.B) {
    println("io.wr === true.B && io.rd === true.B")
    when(count === 0.U) { io.dataout := io.datain }
    .otherwise {
      io.dataout := mem(rp)
      rp := IndexAdd(rp)
      mem(wp) := io.datain
      wp := IndexAdd(wp)
    } 
  } .elsewhen (io.wr === true.B && io.rd === false.B) {
    println("io.wr === true.B && io.rd === false.B")
    io.dataout := 0.U
    when(count < depth.U) {
      mem(wp) := io.datain
      wp := IndexAdd(wp)
      count := count + 1.U
    }
  } .elsewhen (io.wr === false.B && io.rd === true.B) {
    when(count > 0.U) {
      io.dataout := mem(rp)
      rp := IndexAdd(rp)
      count := count - 1.U
    } .otherwise {
      io.dataout := 0.U
    }
  } .otherwise {
    io.dataout := 0.U
  }

  io.full := (depth.U === count)
  io.empty := (count === 0.U)

  printf("count = %d, empty = %d, full = %d\n", count, io.empty, io.full);
}

/*object myFifo {
  def apply(val w : Int, val depth : Int)
}*/