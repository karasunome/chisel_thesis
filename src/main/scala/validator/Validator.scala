package validator

import chisel3._
import chisel3.util._

/**
  * FIFO IO with enqueue and dequeue ports using the ready/valid interface.
  */
class ValidatorIO[T <: Data](data: T) extends Bundle {
  val enq = Flipped(new DecoupledIO(data))
  val deq = new DecoupledIO(data)
}

class Validator[T <: Data](data: T, depth: Int) extends Module {
    val io = IO(new ValidatorIO(data))

    def counter(depth: Int, incr: Bool): (UInt, UInt) = {
        val cntReg = RegInit(0.U(log2Ceil(depth).W))
        val nextVal = Mux(cntReg === (depth-1).U, 0.U, cntReg + 1.U)
        when (incr) {
            cntReg := nextVal
        }
        (cntReg, nextVal)
    }
    
    val mem = Mem(depth, data)

    val enq_ptr = RegInit(0.U)
    val deq_ptr = RegInit(0.U)

    val is_full = RegInit(false.B)
    val is_empty = !is_full && (enq_ptr === deq_ptr)

    when (io.enq.valid && !is_full) {
        mem.write(enq_ptr, io.enq.bits)
        when (enq_ptr)
    }

}