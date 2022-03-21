package validator

import chisel3._
import chisel3.util._

/**
  * enqueue and dequeue ports using the ready/valid interface
  * with width.
  */
class ValidatorIO[T <: Data](width: T) extends Bundle {
  val enq = Flipped(new DecoupledIO(width))
  val deq = new DecoupledIO(width)
}

class Validator[T <: Data](width: T, depth: Int) extends Module {
    val io = IO(new ValidatorIO(width))
    class ValidatorIO[T <: Data](width: T) extends Bundle {
      val enq = Flipped(new DecoupledIO(width))
      val deq = new DecoupledIO(width)
    }

    /*
     when counter is firstly called cnt is initialized to 0
     that is the first position of memory. Then Each time this 
     value is incremented. If next position is depth then
     counter is set to 0 again.
     */
    def counter(depth: Int, inc: Bool): (UInt, UInt) = {
        val cntReg = RegInit(0.U(log2Ceil(depth).W))
        val nextVal = Mux(cntReg === (depth-1).U, 0.U, cntReg + 1.U)
        when (inc) {
            cntReg := nextVal
        }
        (cntReg, nextVal)
    }

    /* create memory with depth and width */
    val mem = Mem(depth, width)

    /* Here we set inc value to false and each time same memory 
     * position is returned in read and write pointers.
     **/
    val incrRead = WireInit(false.B)
    val incrWrite = WireInit(false.B)
    val (readPtr, nextRead) = counter(depth, incrRead)
    val (writePtr, nextWrite) = counter(depth, incrWrite)

    /* initialization full flag register */
    val full = RegInit(false.B)

    printf("%d %d %d\n", writePtr, readPtr, full);

    /* when if input data is valid and fifo is
     * not full then write data into next pos in the memory
     * and then increment the position
     */
    when (io.enq.valid && !full) {
      mem.write(writePtr, io.enq.bits)
      full := (nextWrite === readPtr)
      incrWrite := true.B
    }
    printf("%d %d %d\n", writePtr, readPtr, full);

    io.deq.bits :=  291.U
    io.enq.ready := true.B
    io.deq.valid := true.B
}