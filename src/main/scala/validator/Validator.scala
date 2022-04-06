package validator

import chisel3._
import chisel3.util._

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
    val mem = SyncReadMem(depth, width)

    /* Here we set inc value to false and each time same memory 
     * position is returned in read and write pointers.
     **/
    val incrRead = WireInit(false.B)
    val incrWrite = WireInit(false.B)
    val (readPtr, nextRead) = counter(depth, incrRead)
    val (writePtr, nextWrite) = counter(depth, incrWrite)

    /* initialization full flag register */
    val full = RegInit(false.B)

    /* when if enq data is valid and fifo is
     * not full then write data into next pos into the memory
     * and then increment the write pointer
     */
    when (!full) {
      io.enq.ready := true.B
      when (io.enq.valid) {
        printf(p"$writePtr, 0x${Hexadecimal(io.enq.bits.asUInt)}\n")
        mem.write(writePtr, io.enq.bits)
        incrWrite := true.B
        full := (nextWrite === 0.U)
      }
      io.deq.valid := false.B
      io.deq.bits := 0.U
    } otherwise {
      io.enq.ready := false.B
      when (io.deq.ready) {
        val data = mem.read(readPtr)
        printf(p"$readPtr, 0x${Hexadecimal(data.asUInt)}\n")
        incrRead := true.B
        io.deq.valid := true.B
        io.deq.bits := data
      } otherwise {
        io.deq.valid := false.B
        io.deq.bits := 0.U
      }
    }
}