package validator

import chisel3._
import chisel3.util._

class ValidatorIO[T <: Data](width: T) extends Bundle {
  val enq = Flipped(new DecoupledIO(width))
  val deq = new DecoupledIO(width)
}

class Validator[T <: Data](width: T, depth: Int) extends Module {

  val io = IO(new ValidatorIO(width))

  // when counter is firstly called cnt is initialized to 0
  // that is the first position of memory. Then Each time this 
  // value is incremented. If next position is depth then
  // counter is set to 0 again.
  def counter(depth: Int, inc: Bool): (UInt, UInt) = {
      val cntReg = RegInit(0.U(log2Ceil(depth).W))
      val nextVal = Mux(cntReg === (depth-1).U, 0.U, cntReg + 1.U)
      when (inc) {
          cntReg := nextVal
      }
      (cntReg, nextVal)
  }

  // AES module parameters
  val Nk: Int = 4
  val unrolled: Int = 0
  val SubBytes_SCD: Boolean = false
  val InvSubBytes_SCD: Boolean = false
  val expandedKeyMemType: String = "Mem"

  val aes = AES(Nk, unrolled, SubBytes_SCD, InvSubBytes_SCD, 
                  expandedKeyMemType)

  // create memory with depth and width
  val mem = Mem(depth, width)

  // Here we set inc value to false and each time same memory 
  // position is returned in read and write pointers.
  val incrRead = WireInit(false.B)
  val incrWrite = WireInit(false.B)
  val (readPtr, nextRead) = counter(depth, incrRead)
  val (writePtr, nextWrite) = counter(depth, incrWrite)

  // initialization full flag register
  val full = RegInit(false.B)

  // when if enq data is valid and fifo is
  // not full then write data into next pos into the memory
  // and then increment the write pointer otherwise
  // sets enqueue not ready state and give output from dequeue
  when (!full) {
    aes.io.AES_mode := 0.U // aes off when fifo is not full
    io.enq.ready := true.B
    when (io.enq.valid) {
      mem.write(writePtr, io.enq.bits)
      incrWrite := true.B
      full := (nextWrite === 0.U)
    }
    io.deq.valid := false.B
    io.deq.bits := 0.U
  } otherwise {
    aes.io.AES_mode := 2.U
    io.enq.ready := false.B

    val data = mem.read(readPtr)
    incrRead := true.B
    for (i <- 0 until Params.StateLength)
    {
      printf(p"$readPtr, 0x${Hexadecimal(data.asUInt)}\n")
    }
    when (io.deq.ready) {
      io.deq.valid := true.B
      //io.deq.bits := data
    } otherwise {
      io.deq.valid := false.B
      //io.deq.bits := 0.U
    }
  }
}