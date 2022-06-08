package validator

import chisel3._
import chisel3.util._

class ValidatorIO[T <: Data](width: UInt) extends Bundle {
  val enq = Flipped(new DecoupledIO(width))
  val deq = new DecoupledIO(width)
}

class Validator[T <: Data](width: UInt, depth: Int) extends Module {

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
  val aes_mode = RegInit(0.U(2.W))
  val input_vector = VecInit(Seq.fill(Params.StateLength)(0.U(8.W)))

  aes.io.AES_mode := aes_mode
  aes.io.input_text := input_vector

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
    io.enq.ready := false.B

    val data = mem.read(readPtr)
    incrRead := true.B
    val data2 = mem.read(readPtr)
    incrRead := true.B

    for (i <- 0 until (Params.StateLength >> 2)) {
      aes.io.input_text(i) := (data.asUInt << (8*i))
      aes.io.input_text(i+8) := (data2.asUInt << (8*i))
    }

    /*printf("%x %x %x %x %x %x %x %x %x %x %x %x %x %x %x %x\n",
        aes.io.input_text(0), aes.io.input_text(1), 
        aes.io.input_text(2), aes.io.input_text(3), 
        aes.io.input_text(4), aes.io.input_text(5), 
        aes.io.input_text(6), aes.io.input_text(7), 
        aes.io.input_text(8), aes.io.input_text(9), 
        aes.io.input_text(10), aes.io.input_text(11), 
        aes.io.input_text(12), aes.io.input_text(13), 
        aes.io.input_text(14), aes.io.input_text(15))*/
    when (io.deq.ready) {
      io.deq.valid := true.B
      io.deq.bits := data
    } otherwise {
      io.deq.valid := false.B
      io.deq.bits := 0.U
    }
  }
}