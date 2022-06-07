package validator

import chisel3._
import chisel3.util._

class ValidatorIO[T <: Data](width: T) extends Bundle {
  val enq = Flipped(new DecoupledIO(width))
  val deq = new DecoupledIO(width)
}

class Validator[T <: Data](width: T, depth: Int) extends Module {
  
  val io = IO(new ValidatorIO(width))

  val expandedKeyMemType = "Mem" // ROM or Mem or SyncReadMem works
  val SubBytes_SCD = false
  val InvSubBytes_SCD = false
  val Nk = 4 // 4, 6, 8 [32-bit words] columns in cipher key
  val unrolled = 14
  val KeyLength: Int = Nk * Params.rows
  val Nr: Int = Nk + 6 // 10, 12, 14 rounds
  val Nrplus1: Int = Nr + 1 // 10+1, 12+1, 14+1
  val EKDepth: Int = 16 // enough memory for any expanded key

  // Instantiate module objects
  val CipherModule = Cipher(Nk, SubBytes_SCD)
  val InvCipherModule = InvCipher(Nk, InvSubBytes_SCD)

  //when counter is firstly called cnt is initialized to 0
  //that is the first position of memory. Then Each time this 
  //value is incremented. If next position is depth then
  //counter is set to 0 again.
  def counter(depth: Int, inc: Bool): (UInt, UInt) = {
      val cntReg = RegInit(0.U(log2Ceil(depth).W))
      val nextVal = Mux(cntReg === (depth-1).U, 0.U, cntReg + 1.U)
      when (inc) {
          cntReg := nextVal
      }
      (cntReg, nextVal)
  }

  // create memory with depth and width
  val mem = Mem(depth, width)

  val aesMem = Mem(EKDepth, Vec(Params.StateLength, UInt(8.W)))

  // Here we set inc value to false and each time same memory 
  // position is returned in read and write pointers.
  val incrRead = WireInit(false.B)
  val incrWrite = WireInit(false.B)
  val (readPtr, nextRead) = counter(depth, incrRead)
  val (writePtr, nextWrite) = counter(depth, incrWrite)

  // initialization full flag register
  val full = RegInit(false.B)

  val dataOut = Wire(Vec(Params.StateLength, UInt(8.W)))

  // when if enq data is valid and fifo is
  // not full then write data into next pos into the memory
  // and then increment the write pointer otherwise
  // sets enqueue not ready state and give output from dequeue
  when (!full) {
    io.enq.ready := true.B
    when (io.enq.valid) {
      //printf(p"$writePtr, 0x${Hexadecimal(io.enq.bits.asUInt)}\n")
      mem.write(writePtr, io.enq.bits)
      incrWrite := true.B
      full := (nextWrite === 0.U)
    }
    io.deq.valid := false.B
    io.deq.bits := 0.U
  } otherwise {
    io.enq.ready := false.B
    dataOut := mem.read(readPtr)
    incrRead := true.B

    //when (io.deq.ready) {
    //  val data = mem.read(readPtr)
    //  //printf(p"$readPtr, 0x${Hexadecimal(data.asUInt)}\n")
    //  incrRead := true.B
    //  io.deq.valid := true.B
    //  io.deq.bits := data
    //} otherwise {
    //  io.deq.valid := false.B
    //  io.deq.bits := 0.U
    //}
  }
}