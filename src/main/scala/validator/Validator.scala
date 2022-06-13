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

  def MSB(data: UInt): Bool = {
    if (1.U == data >> 127) {
      return true.B
    } else {
      return false.B
    }
  }

  io.deq.valid := false.B
  io.deq.bits := 0.U

  // AES module parameters
  val const_Rb = 87.U(128.W)
  val Nk: Int = 4
  val unrolled: Int = 0
  val SubBytes_SCD: Boolean = false
  val InvSubBytes_SCD: Boolean = false
  val expandedKeyMemType: String = "Mem"

  val KeyLength: Int = Nk * Params.rows
  val Nr: Int = Nk + 6 // 10, 12, 14 rounds
  // !! only one key we will send
  val Nrplus1: Int = Nr + 1 // 10+1, 12+1, 14+1

  val L = RegInit(VecInit(Seq.fill(Params.StateLength)(0.U(8.W))))
  val K1 = RegInit(0.U(128.W))
  val M_i = RegInit(0.U(128.W))
  val M_i_aes = RegInit(0.U(128.W))
  val counter = RegInit(0.U)

  val aes = AES(Nk, unrolled, SubBytes_SCD, InvSubBytes_SCD, 
                  expandedKeyMemType)

  // create memory with depth and width
  val mem = Mem(depth, width)

  // create memory for hashes
  val hashMem = Mem(depth, width)

  // Here we set inc value to false and each time same memory 
  // position is returned in read and write pointers.
  val incrRead = WireInit(false.B)
  val incrWrite = WireInit(false.B)
  val (readPtr, nextRead) = counter(depth, incrRead)
  val (writePtr, nextWrite) = counter(depth, incrWrite)

  // initialization full flag register
  val full = RegInit(false.B)
  val key_valid = RegInit(false.B)
  val aes_mode = RegInit(0.U(2.W))

  aes.io.AES_mode := aes_mode
  aes.io.input_text := L

  when (!key_valid) {
    // send expanded key to AES memory block
    aes_mode := 1.U(2.W) // configure key
    for (i <- 0 until Nrplus1) {
      for (j <- 0 until Params.StateLength) {
        aes.io.input_text(j) := Params.expandedKey(i)(j).asUInt
      }
    }
    aes_mode := 2.U(2.W)
    aes.io.input_text := L
    val L_hash = (aes.io.output_text.asUInt() << 1)
    K1 := Mux(MSB(L_hash), (L_hash ^ const_Rb), (L_hash))
    //printf("L_hash = 0x%x\n", L_hash)
    //printf("K1 = 0x%x\n", K1)
    // here out block size already
    // multiple of 128 bit so we dont
    // need K2 calculation
    aes_mode := 0.U(2.W)
    key_valid := true.B
    printf("K1 = 0x%x\n", K1)
  }

  // when if enq data is valid and fifo is
  // not full then write data into next pos into the memory
  // and then increment the write pointer otherwise
  // sets enqueue not ready state and give output from dequeue
  when (!full) {
    aes_mode := 2.U(2.W)
    io.enq.ready := true.B
    when (io.enq.valid) {
      // write 128 bit data into memory
      mem.write(writePtr, io.enq.bits)
      printf("data = 0x%x\n", io.enq.bits.asUInt)
      for (j <- 0 until (Params.StateLength)) {
        aes.io.input_text(j) := (io.enq.bits.asUInt >> (8.U*(15.U-j.asUInt)))
      }
      // calculate and store aes hash
      hashMem.write(writePtr, aes.io.output_text.asUInt)
      printf("hash = 0x%x\n", aes.io.output_text.asUInt)
      incrWrite := true.B
      full := (nextWrite === 0.U)
    }
  } .otherwise {
    io.enq.ready := false.B
    when (io.deq.ready) {
      io.deq.valid := true.B
    } otherwise {
      io.deq.valid := false.B
    }
  }
}