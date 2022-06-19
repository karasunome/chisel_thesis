package validator

import chisel3._
import chisel3.util._

class ValidatorIO(width: Int) extends Bundle {
  val mode = Input(UInt(2.W))
  val input_key = Input(Vec(Params.StateLength, UInt(8.W)))
  val enq = Flipped(new DecoupledIO(UInt(width.W)))
  val deq = new DecoupledIO(UInt(width.W))
}

class Validator(width: Int, depth: Int) extends Module {

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
    if (1.U == ((data >> (width-1)) & 1.U)) {
      return true.B
    } else {
      return false.B
    }
  }

  // aes module parameters 
  val const_Zero = VecInit(Seq.fill(Params.StateLength)(0.U(8.W)))
  val const_Rb = 87.U
  val Nk: Int = 4
  val unrolled: Int = 14
  val SubBytes_SCD: Boolean = false
  val InvSubBytes_SCD: Boolean = false
  val expandedKeyMemType: String = "Mem"

  val aes = AES(Nk, unrolled, SubBytes_SCD, InvSubBytes_SCD, 
                  expandedKeyMemType)
  aes.io.AES_mode := 0.U
  aes.io.input_text := const_Zero

  // cmac subkey registers
  val K1 = RegInit(0.U(width.W))
  val K2 = RegInit(0.U(width.W))
  val T = RegInit(0.U(width.W))

  // create memory with depth and width
  val mem = Mem(depth, UInt(width.W))

  // Here we set inc value to false and each time same memory 
  // position is returned in read and write pointers.
  val incrRead = WireInit(false.B)
  val incrWrite = WireInit(false.B)
  val (readPtr, nextRead) = counter(depth, incrRead)
  val (writePtr, nextWrite) = counter(depth, incrWrite)

  // initialization full flag register
  val full = RegInit(false.B)

  when (io.mode === 0.U) { //idle state
    io.enq.ready := false.B
    aes.io.AES_mode := 0.U
    io.deq.valid := false.B
    io.deq.bits := 0.U
  } .elsewhen (io.mode === 1.U) { //update key state
    // send key to AES memory block
    io.enq.ready := false.B
    aes.io.AES_mode := 1.U
    aes.io.input_text := io.input_key
    io.deq.valid := false.B
    io.deq.bits := 0.U
  } .elsewhen (io.mode === 2.U) { // calculate subkeys
    io.enq.ready := false.B
    io.deq.valid := false.B
    aes.io.AES_mode := 2.U
    // aes input already zero initialized
    // here we want hash of 128 bit 0
    K1 := Mux(MSB(aes.io.output_text.asUInt), ((aes.io.output_text.asUInt << 1) ^ const_Rb), 
                                              (aes.io.output_text.asUInt << 1))
    K2 := Mux(MSB(K1), ((K1 << 1) ^ const_Rb), (K1 << 1))
    io.deq.valid := false.B
    io.deq.bits := 0.U
  } .otherwise {
    // when if enq data is valid and fifo is
    // not full then write data into next pos into the memory
    // and then increment the write pointer otherwise
    // sets enqueue not ready state and give output from dequeue
    when (!full) {
      io.enq.ready := true.B
      aes.io.AES_mode := 2.U
      when (io.enq.valid) {
        // write 128 bit data into memory
        mem.write(writePtr, io.enq.bits)
        // calculate and store aes hash
        if (writePtr == 0.U) {
          T := (io.enq.bits.asUInt)
        } else if (writePtr == (depth-1).U) {
          T := (io.enq.bits.asUInt ^ T ^ K1)
        } else {
          T := (io.enq.bits.asUInt ^ T)
        }
        for (j <- 0 until (Params.StateLength)) {
          aes.io.input_text(j) := (io.enq.bits.asUInt >> (8*(15-j)))
        }
        T := aes.io.output_text.asUInt
        incrWrite := true.B
        full := (nextWrite === 0.U)
      }
    } .otherwise {
      io.enq.ready := false.B
      aes.io.AES_mode := 0.U
      when (io.deq.ready) {
        io.deq.valid := true.B
        io.deq.bits.asUInt := mem.read(nextRead)
        incrRead := true.B
      } otherwise {
        io.deq.valid := false.B
        io.deq.bits.asUInt := 0.U
      }
    }
    io.deq.valid := false.B
    io.deq.bits := 0.U
  }
}