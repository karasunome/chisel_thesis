package validator

import chisel3._
import chisel3.util._

class ValidatorIO[T <: Data](width: Int) extends Bundle {
  val mode = Input(UInt(2.W))
  val input_key = Input(Vec(Params.StateLength, UInt(8.W)))
  val enq = Flipped(new DecoupledIO(UInt(width.W)))
  val deq = new DecoupledIO(UInt(width.W))
}

class Validator[T <: Data](width: Int, depth: Int) extends Module {

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
    if (1.U == (data >> (width-1))) {
      return true.B
    } else {
      return false.B
    }
  }

  io.deq.valid := false.B
  io.deq.bits := 0.U

  // aes module parameters
  val const_Zero = VecInit(Seq.fill(Params.StateLength)(0.U(8.W)))
  val const_Rb = 87.U
  val Nk: Int = 4
  val unrolled: Int = 0
  val SubBytes_SCD: Boolean = false
  val InvSubBytes_SCD: Boolean = false
  val expandedKeyMemType: String = "Mem"

  val Nr: Int = Nk + 6 // 10, 12, 14 rounds
  val Nrplus1: Int = 1// Nr + 1 // 10+1, 12+1, 14+1

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
  val key_valid = RegInit(false.B)

  when (io.mode === 0.U) { //idle state
    io.enq.ready := false.B
    io.deq.valid := false.B
    io.deq.bits := 0.U
  } .elsewhen (io.mode === 1.U) { //update key state
    // send expanded key to AES memory block
    io.enq.ready := false.B
    aes.io.AES_mode := 1.U
    aes.io.input_text := io.input_key
    printf("0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x\n",
            aes.io.input_text(0), aes.io.input_text(1), aes.io.input_text(2),
            aes.io.input_text(3), aes.io.input_text(4), aes.io.input_text(5),
            aes.io.input_text(6), aes.io.input_text(7), aes.io.input_text(8),
            aes.io.input_text(9), aes.io.input_text(10), aes.io.input_text(11),
            aes.io.input_text(12), aes.io.input_text(13), aes.io.input_text(14),
            aes.io.input_text(15))
  } .otherwise {
    aes.io.AES_mode := 2.U
    when(!key_valid) {
      io.enq.ready := false.B
      // calculate subkeys
      aes.io.input_text := const_Zero
      K1 := Mux(MSB(aes.io.output_text.asUInt), ((aes.io.output_text.asUInt << 1) ^ const_Rb), 
                                                (aes.io.output_text.asUInt << 1))
      K2 := Mux(MSB(K1), ((K1 << 1) ^ const_Rb), (K1 << 1))
      printf("K1 = 0x%x\n", K1)
      printf("K2 = 0x%x\n", K2)
      //key_valid := true.B
      aes.io.AES_mode := 0.U
    } otherwise {
      io.enq.ready := true.B
      // when if enq data is valid and fifo is
      // not full then write data into next pos into the memory
      // and then increment the write pointer otherwise
      // sets enqueue not ready state and give output from dequeue
      when (!full) {
        io.enq.ready := true.B
        when (io.enq.valid) {
          // write 128 bit data into memory
          mem.write(writePtr, io.enq.bits)
          printf("data = 0x%x\n", io.enq.bits.asUInt)
          // calculate and store aes hash
          /*if (writePtr == 0.U) {
            T := (io.enq.bits.asUInt)
          } else if (writePtr == (depth-1).U) {
            T := (io.enq.bits.asUInt ^ T ^ K1)
          } else {
            T := (io.enq.bits.asUInt ^ T)
          }*/
          //printf("T = 0x%x\n", io.enq.bits.asUInt)
          for (j <- 0 until (Params.StateLength)) {
            aes.io.input_text(j) := (io.enq.bits.asUInt >> (8*(15-j)))
          }
          printf("hash = 0x%x\n", aes.io.output_text.asUInt)
          T := aes.io.output_text.asUInt
          incrWrite := true.B
          full := (nextWrite === 0.U)
        }
      } .otherwise {
        //aes.io.AES_mode := 0.U
        printf("final hash = 0x%x\n", T)
        io.enq.ready := false.B
        when (io.deq.ready) {
          io.deq.valid := true.B
          io.deq.bits.asUInt := 0.U
        } otherwise {
          io.deq.valid := false.B
          io.deq.bits.asUInt := 0.U
        }
      }
    }
  }
}