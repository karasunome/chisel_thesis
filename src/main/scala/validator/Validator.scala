package validator

import chisel3._
import chisel3.util._

// this module is implemented to validate memory blocks
// with aes cmac calculation method
// References:
//   AES CMAC Algorithm: https://datatracker.ietf.org/doc/html/rfc4493
//   AES Module: https://github.com/hplp/aes_chisel

class ValidatorIO(width: Int) extends Bundle {
  val input_key = Input(Vec(Params.StateLength, UInt(8.W)))
  val input_key_ready = Input(Bool())
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
  val Nk: Int = 4 //128bit
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
  val incrRead = WireInit(false.B)
  val incrWrite = WireInit(false.B)
  val (readPtr, nextRead) = counter(depth, incrRead)
  val (writePtr, nextWrite) = counter(depth, incrWrite)

  // initialization full flag register
  val full = RegInit(false.B)
  val state = RegInit(0.U(2.W))
  val validation = RegInit(false.B)

  io.deq.valid := false.B
  io.deq.bits := 0.U

  when (state === 0.U) { //idle state
    io.enq.ready := false.B
    aes.io.AES_mode := 0.U
    when (io.input_key_ready) {
      state := 1.U
    }
  } .elsewhen (state === 1.U) { //update key state
    // this state takes input expanded keys
    // and stores keys into AES module Mem
    io.enq.ready := false.B
    when (io.input_key_ready) {
      aes.io.AES_mode := 1.U
      aes.io.input_text := io.input_key
    } .otherwise {
      state := 2.U
    }
  } .elsewhen (state === 2.U) { // calculate subkeys state    
    io.enq.ready := false.B
    aes.io.AES_mode := 2.U
    when (aes.io.output_valid) {
      io.deq.valid := true.B
      io.deq.bits := K1
      K1 := Mux(MSB(aes.io.output_text.asUInt), ((aes.io.output_text.asUInt << 1) ^ const_Rb), 
                                              (aes.io.output_text.asUInt << 1))
      K2 := Mux(MSB(K1), ((K1 << 1) ^ const_Rb), (K1 << 1))
      state := 3.U
    }
  } .otherwise {   
    when (!full) {
      io.enq.ready := true.B
      when (io.enq.valid) {
        // write 128 bit data into memory
        mem.write(writePtr, io.enq.bits)
        aes.io.AES_mode := 2.U
        for (j <- 0 until (Params.StateLength)) {
          aes.io.input_text(j) := (T >> (8*(15-j)))
        }
        when (aes.io.output_valid) {
          T := aes.io.output_text.asUInt
          incrWrite := true.B
        } .otherwise {
          io.enq.ready := false.B
        }
        full := (nextWrite === 0.U)
      } .otherwise {
        aes.io.AES_mode := 0.U
      }
      io.deq.valid := false.B
      io.deq.bits := 0.U
    } .otherwise {
      io.enq.ready := false.B
      aes.io.AES_mode := 0.U
      when (io.input_key_ready) {
        validation := (io.input_key.asUInt === T)
      } .otherwise {
        when (io.deq.ready & validation) {
          // Here input hash value and T are calculated
          // If the values are equal then gives memory
          // to dequeue, otherwise dequeue drives invalid
          //  TODO:
          io.deq.valid := true.B
          io.deq.bits.asUInt := mem.read(nextRead)
          incrRead := true.B
        } otherwise {
          io.deq.valid := false.B
          io.deq.bits.asUInt := 0.U
        }
      }
    }
  }
  printf("state=0x%x\n", state)
  printf("aes.io.output_text.asUInt=0x%x\n", aes.io.output_text.asUInt)
  printf("K1=0x%x\n", K1)
}