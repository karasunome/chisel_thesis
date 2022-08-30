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
  val enq = Flipped(new DecoupledIO(Vec(width, UInt(8.W))))
  val deq = new DecoupledIO(Vec(width, UInt(8.W)))
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

  def MSB(data: UInt): UInt = {
    if (1.U == ((data >> 7) & 1.U)) {
      return 1.U
    } else {
      return 0.U
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
  val K1 = RegInit(VecInit(Seq.fill(Params.StateLength)(0.U(8.W))))
  val T = RegInit(VecInit(Seq.fill(Params.StateLength)(0.U(8.W))))

  // create memory with depth and width
  val inc = WireInit(false.B)
  val (pos, next_pos) = counter(depth, inc)
  val mem = Mem(depth, Vec(width, UInt(8.W)))

  // initialization full flag register
  val full = RegInit(false.B)
  val state = RegInit(0.U(3.W))
  val result = RegInit(false.B)
  val busy = RegInit(0.U(2.W))

  io.deq.valid := false.B
  io.deq.bits := VecInit(Seq.fill(Params.StateLength)(0.U(8.W)))
  io.enq.ready := false.B

  //printf("state=0x%x\n", state)
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
      //for (i <- 0 until Params.StateLength) {
      //  printf("0x%x ", io.input_key(i))
      //}
      //printf("\n")
    } .otherwise {
      state := 2.U
    }
  } .elsewhen (state === 2.U) { // calculate subkeys state
    aes.io.AES_mode := 0.U
    io.enq.ready := false.B
    when (io.input_key_ready) {
      K1 := io.input_key
      state := 3.U
      //for (i <- 0 until Params.StateLength) {
      //  printf("0x%x ", K1(i))
      //}
      //printf("\n")
    }
  } .elsewhen (state === 3.U) {
    when (io.enq.valid) {
      // write 128 bit data into memory
      io.enq.ready := !busy
      when (busy =/= 3.U) {
        T := io.enq.bits
        busy := busy + 1.U

        when (pos === 0.U) {
          T := aes.io.output_text
        } .elsewhen (pos === depth.U) {
          for (i <- 0 until Params.StateLength) {
            T(i) := aes.io.output_text(i) ^ T(i) ^ K1(i)
          }
        } .otherwise {
          for (i <- 0 until Params.StateLength) {
            T(i) := aes.io.output_text(i) ^ T(i)
          }
        }
        for (i <- 0 until Params.StateLength) {
          printf("0x%x ", aes.io.output_text(i))
        }
        printf("\n")
      } .otherwise {
        aes.io.AES_mode := 2.U
        aes.io.input_text := T
        printf("in: ")
        for (i <- 0 until Params.StateLength) {
          printf("0x%x ", T(i))
        }
        printf("\n")
        when (aes.io.output_valid) {
          aes.io.AES_mode := 0.U
          printf("out: ")
          for (i <- 0 until Params.StateLength) {
            printf("0x%x ", aes.io.output_text(i))
          }
          printf("\n")
          busy := 0.U
        }
      }
      full := (next_pos === 0.U)
    }
  }

  /*printf("T=")
  for (i <- 0 until Params.StateLength) {
    printf("0x%x ", T(i))
  }
  printf("\n")*/
}