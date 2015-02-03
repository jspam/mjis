package mjis.asm

case class Register(subregs: Map[Int, String])

object AMD64Registers {
  val RSP = -1
  val RAX = -2
  val RBX = -3
  val RDI = -4
  val RSI = -5
  val RDX = -6
  val RCX = -7
  val R8  = -8
  val R9  = -9
  val R10 = -10
  val R11 = -11
  val R12 = -12
  val R13 = -13
  val R14 = -14
  val R15 = -15
  val RBP = -16

  val XMM0 = -100
  val XMM1 = -101
  val XMM2 = -102
  val XMM3 = -103
  val XMM4 = -104
  val XMM5 = -105
  val XMM6 = -106
  val XMM7 = -107
  val XMM8 = -108
  val XMM9 = -109
  val XMM10 = -110
  val XMM11 = -111
  val XMM12 = -112
  val XMM13 = -113
  val XMM14 = -114
  val XMM15 = -115

  val Registers = Map[Int, Register] (
    RSP -> Register(Map(8 -> "rsp", 4 -> "esp", 2 -> "sp", 1 -> "spl")),
    RBP -> Register(Map(8 -> "rbp", 4 -> "ebp", 2 -> "bp", 1 -> "bpl")),
    RAX -> Register(Map(8 -> "rax", 4 -> "eax", 2 -> "ax", 1 -> "al")),
    RBX -> Register(Map(8 -> "rbx", 4 -> "ebx", 2 -> "bx", 1 -> "bl")),
    RDI -> Register(Map(8 -> "rdi", 4 -> "edi", 2 -> "di", 1 -> "dil")),
    RSI -> Register(Map(8 -> "rsi", 4 -> "esi", 2 -> "si", 1 -> "sil")),
    RDX -> Register(Map(8 -> "rdx", 4 -> "edx", 2 -> "dx", 1 -> "dl")),
    RCX -> Register(Map(8 -> "rcx", 4 -> "ecx", 2 -> "cx", 1 -> "cl")),
    R8  -> Register(Map(8 -> "r8", 4 -> "r8d", 2 -> "r8w", 1 -> "r8b")),
    R9  -> Register(Map(8 -> "r9", 4 -> "r9d", 2 -> "r9w", 1 -> "r9b")),
    R10 -> Register(Map(8 -> "r10", 4 -> "r10d", 2 -> "r10w", 1 -> "r10b")),
    R11 -> Register(Map(8 -> "r11", 4 -> "r11d", 2 -> "r11w", 1 -> "r11b")),
    R12 -> Register(Map(8 -> "r12", 4 -> "r12d", 2 -> "r12w", 1 -> "r12b")),
    R13 -> Register(Map(8 -> "r13", 4 -> "r13d", 2 -> "r13w", 1 -> "r13b")),
    R14 -> Register(Map(8 -> "r14", 4 -> "r14d", 2 -> "r14w", 1 -> "r14b")),
    R15 -> Register(Map(8 -> "r15", 4 -> "r15d", 2 -> "r15w", 1 -> "r15b")),

    // movd/movs will select the appropriate instruction suffix,
    // no need to differentiate registers by their name
    XMM0 -> Register(Map(128 -> "xmm0")),
    XMM1 -> Register(Map(128 -> "xmm1")),
    XMM2 -> Register(Map(128 -> "xmm2")),
    XMM3 -> Register(Map(128 -> "xmm3")),
    XMM4 -> Register(Map(128 -> "xmm4")),
    XMM5 -> Register(Map(128 -> "xmm5")),
    XMM6 -> Register(Map(128 -> "xmm6")),
    XMM7 -> Register(Map(128 -> "xmm7")),
    XMM8 -> Register(Map(128 -> "xmm8")),
    XMM9 -> Register(Map(128 -> "xmm9")),
    XMM10 -> Register(Map(128 -> "xmm10")),
    XMM11 -> Register(Map(128 -> "xmm11")),
    XMM12 -> Register(Map(128 -> "xmm12")),
    XMM13 -> Register(Map(128 -> "xmm13")),
    XMM14 -> Register(Map(128 -> "xmm14")),
    XMM15 -> Register(Map(128 -> "xmm15"))
  )

  // Registers to pass parameters in (in order)
  val ParamRegisters = List(RDI, RSI, RDX, RCX, R8, R9)

  val CallerSaveRegisters = Set(RAX, RDI, RSI, RDX, RCX, R8, R9, R10, R11)
  val CalleeSaveRegisters = Set(RBP, RBX, R12, R13, R14, R15)
  val SSERegisters = Set(XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15)
}

