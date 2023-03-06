package wacc

object AssemblyInstructions {
    sealed trait Operand

    sealed trait Register extends Operand
    sealed trait GeneralRegister extends Register
    sealed trait ReservedRegister extends Register
    sealed trait VolatileRegister extends Register

    case object Return extends VolatileRegister
    case object R1 extends VolatileRegister
    case object R2 extends VolatileRegister
    case object R3 extends VolatileRegister
    case object R4 extends GeneralRegister
    case object R5 extends GeneralRegister
    case object R6 extends GeneralRegister
    case object R7 extends GeneralRegister
    case object R8 extends GeneralRegister
    case object R9 extends GeneralRegister
    case object R10 extends GeneralRegister
    case object FP extends ReservedRegister // frame pointer
    case object IPC extends ReservedRegister // intra procedural call
    case object SP extends ReservedRegister // stack pointer
    case object LR extends ReservedRegister // link register
    case object PC extends ReservedRegister // AssProg counter

    val generalRegisters = Set(R4, R5, R6, R7, R8, R9, R10)
    val VolatileRegisters = List(Return, R1, R2, R3)

    case class Imm(x: Integer) extends Operand
    case class Label(label: String) extends Operand

    sealed trait Opcode
    case object Add extends Opcode
    case object Mov extends Opcode
    case object Ldr extends Opcode
    case object Str extends Opcode
    case object Push extends Opcode
    case object Pop extends Opcode
    case object Sub extends Opcode
    case object Cmp extends Opcode
    case object Mul extends Opcode
    case object Smull extends Opcode
    case object And extends Opcode
    case object Or extends Opcode
    case object RightSub extends Opcode
    case object Branch extends Opcode

    sealed trait Condition
    case object EQ extends Condition with Opcode
    case object NE extends Condition with Opcode
    case object GE extends Condition with Opcode
    case object LT extends Condition with Opcode
    case object GT extends Condition with Opcode
    case object LE extends Condition with Opcode
    case object VS extends Condition with Opcode

    sealed trait InBuilt
    case object PrintI extends InBuilt
    case object PrintB extends InBuilt
    case object PrintC extends InBuilt
    case object PrintS extends InBuilt
    case object PrintA extends InBuilt
    case object PrintCA extends InBuilt
    case object PrintLn extends InBuilt
    case object ReadI extends InBuilt
    case object ReadC extends InBuilt
    case object DivMod extends InBuilt
    case object DivZero extends InBuilt
    case object Exit extends InBuilt
    case object Free extends InBuilt
    case object Overflow extends InBuilt
    case object NullError extends InBuilt
    case object Malloc extends InBuilt
    case object OutOfBound extends InBuilt
    
    sealed trait Instruction
    // General convention being dest src src1 ...
    case class Instr(op: Opcode, cond: Option[Condition], args: List[Operand]) extends Instruction
    case class Call(func: InBuilt, args: List[Operand])
    case class Branch(cond: Option[Condition], label: String) extends Instruction
    case class NewLabel(label: String) extends Instruction

    case class Program(instr: List[Instruction])
    case class Block(label: Label, instrs: List[Instruction])
}
