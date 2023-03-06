package wacc

/*
Assembly language abstract structure.
This will be used to help translate the AST into Assembly language.
*/

object assemblyAbstractStructure {
    case class Program(main: List[Instruction], functions: List[Function])
    case class Function(id: String, body: List[Instruction], args: List[Stored])

    sealed trait Instruction
    
    case class BinaryOperation(operator: AssemblyBOperator, src1: Value, src2: Value, dest: Value) extends Instruction
    case class UnaryOperation(operator: AssemblyUOperator, src: Value, dest: Value) extends Instruction
    case class InbuiltFunction(operator: AssemblyIOperator, src: Value) extends Instruction
    case class FunctionCall(functionName: String, args: List[Value], returnTo: Value) extends Instruction

    case class IfInstruction(condition: Conditional, ifInstructions: List[Instruction], elseInstructions: List[Instruction]) extends Instruction
    case class WhileInstruction(condition: Conditional, body: List[Instruction]) extends Instruction
    case class ScopeInstruction(body: List[Instruction]) extends Instruction

    case class Conditional(value: Value, conditions: List[Instruction])

    sealed trait Value
    case class Stored(id: String) extends Value
    case class Immediate(value: Int) extends Value
    case class StringLiteral(value: String) extends Value
    case object Null extends Value

    sealed trait DerefType
    case class PairAccess(pos: PairPos, pair: Value) extends Value with DerefType
    case class ArrayAccess(pos: Value, array: Stored, formation: Boolean) extends Value with DerefType

    sealed trait PairPos
    case object Fst extends PairPos
    case object Snd extends PairPos

    sealed trait AssemblyUOperator
    sealed trait AssemblyBOperator
    sealed trait AssemblyIOperator
     
    case object A_Add extends AssemblyBOperator
    case object A_Sub extends AssemblyBOperator
    case object A_Mul extends AssemblyBOperator
    case object A_Div extends AssemblyBOperator
    case object A_Mod extends AssemblyBOperator
    case object A_And extends AssemblyBOperator
    case object A_Or extends AssemblyBOperator
    case object A_GT extends AssemblyBOperator
    case object A_GTE extends AssemblyBOperator
    case object A_LT extends AssemblyBOperator
    case object A_LTE extends AssemblyBOperator
    case object A_EQ extends AssemblyBOperator
    case object A_NEQ extends AssemblyBOperator
    case object A_Not extends AssemblyUOperator
    case object A_Neg  extends AssemblyUOperator
    case object A_Len extends AssemblyUOperator with AssemblyIOperator
    case object A_Chr extends AssemblyUOperator
    case object A_Ord extends AssemblyUOperator
    case object A_Mov extends AssemblyUOperator
    case object A_PrintI extends AssemblyIOperator
    case object A_PrintC extends AssemblyIOperator
    case object A_PrintB extends AssemblyIOperator
    case object A_PrintS extends AssemblyIOperator
    case object A_PrintA extends AssemblyIOperator
    case object A_PrintCA extends AssemblyIOperator
    case object A_Println extends AssemblyIOperator
    case object A_Assign extends AssemblyUOperator
    case object A_ReadI extends AssemblyIOperator
    case object A_ReadC extends AssemblyIOperator
    case object A_Free extends AssemblyIOperator
    case object A_Return extends AssemblyIOperator
    case object A_Exit extends AssemblyIOperator
    case object A_ArrayCreate extends AssemblyIOperator with AssemblyUOperator// src: Length, dst: Addr
    case object A_PairCreate extends AssemblyIOperator // src: Addr
}
