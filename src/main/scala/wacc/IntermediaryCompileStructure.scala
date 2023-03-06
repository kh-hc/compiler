package wacc

object IntermediaryCompileStructure {
    case class Program(main: List[Instr], functions: List[Function])
    case class Function(id: String, body: List[Instr], args: List[Stored])

    sealed trait IntermediateType
    case object IntType extends IntermediateType
    case object BoolType extends IntermediateType
    case object StringType extends IntermediateType
    case object CharType extends IntermediateType
    case object PointerType extends IntermediateType

    sealed trait Value
    sealed trait BaseValue extends Value
    case class Immediate(value: Int) extends BaseValue
    case class Stored(id: String, tiepe: IntermediateType) extends BaseValue
    case class IntermediateValue(id: Int, tiepe: IntermediateType) extends BaseValue
    
    case class StringLiteral(value: String) extends Value
    case class Access(pointer: Stored, access: BaseValue) extends Value

    sealed trait Instr
    case class BinaryOperation(operator: AssemblyBOperator, src1: Value, src2: Value, dest: Value)
    case class UnaryOperation(operator: AssemblyUOperator, src: Value, dest: Value)
    case class InbuiltFunction(operator: AssemblyIOperator, src: Value) extends Instr
    case class FunctionCall(functionName: String, args: List[Value], returnTo: Value) extends Instr

    case class IfInstruction(condition: Conditional, ifInstructions: List[Instr], elseInstructions: List[Instr]) extends Instr
    case class WhileInstruction(condition: Conditional, body: List[Instr]) extends Instr
    case class ScopeInstruction(body: List[Instr]) extends Instr

    case class Conditional(value: BaseValue, conditions: List[Instr])

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
