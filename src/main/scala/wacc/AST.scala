package wacc

object abstractSyntaxTree {
    import SymbolTypes._
    import parsley.genericbridges._
    import parsley.Parsley
    import parsley.position._
    import parsley.implicits.zipped._

    sealed trait ASTNode {
        var symbolTable: Option[SymbolTable] = None
        var tiepe: Option[SymbolType] = None
    }

    case class WACCprogram(funcs: List[FunctionUnit], stat: StatementUnit)(val pos: (Int, Int))  extends ASTNode
    
    case class FunctionUnit(t: Type, id: Identifier, params: ParamList, body: StatementUnit)(val pos: (Int, Int)) extends ASTNode

    case class ParamList(paramlist: List[Param])(val pos: (Int, Int)) extends ASTNode

    case class Param(t: Type, id: Identifier)(val pos: (Int, Int)) extends ASTNode

    sealed trait StatementUnit extends ASTNode {
        val pos: (Int, Int)
    } 
    case object SkipStat extends StatementUnit with ParserBridgePos0[StatementUnit] {
      override val pos: (Int, Int) = (0,0) // cannot get position here so return incorrect pos
    }

    case class AssignStat(t: Type, id: Identifier, value: Rvalue)(val pos: (Int, Int)) extends StatementUnit
    case class ReassignStat(left: Lvalue, right: Rvalue)(val pos: (Int, Int)) extends StatementUnit
    case class ReadStat(value: Lvalue)(val pos: (Int, Int)) extends StatementUnit
    case class FreeStat(expr: Expr)(val pos: (Int, Int)) extends StatementUnit
    case class ReturnStat(expr: Expr)(val pos: (Int, Int)) extends StatementUnit
    case class ExitStat(expr: Expr)(val pos: (Int, Int)) extends StatementUnit
    case class PrintStat(expr: Expr)(val pos: (Int, Int)) extends StatementUnit
    case class PrintlnStat(expr: Expr)(val pos: (Int, Int)) extends StatementUnit
    case class IfStat(cond: Expr, ifStat: StatementUnit, elseStat: StatementUnit)(val pos: (Int, Int)) extends StatementUnit
    case class WhileStat(cond: Expr, body: StatementUnit)(val pos: (Int, Int)) extends StatementUnit
    case class ScopeStat(body: StatementUnit)(val pos: (Int, Int)) extends StatementUnit
    case class SeqStat(statements: List[StatementUnit])(val pos: (Int, Int)) extends StatementUnit

    sealed trait Lvalue extends ASTNode {
        val pos: (Int, Int)
    }

    sealed trait PairElem extends Rvalue with Lvalue

    case class PairElemFst(pair: Lvalue)(val pos: (Int, Int)) extends PairElem
    case class PairElemSnd(pair: Lvalue)(val pos: (Int, Int)) extends PairElem

    sealed trait Rvalue extends ASTNode{
        val pos: (Int, Int)
    }
    sealed trait Expr extends Rvalue
    case class ArrayLiteral(value: List[Expr])(val pos: (Int, Int)) extends Rvalue
    case class NewPair(exprLeft: Expr, exprRight: Expr)(val pos: (Int, Int)) extends Rvalue
    case class Call(id: Identifier, args: ArgList)(val pos: (Int, Int)) extends Rvalue

    case class ArgList(args: List[Expr])(val pos: (Int, Int))

    sealed trait Type extends ASTNode
    
    sealed trait BaseType extends Type
    case object IntT extends BaseType with ParserBridgePos0[Type]
    case object BoolT extends BaseType with ParserBridgePos0[Type]
    case object CharT extends BaseType with ParserBridgePos0[Type]
    case object StringT extends BaseType with ParserBridgePos0[Type]
    
    case class ArrayType(t: Type)(val pos: (Int, Int)) extends Type

    case class PairType(left: PairElemType, right: PairElemType)(val pos: (Int, Int)) extends Type
    sealed trait PairElemType extends ASTNode
    case class PairElemTypeT(t: Type)(val pos: (Int, Int)) extends PairElemType
    case object NestedPair extends PairElemType with ParserBridgePos0[PairElemType]

    case class IntExpr(value: Int)(val pos: (Int, Int)) extends Expr0
    case class BoolExpr(value: Boolean)(val pos: (Int, Int)) extends Expr0
    case class CharExpr(value: Char)(val pos: (Int, Int)) extends Expr0
    case class StrExpr(value: String)(val pos: (Int, Int)) extends Expr0
    case object PairLiteral extends Expr0 with ParserBridge0[Expr0] {
        override val pos: (Int, Int) = (0,0) // cannot get position
    }
    case class ArrayElem(id: Identifier, position: List[Expr])(val pos: (Int, Int)) extends Expr0 with Lvalue
    case class ParenExpr(expr: Expr)(val pos: (Int, Int)) extends Expr0
    case class Identifier(id: String)(val pos: (Int, Int)) extends Expr0 with Lvalue

    // precedence implemented using number Expr
    // lower number = higher precendece but 0 is used for no precedence i.e. for unary
    sealed trait Expr0 extends Expr1
    sealed trait Expr1 extends Expr2
    sealed trait Expr2 extends Expr3
    sealed trait Expr3 extends Expr4
    sealed trait Expr4 extends Expr5
    sealed trait Expr5 extends Expr6
    sealed trait Expr6 extends Expr

    // Unary Operators
    sealed trait UnaryOp
    case class NotOp(expr: Expr0)(val pos: (Int, Int)) extends Expr0 with UnaryOp
    case class NegateOp(expr: Expr0)(val pos: (Int, Int)) extends Expr0 with UnaryOp
    case class LenOp(expr: Expr0)(val pos: (Int, Int)) extends Expr0 with UnaryOp
    case class OrdOp(expr: Expr0)(val pos: (Int, Int)) extends Expr0 with UnaryOp
    case class ChrOp(expr: Expr0)(val pos: (Int, Int)) extends Expr0 with UnaryOp

    // note left-associativitiy so typing is (B,A) => B
    sealed trait BinaryOp extends ASTNode

    sealed trait MathExpr extends BinaryOp
    sealed trait LogicExpr extends BinaryOp
    sealed trait EqExpr extends BinaryOp
    sealed trait CmpExpr extends BinaryOp
    case class Div(exprLeft: Expr1, exprRight: Expr0)(val pos: (Int, Int)) extends Expr1 with MathExpr
    case class Mod(exprLeft: Expr1, exprRight: Expr0)(val pos: (Int, Int)) extends Expr1 with MathExpr
    case class Mul(exprLeft: Expr1, exprRight: Expr0)(val pos: (Int, Int)) extends Expr1 with MathExpr
    case class Add(exprLeft: Expr2, exprRight: Expr1)(val pos: (Int, Int)) extends Expr2 with MathExpr
    case class Sub(exprLeft: Expr2, exprRight: Expr1)(val pos: (Int, Int)) extends Expr2 with MathExpr
    case class GreaterThan(exprLeft: Expr3, exprRight: Expr2)(val pos: (Int, Int)) extends Expr3 with CmpExpr
    case class GreaterOrEqualThan(exprLeft: Expr3, exprRight: Expr2)(val pos: (Int, Int)) extends Expr3 with CmpExpr
    case class LessThan(exprLeft: Expr3, exprRight: Expr2)(val pos: (Int, Int)) extends Expr3 with CmpExpr
    case class LessOrEqualThan(exprLeft: Expr3, exprRight: Expr2)(val pos: (Int, Int)) extends Expr3 with CmpExpr
    case class Equal(exprLeft: Expr4, exprRight: Expr4)(val pos: (Int, Int)) extends Expr4 with EqExpr
    case class NotEqual(exprLeft: Expr4, exprRight: Expr4)(val pos: (Int, Int)) extends Expr4 with EqExpr
    case class And(exprLeft: Expr5, exprRight: Expr4)(val pos: (Int, Int)) extends Expr5 with LogicExpr
    case class Or(exprLeft: Expr6, exprRight: Expr5)(val pos: (Int, Int)) extends Expr6 with LogicExpr

    // PositionBridges
    trait ParserBridgePosSingleton[+A] {
        def con(pos: (Int, Int)): A
        def <#(op: Parsley[_]): Parsley[A] = pos.map(this.con(_)) <* op
    }

    trait ParserBridgePos0[+A] extends ParserBridgePosSingleton[A] {
        this: A =>
        override final def con(pos: (Int, Int)): A = this
    }

    trait ParserBridgePos1[-A, +B] extends ParserBridgePosSingleton[A=>B] {
        def apply(x: A)(pos: (Int, Int)): B
        def apply(x: Parsley[A]): Parsley[B] = pos <**> x.map(this.apply(_) _)
        override final def con(pos: (Int, Int)): A => B = this.apply(_)(pos)
    }

    trait ParserBridgePos2[-A, -B, +C] extends ParserBridgePosSingleton[(A, B) => C] {
        def apply(x: A, y: B)(pos: (Int, Int)): C
        def apply(x: Parsley[A], y: =>Parsley[B]): Parsley[C] = pos <**> (x, y).zipped(this.apply(_, _) _)
        override final def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(pos)
    }

    trait ParserBridgePos3[-A, -B, -C, +D] extends ParserBridgePosSingleton[(A, B, C) => D] {
        def apply(x: A, y: B, z: C)(pos: (Int, Int)): D
        def apply(x: Parsley[A], y: =>Parsley[B], z: =>Parsley[C]): Parsley[D] = pos <**> (x, y, z).zipped(this.apply(_, _, _) _)
        override final def con(pos: (Int, Int)): (A, B, C) => D = this.apply(_, _, _)(pos)
    }

    trait ParserBridgePos4[-A, -B, -C, -D, +E] extends ParserBridgePosSingleton[(A, B, C, D) => E] {
        def apply(x: A, y: B, z: C, w: D)(pos: (Int, Int)): E
        def apply(x: Parsley[A], y: =>Parsley[B], z: =>Parsley[C], w: =>Parsley[D]): Parsley[E] = pos <**> (x, y, z, w).zipped(this.apply(_, _, _, _) _)
        override final def con(pos: (Int, Int)): (A, B, C, D) => E = this.apply(_, _, _, _)(pos)
    }

    // Bridges

    object WACCprogram extends ParserBridgePos2[List[FunctionUnit], StatementUnit, WACCprogram]

    object FunctionUnit extends ParserBridgePos4[Type, Identifier, ParamList, StatementUnit, FunctionUnit]

    object ParamList extends ParserBridgePos1[List[Param], ParamList]

    object Param extends ParserBridgePos2[Type, Identifier, Param]

    object AssignStat extends ParserBridgePos3[Type, Identifier, Rvalue, AssignStat]
    object ReassignStat extends ParserBridgePos2[Lvalue, Rvalue, ReassignStat]
    object ReadStat extends ParserBridgePos1[Lvalue, ReadStat]
    object FreeStat extends ParserBridgePos1[Expr, FreeStat]
    object ReturnStat extends ParserBridgePos1[Expr, ReturnStat]
    object ExitStat extends ParserBridgePos1[Expr, ExitStat]
    object PrintStat extends ParserBridgePos1[Expr, PrintStat]
    object PrintlnStat extends ParserBridgePos1[Expr, PrintlnStat]
    object IfStat extends ParserBridgePos3[Expr, StatementUnit, StatementUnit, IfStat]
    object WhileStat extends ParserBridgePos2[Expr, StatementUnit, WhileStat]
    object ScopeStat extends ParserBridgePos1[StatementUnit, ScopeStat]
    object SeqStat extends ParserBridgePos1[List[StatementUnit], SeqStat]

    object PairElemFst extends ParserBridgePos1[Lvalue, PairElemFst]
    object PairElemSnd extends ParserBridgePos1[Lvalue, PairElemSnd]

    object ArrayLiteral extends ParserBridgePos1[List[Expr], ArrayLiteral]
    object NewPair extends ParserBridgePos2[Expr, Expr, NewPair]
    object Call extends ParserBridgePos2[Identifier, ArgList, Call]

    object ArgList extends ParserBridgePos1[List[Expr], ArgList]

    object ArrayType extends ParserBridgePos1[Type, ArrayType]

    object PairType extends ParserBridgePos2[PairElemType, PairElemType, PairType]
    object PairElemTypeT extends ParserBridgePos1[Type, PairElemType]
    
    object IntExpr extends ParserBridgePos1[Int, Expr0]
    object BoolExpr extends ParserBridgePos1[Boolean, Expr0]
    object CharExpr extends ParserBridgePos1[Char, Expr0]
    object StrExpr extends ParserBridgePos1[String, Expr0]
    object ArrayElem extends ParserBridgePos2[Identifier, List[Expr], ArrayElem]
    object ParenExpr extends ParserBridgePos1[Expr, Expr0]
    object Identifier extends ParserBridgePos1[String, Identifier]
    // Unary operations
    object NotOp extends ParserBridgePos1[Expr0, Expr0]
    object NegateOp extends ParserBridgePos1[Expr0, Expr0]
    object LenOp extends ParserBridgePos1[Expr0, Expr0]
    object OrdOp extends ParserBridgePos1[Expr0, Expr0]
    object ChrOp extends ParserBridgePos1[Expr0, Expr0]
    // Binary operations
    object Div extends ParserBridgePos2[Expr1, Expr0, Expr1] 
    object Mod extends ParserBridgePos2[Expr1, Expr0, Expr1] 
    object Mul extends ParserBridgePos2[Expr1, Expr0, Expr1] 
    object Add extends ParserBridgePos2[Expr2, Expr1, Expr2] 
    object Sub extends ParserBridgePos2[Expr2, Expr1, Expr2] 
    object GreaterThan extends ParserBridgePos2[Expr3, Expr2, Expr3]
    object GreaterOrEqualThan extends ParserBridgePos2[Expr3, Expr2, Expr3]
    object LessThan extends ParserBridgePos2[Expr3, Expr2, Expr3]
    object LessOrEqualThan extends ParserBridgePos2[Expr3, Expr2, Expr3]
    object Equal extends ParserBridgePos2[Expr4, Expr4, Expr4]
    object NotEqual extends ParserBridgePos2[Expr4, Expr4, Expr4]
    object And extends ParserBridgePos2[Expr5, Expr4, Expr5]
    object Or extends ParserBridgePos2[Expr6, Expr5, Expr6]    
}
