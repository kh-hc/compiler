package wacc

class AbstractTranslator {
    import scala.collection.mutable.ListBuffer
    import assemblyAbstractStructure._
    import SymbolTypes._
    import abstractSyntaxTree._
    
    var intermediateCounter: Int = 0

    def getNewIntermediate: Stored = {
        intermediateCounter = intermediateCounter + 1
        return Stored(intermediateCounter.toString)    
    }
    
    def translate(ast: WACCprogram): Program = {
        val functions = ast.funcs.map(f => translateFunction(f))
        val body = translateMain(ast.stat)
        return Program(body, functions)
    }

    def translateFunction(func: FunctionUnit): Function
        = Function(func.id.id, translateStat(func.body), func.params.paramlist.map(p => Stored(func.symbolTable.get.lookupRecursiveID(p.id.id))))

    def translateMain(stat: StatementUnit): List[Instruction] = translateStat(stat)

    def translateStat(stat: StatementUnit): List[Instruction] = {
        stat match {
        case SkipStat => List.empty
        case AssignStat(t, id, value) => {
            val (rightIntermediate, instrR) = translateRvalue(value, stat.symbolTable.get)
            return instrR ++ List(UnaryOperation(A_Assign, rightIntermediate, Stored(stat.symbolTable.get.lookupRecursiveID(id.id))))
        }
        case ReassignStat(left, right) => {
            val (leftIntermediate, instrL) = translateLvalue(left, stat.symbolTable.get)
            val (rightIntermediate, instrR) = translateRvalue(right, stat.symbolTable.get)
            return instrL ++ instrR ++ List(UnaryOperation(A_Assign, rightIntermediate, leftIntermediate))
        }
        case ReadStat(value) => {
            val (dest, instr) = translateLvalue(value, stat.symbolTable.get)
            return instr ++ List(InbuiltFunction(A_Read, dest))
        }
        case FreeStat(expr) => {
            val intermediate = getNewIntermediate
            return translateExp(expr, intermediate, stat.symbolTable.get) ++ List(InbuiltFunction(A_Free, intermediate))
        }
        case ReturnStat(expr) => {
            val intermediate = getNewIntermediate
            return translateExp(expr, intermediate, stat.symbolTable.get) ++ List(InbuiltFunction(A_Return, intermediate))
        }
        case ExitStat(expr) => {
            val intermediate = getNewIntermediate
            return translateExp(expr, intermediate, stat.symbolTable.get) ++ List(InbuiltFunction(A_Exit, intermediate))
        }
        case p: PrintStat => {
            val intermediate = getNewIntermediate
            return translateExp(p.expr, intermediate, stat.symbolTable.get) ++ (p.printType.get match{
                case IntSymbol => List(InbuiltFunction(A_PrintI, intermediate))
                case BoolSymbol => List(InbuiltFunction(A_PrintB, intermediate))
                case CharSymbol => List(InbuiltFunction(A_PrintC, intermediate))
                case StringSymbol => List(InbuiltFunction(A_PrintS, intermediate))
                case a: Any => List(InbuiltFunction(A_PrintA, intermediate))
            })
        }
        case p: PrintlnStat => {
            val intermediate = getNewIntermediate
            return translateExp(p.expr, intermediate, stat.symbolTable.get) ++ (p.printType.get match{
                case IntSymbol => List(InbuiltFunction(A_PrintI, intermediate))
                case BoolSymbol => List(InbuiltFunction(A_PrintB, intermediate))
                case CharSymbol => List(InbuiltFunction(A_PrintC, intermediate))
                case StringSymbol => List(InbuiltFunction(A_PrintS, intermediate))
                case ArraySymbol(CharSymbol) => List(InbuiltFunction(A_PrintS, intermediate))
                case a: Any => List(InbuiltFunction(A_PrintA, intermediate))
            }) ++ List(InbuiltFunction(A_Println, Null))
        }
        case IfStat(cond, ifStat, elseStat) => {
            val intermediate = getNewIntermediate
            val conditions = translateExp(cond, intermediate, stat.symbolTable.get)
            return List(IfInstruction(Conditional(intermediate, conditions), translateStat(ifStat), translateStat(elseStat)))
        }
        case WhileStat(cond, body) =>  {
            val intermediate = getNewIntermediate
            val conditions = translateExp(cond, intermediate, stat.symbolTable.get)
            return List(WhileInstruction(Conditional(intermediate, conditions), translateStat(body)))
        }
        case ScopeStat(body) => translateStat(body)
        case SeqStat(statements) => statements.map(translateStat).flatten
    }
}

    def translateRvalue(value: Rvalue, st: SymbolTable): (Value, List[Instruction]) = value match {
        case ArrayLiteral(value) => {
            val array = getNewIntermediate
            var assignInstrs: List[Instruction] = List.empty
            for (i <- 0 to (value.length - 1)) {
                val indexValue = ArrayAccess(Immediate(i * 4), array)
                assignInstrs = assignInstrs ++ translateExp(value(i), indexValue, st)
            }
            // Remember to store the length in the array
            // We malloc an area of length + 1, store length in 0, then return the array address as 4
            (array, List(UnaryOperation(A_ArrayCreate, Immediate((value.length + 1)* 4), array),
                UnaryOperation(A_Mov, Immediate(value.length), ArrayAccess(Immediate(0), array)),
                BinaryOperation(A_Add, array, Immediate(4), array)) ++ assignInstrs)

        }
        case NewPair(exprLeft, exprRight) => {
            val pair = getNewIntermediate
            val leftInstr = translateExp(exprLeft, PairAccess(Fst, pair), st)
            val rightInstr = translateExp(exprRight, PairAccess(Snd, pair), st)
            return (pair, List(InbuiltFunction(A_PairCreate, pair))
                ++ leftInstr
                ++ rightInstr)
        }
        case Call(id, args) => {
            val returnVal = getNewIntermediate
            var argInstrs: List[Instruction] = List.empty
            var argList: List[Value] = List.empty
            for (e <- args.args) {
                val argValue = getNewIntermediate
                argList = argList :+ argValue
                argInstrs = argInstrs ++ translateExp(e, argValue, st)
            }
            (returnVal, argInstrs :+ FunctionCall(id.id, argList, returnVal))
        }
        case lvalue: Lvalue => translateLvalue(lvalue, st)
        case expr: Expr => {
            val intermediate = getNewIntermediate
            (intermediate, translateExp(expr, intermediate, st))
        }
    }

    def translateLvalue(value: Lvalue, st: SymbolTable): (Value, List[Instruction]) = value match {
        case PairElemFst(pairToDeref) => {
            val (pair, instr) = translateLvalue(pairToDeref, st)
            pair match {
            case d: DerefType => {
                // Dereference the address to another reg
                val intermediate = getNewIntermediate
                (PairAccess(Fst, intermediate), instr :+ UnaryOperation(A_Mov, pair, intermediate))
            }
            case _ => (PairAccess(Fst, pair), instr)
            }
        }
        case PairElemSnd(pairToDeref) => {
            val (pair, instr) = translateLvalue(pairToDeref, st)
            pair match {
            case d: DerefType => {
                // Dereference the address to another reg
                val intermediate = getNewIntermediate
                (PairAccess(Snd, intermediate), instr :+ UnaryOperation(A_Mov, pair, intermediate))
            }
            case _ => (PairAccess(Snd, pair), instr)
            }
        }
        case Identifier(id) => (Stored(st.lookupRecursiveID(id)), List.empty)
        case ArrayElem(id, position) => {
            // TODO: Refactor dereference to work like pairs
            var access = Stored(st.lookupRecursiveID(id.id))
            var inter = getNewIntermediate
            var currentPos = ArrayAccess(inter, access)
            val instructions = new ListBuffer[Instruction]

            for (pos <- position) {
                // Translate the position, then store the array access in inter
                val posInstrs = translateExp(pos, inter, st)
                // The position is now in intermediate, 
                instructions.appendAll(posInstrs)
                instructions.append(UnaryOperation(A_Mov, currentPos, inter))
                currentPos = ArrayAccess(inter, access)
                access = inter
                inter = getNewIntermediate
            }
            (currentPos, instructions.toList)
        }
    }

    def translateExp(ast: Expr, dest: Value, st: SymbolTable): List[Instruction] = {
        val newdest = getNewIntermediate
        return ast match {
            case Identifier(id) => List(UnaryOperation(A_Assign, Stored(st.lookupRecursiveID(id)), dest))
            case Add(exprLeft, exprRight) => translateExp(exprLeft, dest, st) ++ translateExp(exprRight, newdest, st) ++ List(BinaryOperation(A_Add, dest, newdest, dest))
            case Sub(exprLeft, exprRight) => translateExp(exprLeft, dest, st) ++ translateExp(exprRight, newdest, st) ++ List(BinaryOperation(A_Sub, dest, newdest, dest))
            case Div(exprLeft, exprRight) => translateExp(exprLeft, dest, st) ++ translateExp(exprRight, newdest, st) ++ List(BinaryOperation(A_Div, dest, newdest, dest))
            case Mul(exprLeft, exprRight) => translateExp(exprLeft, dest, st) ++ translateExp(exprRight, newdest, st) ++ List(BinaryOperation(A_Mul, dest, newdest, dest))
            case Mod(exprLeft, exprRight) => translateExp(exprLeft, dest, st) ++ translateExp(exprRight, newdest, st) ++ List(BinaryOperation(A_Mod, dest, newdest, dest))
            case And(exprLeft, exprRight) => translateExp(exprLeft, dest, st) ++ translateExp(exprRight, newdest, st) ++ List(BinaryOperation(A_And, dest, newdest, dest))
            case Or(exprLeft, exprRight) => translateExp(exprLeft, dest, st) ++ translateExp(exprRight, newdest, st) ++ List(BinaryOperation(A_Or, dest, newdest, dest))
            case Equal(exprLeft, exprRight) => translateExp(exprLeft, dest, st) ++ translateExp(exprRight, newdest, st) ++ List(BinaryOperation(A_EQ, dest, newdest, dest))
            case NotEqual(exprLeft, exprRight) => translateExp(exprLeft, dest, st) ++ translateExp(exprRight, newdest, st) ++ List(BinaryOperation(A_NEQ, dest, newdest, dest))
            case GreaterThan(exprLeft, exprRight) => translateExp(exprLeft, dest, st) ++ translateExp(exprRight, newdest, st) ++ List(BinaryOperation(A_GT, dest, newdest, dest))
            case GreaterOrEqualThan(exprLeft, exprRight) => translateExp(exprLeft, dest, st) ++ translateExp(exprRight, newdest, st) ++ List(BinaryOperation(A_GTE, dest, newdest, dest))
            case LessThan(exprLeft, exprRight) => translateExp(exprLeft, dest, st) ++ translateExp(exprRight, newdest, st) ++ List(BinaryOperation(A_LT, dest, newdest, dest))
            case LessOrEqualThan(exprLeft, exprRight) => translateExp(exprLeft, dest, st) ++ translateExp(exprRight, newdest, st) ++ List(BinaryOperation(A_LTE, dest, newdest, dest))
            case CharExpr(value) => List(UnaryOperation(A_Assign, Immediate(value.toInt), dest))
            case IntExpr(value) => List(UnaryOperation(A_Assign, Immediate(value), dest))
            case BoolExpr(true) => List(UnaryOperation(A_Assign, Immediate(1), dest)) 
            case BoolExpr(false) => List(UnaryOperation(A_Assign, Immediate(0), dest))
            case StrExpr(value)  => List(UnaryOperation(A_Assign, StringLiteral(value), dest))
            case NotOp(expr) => translateExp(expr, dest, st) ++  List(UnaryOperation(A_Not, dest, dest))
            case NegateOp(expr) => translateExp(expr, dest, st) ++  List(UnaryOperation(A_Neg, dest, dest))
            case ChrOp(expr) => translateExp(expr, dest, st) ++  List(UnaryOperation(A_Chr, dest, dest))
            case LenOp(expr) => translateExp(expr, dest, st) ++  List(UnaryOperation(A_Len, dest, dest))
            case OrdOp(expr) => translateExp(expr, dest, st) ++  List(UnaryOperation(A_Ord, dest, dest))
            case arrayElem: ArrayElem => {
                val (elem, instrs) = translateLvalue(arrayElem, st)
                return instrs :+ UnaryOperation(A_Assign, elem, dest)                
            }
            case PairLiteral => List(UnaryOperation(A_Assign, Null, dest))
            case ParenExpr(expr) => translateExp(expr, dest, st)
        }
    }
}