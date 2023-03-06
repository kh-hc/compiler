package wacc

import scala.collection.mutable.ListBuffer

class AssemblyTranslator {
    import assemblyCode._
    import assemblyAbstractStructure._
    import scala.collection.mutable.Set
    import scala.collection.mutable.Map

    val usedFunctions = Set.empty[InBuilt]
    val stringLabelMap = Map.empty[String, String]
    var stringsCount = 0
    var labelCount = 0

 def translate(program: Program): (AssProg, Set[InBuilt], List[Block], Map[String, String]) = {
        val main = translateMain(program.main)
        val funcs = program.functions.map(f => translateFunction(f))
        return (AssProg(List(main)), usedFunctions, funcs, stringLabelMap)
    }

    def translateMain(instructions: List[Instruction]): Block = {
        val allocator = new RegisterAllocator()
        val assembly = new ListBuffer[AssInstr]
        instructions.foreach(i => assembly.appendAll(translateInstruction(i, allocator)))
        val (prefix, suffix) = allocator.generateBoilerPlate()
        return Block(Label("main"), prefix ++ (assembly.toList :+ BinaryAssInstr(Mov, None, Return, Imm(0))) ++ suffix)
    }

    def translateFunction(function: Function): Block = {
        val allocator = new RegisterAllocator()
        val retrieveArgs = allocator.loadArgs(function.args.map(p => p.id))
        val assembly = new ListBuffer[AssInstr]
        function.body.foreach(i => assembly.appendAll(translateInstruction(i, allocator)))
        val (prefix, suffix) = allocator.generateBoilerPlate()
        return Block(Label(function.id), prefix ++ retrieveArgs ++ assembly.toList ++ (NewLabel("0") +: suffix))
    }

    // TODO: Deal with arrays, pairs and strings properly
    def translateValue(value: Value, allocator: RegisterAllocator): (List[AssInstr], Operand) = value match {
        case Stored(id) => {
            allocator.getRegister(id)
        }
        case Immediate(x) => (Nil, Imm(x))
        case ArrayAccess(pos, Stored(id), formation) => {
            val (instrs, arrayToAccess) = allocator.getRegister(id)
            val (posInstrs, posLoc) = translateValue(pos, allocator)
            // Ensure that the array is not bounds checked during creation
            val errList = new ListBuffer[AssInstr]
            if (!formation){
                usedFunctions.add(OutOfBound)
                errList.appendAll(List(UnaryAssInstr(Push, None, R1),
                    BinaryAssInstr(Mov, None, R3, posLoc),
                    BinaryAssInstr(Cmp, None, R3, Imm(0)),
                    BinaryAssInstr(Mov, Some(LT), R1, R3),
                    BranchLinked(OutOfBound, Some(LT)),
                    BinaryAssInstr(Ldr, None, LR, Offset(arrayToAccess, Imm(-4))),
                    BinaryAssInstr(Mov, None, Return, Imm(4)),
                    TernaryAssInstr(Mul, None, LR, LR, Return),
                    BinaryAssInstr(Mov, None, R3, posLoc),
                    BinaryAssInstr(Cmp, None, R3, LR),
                    BinaryAssInstr(Mov, Some(GT), R1, R3),
                    BranchLinked(OutOfBound, Some(GT)),
                    UnaryAssInstr(Pop, None, R1)))
            }
            return (instrs ++ posInstrs ++ errList.toList, Offset(arrayToAccess, posLoc))
        }
        case PairAccess(pos, pair) => {
            val (pairInstrs, p) = translateValue(pair, allocator)
            usedFunctions.addOne(NullError)
            usedFunctions.addOne(PrintS)
            return (pairInstrs ++ 
                    List(BinaryAssInstr(Cmp, None, p, Imm(0)), BranchLinked(NullError, Some(EQ))),
                    Offset(p, Imm(pos match {
                case Fst => 0
                case Snd => 4
            })))
        }
        case StringLiteral(value) => {
            val stringLabel = generateStringLabel(stringsCount)
            stringLabelMap.addOne(stringLabel, value.flatMap(escapedChar))
            stringsCount = stringsCount + 1
            return (Nil, Label(stringLabel))
        }
        
        case Null => return (Nil, Imm(0))
    }

    def translateValueInto(value: Value, allocator: RegisterAllocator, destination: Register): ListBuffer[AssInstr] = {
        val (origInstr, origOperand) = translateValue(value, allocator)
        val instructions = new ListBuffer[AssInstr]
        instructions.appendAll(translateMov(origOperand, destination, allocator))
        instructions.appendAll(origInstr)
        instructions
    }

    def escapedChar(ch: Char): String = ch match {
        case '"'  => "\\\""
        case '\'' => "\\\'"
        case '\\' => "\\\\"
        case '\b' => "\\b"
        case '\n' => "\\n"
        case '\f' => "\\f"
        case '\r' => "\\r"
        case '\t' => "\\t"
        case _    => String.valueOf(ch)
    }


    def translateInstruction(instruction: Instruction, allocator: RegisterAllocator): ListBuffer[AssInstr] = 
        {
        val instructions = new ListBuffer[AssInstr]
        instruction match{
            case BinaryOperation(op, src1, src2, dst) => {
                val src1Instr = translateValueInto(src1, allocator, R1)
                val src2Instr = translateValueInto(src2, allocator, R2)
                val (destInstr, destAssembly) = translateValue(dst, allocator)
                instructions.appendAll(src1Instr)
                instructions.appendAll(src2Instr)
                instructions.appendAll(destInstr)
                op match {
                    case A_Add => {
                        usedFunctions.addOne(Overflow)
                        usedFunctions.addOne(PrintS)
                        instructions.append(TernaryAssInstr(Add, None, destAssembly, R1, R2))
                        instructions.append(BranchLinked(Overflow, Some(VS)))
                    }
                    case A_Sub => {
                        usedFunctions.addOne(Overflow)
                        usedFunctions.addOne(PrintS)
                        instructions.append(TernaryAssInstr(Sub, None, destAssembly, R1, R2))
                        instructions.append(BranchLinked(Overflow, Some(VS)))
                    }
                    case A_Mul => {
                        usedFunctions.addOne(Overflow)
                        usedFunctions.addOne(PrintS)
                        instructions.append(QuaternaryAssInstr(Smull, None, destAssembly, R3, R1, R2))
                        instructions.append(TernaryAssInstr(Cmp, None, R3, destAssembly, ASR(31)))
                        instructions.append(BranchLinked(Overflow, Some(NE)))
                    }
                    case A_Div => {
                        val (saveRegs, restoreRegs) = allocator.saveArgs(List(R1))
                        usedFunctions.addOne(DivZero)
                        usedFunctions.addOne(PrintS)
                        instructions.appendAll(saveRegs)
                        instructions.append(BinaryAssInstr(Mov, None, Return, R1))
                        instructions.append(BinaryAssInstr(Mov, None, R1, R2))
                        instructions.append(BinaryAssInstr(Cmp, None, R1, Imm(0)))
                        instructions.append(BranchLinked(DivZero, Some(EQ)))
                        instructions.append(BranchLinked(DivMod, None))
                        instructions.append(BinaryAssInstr(Mov, None, destAssembly, Return))
                        instructions.appendAll(restoreRegs)
                    }
                    case A_Mod => {
                        val (saveRegs, restoreRegs) = allocator.saveArgs(List(R1))
                        usedFunctions.addOne(DivZero)
                        usedFunctions.addOne(PrintS)
                        instructions.appendAll(saveRegs)
                        instructions.append(BinaryAssInstr(Mov, None, Return,  R1))
                        instructions.append(BinaryAssInstr(Mov, None, R1, R2))
                        instructions.append(BinaryAssInstr(Cmp, None, R1, Imm(0)))
                        instructions.append(BranchLinked(DivZero, Some(EQ)))
                        instructions.append(BranchLinked(DivMod, None))
                        instructions.append(BinaryAssInstr(Mov, None, destAssembly, R1))
                        instructions.appendAll(restoreRegs)
                    }
                    case A_And => instructions.append(TernaryAssInstr(And, None, destAssembly, R1, R2))
                    case A_Or => instructions.append(TernaryAssInstr(Or, None, destAssembly, R1, R2))
                    case A_GT => instructions.append(TernaryAssInstr(GT, None, destAssembly, R1, R2))
                    case A_GTE => instructions.append(TernaryAssInstr(GE, None, destAssembly, R1, R2))
                    case A_LT => instructions.append(TernaryAssInstr(LT, None, destAssembly, R1, R2))
                    case A_LTE => instructions.append(TernaryAssInstr(LE, None, destAssembly, R1, R2))
                    case A_EQ => instructions.append(TernaryAssInstr(EQ, None, destAssembly, R1, R2))
                    case A_NEQ => instructions.append(TernaryAssInstr(NE, None, destAssembly, R1, R2))
                }
            }
            case UnaryOperation(op, src, dst) => {
                val srcInstr = translateValueInto(src, allocator, R1)
                val (destInstr, destAssembly) = translateValue(dst, allocator)
                instructions.appendAll(srcInstr)
                instructions.appendAll(destInstr)
                op match {
                    case A_Not => List(TernaryAssInstr(NE, None, destAssembly, R1, Imm(1)))
                    case A_Neg => {
                        usedFunctions.addOne(Overflow)
                        usedFunctions.addOne(PrintS)
                        instructions.append(TernaryAssInstr(RightSub, None, destAssembly, R1, Imm(0)))
                        instructions.append(BranchLinked(Overflow, Some(VS)))
                    }
                    case A_Len => instructions.appendAll(translateMov(Offset(R1, Imm(-4)), destAssembly, allocator))
                    case A_Chr => instructions.appendAll(translateMov(R1, destAssembly, allocator))
                    case A_Ord => instructions.appendAll(translateMov(R1, destAssembly, allocator))
                    case A_ArrayCreate => {
                        instructions.appendAll(translateMov(R1, Return, allocator))
                        instructions.append(BranchLinked(Malloc, None))
                        instructions.appendAll(translateMov(Return, destAssembly, allocator))
                    }
                    case A_Assign => instructions.appendAll(translateMov(R1, destAssembly, allocator))
                    case A_Mov => instructions.appendAll(translateMov(R1, destAssembly, allocator))
                }
            }
            case InbuiltFunction(op, src) => op match {
                case A_PrintI => instructions.appendAll(translatePrint(src, allocator, PrintI))
                case A_PrintB => instructions.appendAll(translatePrint(src, allocator, PrintB))
                case A_PrintC => instructions.appendAll(translatePrint(src, allocator, PrintC))
                case A_PrintS => instructions.appendAll(translatePrint(src, allocator, PrintS))
                case A_PrintA => instructions.appendAll(translatePrint(src, allocator, PrintA))
                case A_PrintCA => {
                    usedFunctions.addOne(PrintC)
                    usedFunctions.addOne(OutOfBound)
                    instructions.appendAll(translatePrint(src, allocator, PrintI))
                }
                case A_Println => {
                    usedFunctions.addOne(PrintLn)
                    List(BranchLinked(PrintLn, None))
                }
                case A_Exit => {   
                    val (srcInstr, srcOp) = translateValue(src, allocator)
                    val exitCode: Operand = srcOp match {
                        case Imm(x) => {
                            if (x == -1) {
                                Imm(255)
                            } else {
                                Imm(x)
                            }
                        }
                        case x => x
                    }
                    instructions.appendAll(srcInstr)
                    instructions.appendAll(translateMov(exitCode, Return, allocator))
                    instructions.append(BranchLinked(Exit, None))
                }
                case A_Free => {    
                    val (srcInstr, srcOp) = translateValue(src, allocator)
                    usedFunctions.addOne(Free)
                    usedFunctions.addOne(NullError)
                    usedFunctions.addOne(PrintS)
                    instructions.appendAll(srcInstr)
                    instructions.appendAll(translateMov(srcOp, Return, allocator))
                    instructions.append(BranchLinked(Free, None))
                }
                case A_PairCreate => {
                    val (srcInstr, srcOp) = translateValue(src, allocator)
                    instructions.appendAll(srcInstr)
                    instructions.appendAll(translateMov(Imm(8), Return, allocator))
                    instructions.append(BranchLinked(Malloc, None))
                    instructions.appendAll(translateMov(Return, srcOp, allocator))
                }
                case A_ReadI => {
                    usedFunctions.addOne(ReadI)
                    val (srcInstr, srcOp) = translateValue(src, allocator)
                    instructions.appendAll(srcInstr)
                    instructions.append(BranchLinked(ReadI, None))
                    instructions.appendAll(translateMov(Return, srcOp, allocator))    
                }
                case A_ReadC => {
                    usedFunctions.addOne(ReadC)
                    val (srcInstr, srcOp) = translateValue(src, allocator)
                    instructions.appendAll(srcInstr)
                    instructions.append(BranchLinked(ReadC, None))
                    instructions.appendAll(translateMov(Return, srcOp, allocator))
                }
                case A_Return => {
                    val (srcInstr, srcOp) = translateValue(src, allocator)
                    instructions.appendAll(srcInstr)
                    instructions.appendAll(translateMov(srcOp, Return, allocator))
                    instructions.append(BranchUnconditional("0f"))
                }
                case _ => 
            }
            case FunctionCall(name, args, dst) => {
                var count = 0
                for (arg <- args) {
                    val (argInstr, argOp) = translateValue(arg, allocator)
                    if (count < 4) {
                        val reg = argumentRegisters(count)
                        instructions.appendAll(argInstr)
                        instructions.appendAll(translateMov(argOp, reg, allocator))
                    } else {
                        instructions.appendAll(argInstr)
                        instructions.append(UnaryAssInstr(Push, None, argOp))
                    }
                    count = count + 1
                }
                val (dstInstr, dstOp) = translateValue(dst, allocator)
                instructions.append(CallFunction(name))
                instructions.appendAll(dstInstr)
                instructions.appendAll(translateMov(Return, dstOp, allocator))
            }
            case IfInstruction(condition, ifInstructions, elseInstructions) => {
                val preIf = allocator.getState()
                val conditionalInstr = condition.conditions.map(i => translateInstruction(i, allocator)).flatten
                val (assInstr, assOp) = translateValue(condition.value, allocator)
                val ifInstr = ifInstructions.map(i => translateInstruction(i, allocator)).flatten
                val resetIf = allocator.reloadState(preIf)
                val elseInstr = elseInstructions.map(i => translateInstruction(i, allocator)).flatten
                val resetElse = allocator.reloadState(preIf)
                val elseLabel = generateLabel(labelCount)
                labelCount = labelCount + 1
                val endLabel = generateLabel(labelCount)
                labelCount = labelCount + 1
                instructions.appendAll(conditionalInstr)
                instructions.appendAll(assInstr)
                instructions.append(BinaryAssInstr(Cmp, None, assOp, Imm(1)))
                instructions.append(BranchNe(elseLabel))
                instructions.appendAll(ifInstr)
                instructions.appendAll(resetIf)
                instructions.append(BranchUnconditional(endLabel))
                instructions.append(NewLabel(elseLabel))
                instructions.appendAll(elseInstr)
                instructions.appendAll(resetElse)
                instructions.append(NewLabel(endLabel))
            }
            case WhileInstruction(condition, body) => {
                val originalConditionInstr = condition.conditions.map(i => translateInstruction(i, allocator)).flatten
                val (ocInstr, ocOp) = translateValue(condition.value, allocator)
                val preWhile = allocator.getState()
                val startLabel = generateLabel(labelCount)
                labelCount = labelCount + 1
                val endLabel = generateLabel(labelCount)
                labelCount = labelCount + 1
                val bodyInstrs = body.map(i => translateInstruction(i, allocator)).flatten
                val loopConditionInstr = condition.conditions.map(i => translateInstruction(i, allocator)).flatten
                val (loopInstr, loopOp) = translateValue(condition.value, allocator)
                // val preCond = allocator.getState()
                // val endCondition = condition.conditions.map(i => translateInstruction(i, allocator)).flatten
                val resetRegs = allocator.reloadState(preWhile)

                instructions.appendAll(originalConditionInstr)
                instructions.appendAll(ocInstr)
                instructions.append(BinaryAssInstr(Cmp, None, ocOp, Imm(1)))
                instructions.append(BranchNe(endLabel))
                instructions.append(NewLabel(startLabel))
                instructions.appendAll(bodyInstrs)
                instructions.appendAll(loopConditionInstr)
                instructions.appendAll(loopInstr)
                instructions.append(BinaryAssInstr(Cmp, None, loopOp, Imm(1)))
                instructions.appendAll(resetRegs)
                instructions.append(BranchEq(startLabel))
                instructions.append( NewLabel(endLabel))
            }
            case ScopeInstruction(body) => {
                val preScope = allocator.getState()
                val bodyInstrs = body.map(i => translateInstruction(i, allocator)).flatten
                val resetRegs = allocator.reloadState(preScope)
                instructions.appendAll(bodyInstrs)
                instructions.appendAll(resetRegs)
            }
        }
        instructions
    }

    def generateLabel(counter: Int): String = s".L${counter.toString}"

    def generateStringLabel(counter: Int): String = s".L.str${counter.toString}"

    def translatePrint(src: Value, allocator: RegisterAllocator, printType: InBuilt): ListBuffer[AssInstr] = {
        val instructions = new ListBuffer[AssInstr]
        usedFunctions.addOne(printType)
        val (srcInstr, srcOp) = translateValue(src, allocator)
        instructions.appendAll(srcInstr)
        instructions.appendAll(translateMov(srcOp, Return, allocator))
        instructions.append(BranchLinked(printType, None))
        instructions
    }

    def translateMov(srcAss: Operand, dstAss: Operand, allocator: RegisterAllocator): ListBuffer[AssInstr] = {
        val instructions = new ListBuffer[AssInstr]
        (srcAss, dstAss) match {
            case (Label(label), Offset(reg, offset)) => {
                reg match {
                    case r: Register => {
                        val (accessInstr, accessReg) = allocator.getNewAccessRegister(r)
                        instructions.appendAll(accessInstr)
                        instructions.append(BinaryAssInstr(Ldr, None, accessReg, Label(label)))
                        instructions.append(BinaryAssInstr(Str, None, accessReg, Offset(r, offset)))
                    }
                    case _ =>
                }
            }
            case (Label(label), _) => instructions.append(BinaryAssInstr(Ldr, None, dstAss, srcAss))
            case (Offset(reg, offset), o) => {
                reg match {
                    case r: Register => {
                        val (accessInstr, accessReg) = allocator.getNewAccessRegister(r)
                        instructions.appendAll(accessInstr)
                        instructions.append(BinaryAssInstr(Ldr, None, accessReg, Offset(reg, offset)))
                        instructions.appendAll(translateMov(accessReg, o, allocator))
                    }
                    case _ =>
                }
            }
            case (o, Offset(reg, offset)) => {
                reg match {
                    case r: Register => {
                        val (accessInstr, accessReg) = allocator.getNewAccessRegister(r)
                        instructions.appendAll(accessInstr)
                        instructions.appendAll(translateMov(o, accessReg, allocator))
                        instructions.append(BinaryAssInstr(Str, None, accessReg, Offset(reg, offset)))
                    }
                    case _ => Nil
                }
            }
            case (Imm(x), _) => {
                if (x > 255 || x < -255) {
                    instructions.append(BinaryAssInstr(Ldr, None, dstAss, srcAss))
                } else {
                    instructions.append(BinaryAssInstr(Mov, None, dstAss, srcAss))
                }
            }
            case _ => instructions.append(BinaryAssInstr(Mov, None, dstAss, srcAss))
        }
        instructions
    }
}
