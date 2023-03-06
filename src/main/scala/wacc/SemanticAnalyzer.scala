package wacc
import wacc.abstractSyntaxTree._
import scala.collection.mutable.ListBuffer

class SemanticAnalyzer(file: String) {
    import SymbolTypes._    
    import WACCErrors._

    // Stores the errors - this allows us to analyze the entire program and catch all the semantic errors
    var errorStack = new ListBuffer[WACCError]

    // Checks whether any errors have been found
    def isErrors(): Boolean = {
        return errorStack.length > 0 
    }

    // Gets a list of errors 
    def getErrors(): List[WACCError] = {
        return errorStack.toList
    }

    // Main analysis function
    def analyzeProgram(ast: WACCprogram) = {
        val symbolTable = new SymbolTable(None)

        // Record function definitions in the symbol table as functions can be mutually recursive
        ast.funcs.map(f => {
            try{
                symbolTable.add(f)
            } catch {
                case  e: Exception => {
                    errorStack += varAlreadyAss.err(f.id)(file)
                } 
            }
        })

        // Analyze the body of the functions
        ast.funcs.map(f => checkFunction(f, symbolTable))

        // Using NoReturn allows checking that the main body does not return anything
        checkStatement(ast.stat, symbolTable, NoReturn)
        ast.symbolTable = Some(symbolTable)
    }

    def checkFunction(function: FunctionUnit, symbolTable: SymbolTable) = {
        // Create a symbol table for the argument scope
        val argsSymbols = new SymbolTable(Some(symbolTable))

        // Add each parameter into the new symbol table 
        function.params.paramlist.map(p => {
            try {
                argsSymbols.add(p.id.id, p.t) 
            } catch {
                case  e: Exception => {
                    errorStack += varAlreadyAss.err(p.id)(file)
                }
            }})

        // Analyze the function, with a new child symbol table as function arguments can be shadowed
        val functionSymbolTable = new SymbolTable(Some(argsSymbols))
        checkStatement(function.body, functionSymbolTable, translate(function.t))
        function.symbolTable = Some(functionSymbolTable)
    }

    def checkStatement(statement: StatementUnit, symbolTable: SymbolTable, returnType: SymbolType): Unit = {
        statement.symbolTable = Some(symbolTable)
        statement match {
            case SkipStat => ()
            case AssignStat(t, id, value) => {
                // Set the type of the AST node
                val expectedType = translate(t)
                statement.tiepe = Some(expectedType)
                // Check that we are not trying to assign to something that already exists
                if (symbolTable.lookup(id.id).isEmpty) {
                    val providedType = checkRvalue(value, symbolTable)
                    if (expectedType != providedType) {
                        // If the type of the right value provided is unexpected, this could be due to ambiguous typing rules with pairs and arrays:
                        providedType match {
                            case TopPairSymbol(a, b) => expectedType match {
                                case TopPairSymbol(x, y) => {
                                    if (!((x == NestedPairSymbol || a == NestedPairSymbol || x == a)
                                    && (y == NestedPairSymbol || b == NestedPairSymbol || b == y))) {
                                        errorStack += unexpectedTypeStat.err(statement, providedType, expectedType)(file)
                                    }
                                }
                                case NestedPairSymbol => 
                                case PairLiteralSymbol =>
                                case default => errorStack += unexpectedTypeStat.err(statement, providedType, expectedType)(file)
                            }
                            case PairLiteralSymbol => expectedType match {
                                case TopPairSymbol(ft, st) => 
                                case NestedPairSymbol => 
                                case default => errorStack += unexpectedTypeStat.err(statement, providedType, expectedType)(file)
                            }
                            case ArraySymbol(AmbiguousSymbol) => expectedType match {
                                case ArraySymbol(x) =>
                                case default => errorStack += unexpectedTypeStat.err(statement, providedType, expectedType)(file)
                            }
                            case ArraySymbol(TopPairSymbol(x, y)) => expectedType match {
                                case ArraySymbol(TopPairSymbol(a, b)) => {
                                    if (!((x == NestedPairSymbol || a == NestedPairSymbol || x == a)
                                    && (y == NestedPairSymbol || b == NestedPairSymbol || b == y))) {
                                        errorStack += unexpectedTypeStat.err(statement, providedType, expectedType)(file)
                                    }
                                }
                                case ArraySymbol(NestedPairSymbol) => 
                                case ArraySymbol(PairLiteralSymbol) =>
                                case default => errorStack += unexpectedTypeStat.err(statement, providedType, expectedType)(file)
                            }
                            case ArraySymbol(PairLiteralSymbol) => expectedType match {
                                case ArraySymbol(TopPairSymbol(ft, st)) => 
                                case ArraySymbol(NestedPairSymbol) => 
                                case default => errorStack += unexpectedTypeStat.err(statement, providedType, expectedType)(file)
                            }
                            case ArraySymbol(NestedPairSymbol) => expectedType match {
                                case ArraySymbol(x) =>
                                case default => errorStack += unexpectedTypeStat.err(statement, providedType, expectedType)(file)
                            }
                            case NestedPairSymbol => 
                            case AmbiguousSymbol => 
                            case default => errorStack += unexpectedTypeStat.err(statement, providedType, expectedType)(file)
                        }
                    }
                    try {
                        symbolTable.add(id.id, expectedType)
                    } catch {
                        case e: Exception => {
                            errorStack += varAlreadyAss.err(id)(file)
                        }
                    }
                    return ()
                } else {
                    errorStack += varAlreadyAss.err(id)(file)
                    return ()
                }
            }
            case ReassignStat(left, right) => {
                val leftType = checkLvalue(left, symbolTable)
                val rightType = checkRvalue(right, symbolTable)
                statement.tiepe = Some(leftType)
                // If the types are not equal, we may be dealing with the ambiguous typing rules with pairs/arrays
                if (leftType != rightType) {
                    leftType match {
                        case TopPairSymbol(ft, st) => rightType match {
                            case NestedPairSymbol => 
                            case PairLiteralSymbol => 
                            case default => errorStack += unexpectedTypeStat.err(statement, rightType, leftType)(file)
                        }
                        case ArraySymbol(AmbiguousSymbol) => rightType match {
                            case ArraySymbol(x) =>
                            case default => errorStack += unexpectedTypeStat.err(statement, rightType, leftType)(file)
                        }
                        case AmbiguousSymbol =>
                        case PairLiteralSymbol =>  rightType match {
                            case TopPairSymbol(ft, st) => 
                            case NestedPairSymbol =>
                            case default => errorStack += unexpectedTypeStat.err(statement, rightType, leftType)(file) 
                        }
                        case NestedPairSymbol =>
                        case default => errorStack += unexpectedTypeStat.err(statement, rightType, leftType)(file) 
                    }
                }
                // Disallow having an ambiguous type on both sides of the reassign, as mentioned in the spec
                if ((leftType == NestedPairSymbol) && (rightType == NestedPairSymbol)) {
                    errorStack += ambiguousTypesReAss.err(statement, left, right)(file) 
                }
                return ()
            }
            case r: ReadStat => checkLvalue(r.value, symbolTable) match {
                // We can only read ints or chars
                case IntSymbol => r.tiepe = Some(checkLvalue(r.value, symbolTable))
                case CharSymbol => r.tiepe = Some(checkLvalue(r.value, symbolTable))
                case default => errorStack += readError.err(r.value, default)(file) 
            }
            case FreeStat(expr) => {
                    val freeType = checkExpression(expr, symbolTable)
                    statement.tiepe = Some(freeType)
                    freeType match {
                    // Makes sure that we are only freeing pairs or arrays
                    case ArraySymbol(_) => ()
                    case NestedPairSymbol => ()
                    case PairLiteralSymbol => ()
                    case TopPairSymbol(_, _) => ()
                    case default => errorStack += freeError.err(expr, default)(file)
                }
            }
            case ReturnStat(expr) => {
                statement.tiepe = Some(returnType)
                checkEvaluatesTo(expr, symbolTable, returnType)
            }
            case ExitStat(expr) => {
                statement.tiepe = Some(IntSymbol)
                checkEvaluatesTo(expr, symbolTable, IntSymbol)
            }
            case p: PrintStat => statement.tiepe = Some(checkExpression(p.expr, symbolTable))
            case p: PrintlnStat => statement.tiepe = Some(checkExpression(p.expr, symbolTable))
            case IfStat(cond, ifStat, elseStat) => {
                checkEvaluatesTo(cond, symbolTable, BoolSymbol)
                val ifSymbols = new SymbolTable(Some(symbolTable))
                checkStatement(ifStat, ifSymbols, returnType)
                val elseSymbols = new SymbolTable(Some(symbolTable))
                checkStatement(elseStat, elseSymbols, returnType)
            }
            case WhileStat(cond, body) => {
                checkEvaluatesTo(cond, symbolTable, BoolSymbol)
                val bodySymbols = new SymbolTable(Some(symbolTable))
                checkStatement(body, bodySymbols, returnType)
            }
            case ScopeStat(stat) => {
                val scopeSymbols = new SymbolTable(Some(symbolTable))
                checkStatement(stat, scopeSymbols, returnType)
            }
            case SeqStat(stats) => {
                stats.map(s => checkStatement(s, symbolTable, returnType))
            }
        }
    }

    def checkEvaluatesTo(expr: Expr, symbolTable: SymbolTable, t: SymbolType): Unit = {
        expr.tiepe = Some(t)
        t match {
            // We only try evaluting to NoReturn if we are trying to return from the body of the code
            case NoReturn => errorStack += returnError.err(expr)(file)
            // Nested pairs can match with anything
            case NestedPairSymbol => return ()
            case TopPairSymbol(x, y) => {
                val exprType = checkExpression(expr, symbolTable)
                if (exprType != t) {
                    exprType match {
                        case NestedPairSymbol => return ()
                        case PairLiteralSymbol => return ()
                        case _ => errorStack += unexpectedTypeExpr.err(expr, exprType, t)(file)
                    }
                }
            }
            case symbolType => {
                val exprType = checkExpression(expr, symbolTable)
                // If the expected type does not match the actual type, check that the evalution does not lead to an ambiguous type
                if (exprType != symbolType) {
                    if (exprType == NestedPairSymbol) {
                        return ()
                    }
                    if (symbolType == ArraySymbol(AmbiguousSymbol)) {
                        exprType match{
                            case ArraySymbol(subtype) => return ()
                            case _ => errorStack += unexpectedTypeExpr.err(expr, exprType, symbolType)(file) 
                        }
                    } else if (symbolType == PairLiteralSymbol) {
                        exprType match {
                            case TopPairSymbol(x, y) =>
                            case _ => errorStack += unexpectedTypeExpr.err(expr, exprType, symbolType)(file) 
                        }
                    } 
                    else {
                        errorStack += unexpectedTypeExpr.err(expr, exprType, symbolType)(file) 
                    }
                }
                return ()
            }
        }
    }

    def checkExpression(expr: Expr, st: SymbolTable): SymbolType = expr match {
        case IntExpr(_) => {
            expr.tiepe = Some(IntSymbol)
            IntSymbol
        }
        case BoolExpr(_) => {
            expr.tiepe = Some(BoolSymbol)
            BoolSymbol
        }
        case CharExpr(_) => {
            expr.tiepe = Some(CharSymbol)
            CharSymbol
        }
        case StrExpr(_) => {
            expr.tiepe = Some(StringSymbol)
            StringSymbol
        }
        case Identifier(id) => st.lookupRecursive(id) match {
            // If we can't find the identifer, log an error but then continue the analysis with an amibiguous symbol
            case None => {
                errorStack += undefinedVar.err(expr.asInstanceOf[Identifier])(file) 
                AmbiguousSymbol
            }
            case Some(value) => {
                expr.tiepe = Some(value)
                value
            }
        }
        case ArrayElem(id, positions) => {
            val arrayType: SymbolType = st.lookupRecursive(id.id) match {
                case Some(value) => value
                case None => {
                    errorStack += undefinedVar.err(id)(file) 
                    return AmbiguousSymbol
                }
            }
            expr.tiepe = Some(arrayType)
            positions.map(p => checkEvaluatesTo(p, st, IntSymbol))
            // Makes sure that the array types match fully, and logs an error if not
            try {
                derefType(arrayType, positions.length)
            } catch {
                case e: Exception => {
                    errorStack += derefErrE.err(expr)(file) 
                    AmbiguousSymbol
                }
            }
        }
        case ParenExpr(e) => {
            val ex = checkExpression(e, st)
            expr.tiepe = Some(ex)
            ex
        }
        case ChrOp(e) => {
            checkEvaluatesTo(e, st, IntSymbol)
            expr.tiepe = Some(CharSymbol)
            CharSymbol
        }
        case LenOp(e)  => {
            checkEvaluatesTo(e, st, ArraySymbol(AmbiguousSymbol))
            expr.tiepe = Some(IntSymbol)
            IntSymbol
        }
        case NegateOp(e)  => {
            checkEvaluatesTo(e, st, IntSymbol)
            expr.tiepe = Some(IntSymbol)
            IntSymbol
        }
        case NotOp(e)  => {
            checkEvaluatesTo(e, st, BoolSymbol)
            expr.tiepe = Some(BoolSymbol)
            BoolSymbol
        }
        case OrdOp(e)  => {
            checkEvaluatesTo(e, st, CharSymbol)
            expr.tiepe = Some(IntSymbol)
            IntSymbol
        }
        case m: MathExpr => checkMathExpr(m, st)
        case c: CmpExpr => checkOrderExpr(c, st)
        case e: EqExpr => checkEqualExpr(e, st)
        case l: LogicExpr => checkBoolExpr(l, st)
        case PairLiteral => PairLiteralSymbol
    }

    def checkEqualExpr(e: EqExpr, st: SymbolTable): SymbolType = {
        // Checks that the types on either side of a binary expression are the same
        var left: Option[Expr] = None
        var right: Option[Expr] = None
        e match {
            case Equal(l, r) => {
                left = Some(l)
                right = Some(r)
            }
            case NotEqual(l, r)  => {
                left = Some(l)
                right = Some(r)
            }
        }
        val exprType = checkExpression(left.get, st)
        checkEvaluatesTo(right.get, st, exprType)
        BoolSymbol
    }

    def checkOrderExpr(e: CmpExpr, st: SymbolTable): SymbolType = {
        // Checks that the types on either side of a binary expression could be ints or chars
        var left: Option[Expr] = None
        var right: Option[Expr] = None
        e match {
            case GreaterOrEqualThan(l, r) => {
                left = Some(l)
                right = Some(r)
            }
            case GreaterThan(l, r) => {
                left = Some(l)
                right = Some(r)
            }
            case LessOrEqualThan(l, r) => {
                left = Some(l)
                right = Some(r)
            }
            case LessThan(l, r) => {
                left = Some(l)
                right = Some(r)
            }
        }
        val exprType = checkExpression(left.get, st)
        checkEvaluatesTo(right.get, st, exprType)
        if (!(exprType == IntSymbol || exprType == CharSymbol)) {
            errorStack += unexpectedTypeExpr.err(left.get, exprType, IntSymbol)(file) 
        }
        BoolSymbol
    }

    def checkMathExpr(e: MathExpr, st: SymbolTable): SymbolType = {
        var left: Option[Expr] = None
        var right: Option[Expr] = None
        e match {
            case Mul(l, r) => {
                left = Some(l)
                right = Some(r)
            }
            case Add(l, r) => {
                left = Some(l)
                right = Some(r)
            }
            case Sub(l, r) => {
                left = Some(l)
                right = Some(r)
            }
            case Div(l, r) => {
                left = Some(l)
                right = Some(r)
            }
            case Mod(l, r) => {
                left = Some(l)
                right = Some(r)
            }
        }
        e.tiepe = Some(IntSymbol)
        // Checks that both expressions could evaluate to ints
        checkEvaluatesTo(left.get, st, IntSymbol)
        checkEvaluatesTo(right.get, st, IntSymbol)
        IntSymbol
    }

    def checkBoolExpr(e: LogicExpr, st: SymbolTable): SymbolType = {
        // Checks that both expressions could evalute to chars
        var left: Option[Expr] = None
        var right: Option[Expr] = None
        e match {
            case And(l, r) => {
                left = Some(l)
                right = Some(r)
            }
            case Or(l, r) => {
                left = Some(l)
                right = Some(r)
            }
        }
        e.tiepe = Some(BoolSymbol)
        checkEvaluatesTo(left.get, st, BoolSymbol)
        checkEvaluatesTo(right.get, st, BoolSymbol)
        BoolSymbol
    }

    def checkLvalue(lValue: Lvalue, st: SymbolTable): SymbolType = {
        val symbolType = lValue match {
            case PairElemFst(pair) => checkLvalue(pair, st) match {
                case TopPairSymbol(fst, snd) => return fst
                case PairLiteralSymbol => return NestedPairSymbol
                case NestedPairSymbol => return NestedPairSymbol
                case default => {
                    errorStack += derefErr.err(lValue, default)(file) 
                    NestedPairSymbol
                }
            }
            case PairElemSnd(pair) => checkLvalue(pair, st) match {
                case TopPairSymbol(fst, snd) => return snd
                case PairLiteralSymbol => return NestedPairSymbol
                case NestedPairSymbol => return NestedPairSymbol
                case default => {
                    errorStack += derefErr.err(lValue, default)(file) 
                    NestedPairSymbol
                }
            }
            // All l-values types that are not pair elems are encompassed in the evalution of expressions, so we can use the same checker
            case expr: Expr => return checkExpression(expr, st)
        }
        lValue.tiepe = Some(symbolType)
        symbolType
    }

    def checkRvalue(value: Rvalue, st: SymbolTable): SymbolType = {
        val symbolType = value match {
            case ArrayLiteral(contents) => {
                val arrayTypes: List[SymbolType] = contents.map(e => checkExpression(e, st))
                // Ensures that every element in the array is of the same type, and returns that type
                if (arrayTypes.length > 0) {
                    if (arrayTypes.distinct.length == 1) {
                        ArraySymbol(arrayTypes.head)
                    } else {
                        errorStack += arrayTypeErr.err(value, arrayTypes.head)(file) 
                        ArraySymbol(AmbiguousSymbol)
                    }            
                } else {
                    ArraySymbol(AmbiguousSymbol)
                }
            }
            case NewPair(left, right) => {
                val symbolLeft = checkExpression(left, st)
                val symbolRight = checkExpression(right, st)
                // Ensures that the pair type nesting heirachy is preserved when new pairs are created
                TopPairSymbol(symbolLeft match {
                    case TopPairSymbol(ft, st) => PairLiteralSymbol
                    case PairLiteralSymbol => NestedPairSymbol
                    case NestedPairSymbol => NestedPairSymbol
                    case default => default
                }, symbolRight match {
                    case TopPairSymbol(ft, st) => PairLiteralSymbol
                    case NestedPairSymbol => NestedPairSymbol
                    case PairLiteralSymbol => NestedPairSymbol
                    case default => default
                })
            }
            case Call(id, args) => st.lookupFunctionRecursive(id.id) match {
                // Check that the function exists and the argument types match
                case Some(FunctionSymbol(returnT, argsT)) => {
                    matchArgs(argsT, args.args, st, id)
                    returnT
                }
                case _ => {
                    errorStack += undefFunc.err(id)(file) 
                    AmbiguousSymbol
                }
            }
            // Reduce code duplication by letting l-value and expression evalute the rest of the r-values (they evaluate in the same way)
            case PairElemFst(pair) => checkLvalue(value.asInstanceOf[Lvalue], st)
            case PairElemSnd(pair) => checkLvalue(value.asInstanceOf[Lvalue], st)
            case expr => checkExpression(expr.asInstanceOf[Expr], st)
        }
    value.tiepe = Some(symbolType)
    symbolType
    }

    def matchArgs(expected: List[SymbolType], provided: List[Expr], st: SymbolTable, id: Identifier) = {
        // Checks that the function is given the arguments that we expect
        if (expected.length != provided.length) {
            errorStack += argumentMismatch.err(id)(file) 
        } else { 
            expected.lazyZip(provided).map((ex, pr) => checkEvaluatesTo(pr, st, ex))            
        }
    }
}
