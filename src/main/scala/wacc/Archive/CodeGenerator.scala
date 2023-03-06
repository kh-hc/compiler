package wacc

object CodeGenerator{
    import java.io.{BufferedWriter, FileWriter, File}
    import assemblyCode._

    val opcodeMap = Map[Opcode, String](
        Add -> "adds",
        Mov -> "mov",
        Ldr -> "ldr",
        Str -> "str",
        Push -> "push",
        Pop -> "pop",
        Sub -> "subs",
        RightSub -> "rsbs",
        Cmp -> "cmp",
        Mul -> "muls",
        Smull -> "smull",
        And -> "and",
        Or -> "orr",
        // TODO: Update or remove to be correct
        EQ -> "cmp",
        NE -> "cmp",
        GE -> "cmp",
        LT -> "cmp",
        GT -> "cmp",
        LE -> "cmp"
    )

    val condMap = Map[Condition, String](
        EQ -> "eq",
        NE -> "ne",
        GE -> "ge",
        LT -> "lt",
        GT -> "gt",
        LE -> "le",
        VS -> "vs" 
    )

    val inbuiltNameMap = Map[InBuilt, String](
        PrintI -> "_printi",
        PrintB -> "_printb",
        PrintC -> "_printc",
        PrintS -> "_prints",
        PrintA -> "_printp",
        PrintCA -> "_printca",
        PrintLn -> "_println",
        ReadI -> "_readi",
        ReadC -> "_readc",
        DivMod -> "__aeabi_idivmod",
        Overflow -> "_errOverflow",
        DivZero -> "_errDivZero",
        OutOfBound -> "_errOutOfBounds",
        NullError -> "_errNull",
        Exit -> "exit",
        Free -> "_freepair",
        Malloc -> "malloc"
    )

    val registerMap = Map[Register, String](
        Return -> "r0",
        R1 -> "r1",
        R2 -> "r2",
        R3 -> "r3",
        R4 -> "r4",
        R5 -> "r5",
        R6 -> "r6",
        R7 -> "r7",
        R8 -> "r8",
        R9 -> "r9",
        R10 -> "r10",
        FP -> "fp",
        IPC -> "r12",
        SP -> "sp",
        LR -> "lr",
        PC -> "pc"
    )

    val inbuiltMap = Map[InBuilt, String](
        PrintI -> """.data
    .word 2
.L._printi_str0:		
    .asciz "%d"
.text
_printi:
    push {lr}
    push {r1}
    mov r1, r0
    ldr r0, =.L._printi_str0
    bl printf
    mov r0, #0
    bl fflush
    pop {r1}
    pop {pc}
.ltorg""",
        PrintB -> """.data
    .word 5
.L._printb_str0:
    .asciz "false"
    .word 4
.L._printb_str1:
    .asciz "true"
    .word 4
.L._printb_str2:
    .asciz "%.*s"
.text
_printb:
    push {lr}
    push {r1, r2}
    cmp r0, #0
    bne .L_printb0
    ldr r2, =.L._printb_str0
    b .L_printb1
    .ltorg
.L_printb0:
    ldr r2, =.L._printb_str1
.L_printb1:
    ldr r1, [r2, #-4]
    ldr r0, =.L._printb_str2
    bl printf
    mov r0, #0
    bl fflush
    pop {r1, r2}
    pop {pc}""",
        PrintC -> """.data
    .word 2
.L._printc_str0:
	.asciz "%c"
.text
_printc:
	push {lr}
    push {r1}
	mov r1, r0
	ldr r0, =.L._printc_str0
	bl printf
	mov r0, #0
	bl fflush
    pop {r1}
	pop {pc}
    .ltorg""",
        PrintS ->""".data
    .word 4
.L._prints_str0:
    .asciz "%.*s"
.text
_prints:
	push {lr}
    push {r1, r2}
	mov r2, r0
	ldr r1, [r0, #-4]
	ldr r0, =.L._prints_str0
	bl printf
	mov r0, #0
	bl fflush
    pop {r1, r2}
	pop {pc}
    .ltorg""",
        PrintA -> """.data
    .word 2
.L._printp_str0:
	.asciz "%p"
.text
_printp:
	push {lr}
    push {r1}
	mov r1, r0
	ldr r0, =.L._printp_str0
	bl printf
	mov r0, #0
	bl fflush
    pop {r1}
	pop {pc}
    .ltorg""",
        PrintCA -> """_printca:
push {lr}
push {fp}
mov fp, sp
ldr r12, =#44
subs sp, sp, r12
push {r10}
push {r4}
push {r5}
push {r6}
push {r7}
push {r8}
push {r9}
mov r4, r0
mov r1, #0
mov r10, r1
mov r1, r10
mov r9, r1
mov r1, r9
mov r7, r1
mov r1, r4
mov r8, r1
mov r1, r8
ldr r6, [r1, #-4]
mov r8, r6
mov r1, r7
mov r2, r8
cmp r1, r2
movlt r7, #1
movge r7, #0
cmp r7, #1
bne .L1
.L0:
mov r1, #4
mov r5, r1
mov r1, r4
str r4, [fp, #-4]
mov r4, r1
mov r1, r9
str r10, [fp, #-8]
mov r10, r1
mov r1, r10
mov r2, r5
smull r10, r3, r1, r2
cmp r3, r10, asr #31
blne _errOverflow
push {r1}
mov r3, r10
cmp r3, #0
mov r1, r3
bllt _errOutOfBounds
ldr lr, [r4, #-4]
mov r0, #4
muls lr, lr, r0
mov r3, r10
cmp r3, lr
mov r1, r3
blgt _errOutOfBounds
pop {r1}
str r9, [fp, #-12]
ldr r9, [r4, r10]
mov r1, r9
str r7, [fp, #-16]
mov r7, r1
mov r0, r7
bl _printc
str r8, [fp, #-20]
ldr r8, [fp, #-12]
mov r1, r8
mov r6, r1
mov r1, #1
str r5, [fp, #-24]
mov r5, r1
mov r1, r6
mov r2, r5
adds r6, r1, r2
blvs _errOverflow
mov r1, r6
mov r8, r1
mov r1, r8
str r4, [fp, #-28]
ldr r4, [fp, #-16]
mov r4, r1
str r10, [fp, #-32]
ldr r10, [fp, #-4]
mov r1, r10
ldr r9, [fp, #-20]
mov r9, r1
mov r1, r9
str r7, [fp, #-36]
ldr r7, [r1, #-4]
mov r9, r7
mov r1, r4
mov r2, r9
cmp r1, r2
movlt r4, #1
movge r4, #0
cmp r4, #1
str r10, [fp, #-4]
ldr r10, [fp, #-8]
str r4, [fp, #-16]
ldr r4, [fp, #-4]
ldr r7, [fp, #-16]
mov r1, r8
mov r8, r9
mov r9, r1
b 1f
.ltorg
1:
str r5, [fp, #-40]
str r6, [fp, #-44]
beq .L0
.L1:
mov r1, #0
mov r6, r1
mov r0, r6
b 0f
0:
ldr r12, =#44
adds sp, sp, r12
pop {r9}
pop {r8}
pop {r7}
pop {r6}
pop {r5}
pop {r4}
pop {r10}
pop {fp}
pop {pc}
.ltorg
""",
        PrintLn -> """.data
    .word 0
.L._println_str0:
	.asciz ""
.text
_println:
	push {lr}
    push {r1}
	ldr r0, =.L._println_str0
	bl puts
	mov r0, #0
	bl fflush
    pop {r1}
	pop {pc}
    .ltorg""",
        ReadC -> """.data
	.word 3
.L._readc_str0:
	.asciz " %c"
.text
_readc:
	push {lr}
	strb r0, [sp, #-1]!
	mov r1, sp
	ldr r0, =.L._readc_str0
	bl scanf
	ldrsb r0, [sp, #0]
	add sp, sp, #1
	pop {pc}
    .ltorg""",
        ReadI -> """.data
	.word 2
.L._readi_str0:
	.asciz "%d"
.text
_readi:
    push {lr}
    str r0, [sp, #-4]!
    mov r1, sp
    ldr r0, =.L._readi_str0
    bl scanf
    ldr r0, [sp, #0]
	add sp, sp, #4
	pop {pc}
    .ltorg""",
        Overflow -> """
.data
@ length of .L._errOverflow_str0
    .word 52
.L._errOverflow_str0:
    .asciz "#runtime_error#\n"
.text
_errOverflow:
    ldr r0, =.L._errOverflow_str0
    bl _prints
    mov r0, #255
    bl exit
    .ltorg""",
        DivZero -> """
.data
	@ length of .L._errDivZero_str0
		.word 40
.L._errDivZero_str0:
	.asciz "#runtime_error#\n"
.text
_errDivZero:
	ldr r0, =.L._errDivZero_str0
	bl _prints
	mov r0, #255
	bl exit
.ltorg
""",
        OutOfBound -> """
.data
    .word 42
.L._errOutOfBounds_str0:
	.asciz "fatal error: array index %d out of bounds\n"
.text
_errOutOfBounds:
    ldr r0, =.L._errOutOfBounds_str0
    bl printf
    mov r0, #0
    bl fflush
    mov r0, #255
    bl exit
.ltorg
 """,
        Free -> """.text
_freepair:
    push {lr}
    push {r8}
    mov r8, r0
    cmp r8, #0
    bleq _errNull
    mov r0, r8
    bl free
    pop {r8}
    pop {pc}
.ltorg
""",
    NullError -> """.data
    .word 45
.L._errNull_str0:
	.asciz "fatal error: null pair dereferenced or freed\n"
.text
_errNull:
    ldr r0, =.L._errNull_str0
    bl _prints
    mov r0, #255
    bl exit
.ltorg
    """)
    
    def buildAssembly(program: AssProg, waccName: String, usedInbuilts: Set[InBuilt], funcs: List[Block], usedStringConstants: Map[String, String]) = {
        val outputFile = new File(newFileName(waccName))
        val writer = new BufferedWriter(new FileWriter(outputFile))

        writer.append(assemblyToString(program, usedStringConstants))
        funcs.map(f => writer.append("\n" + blockToString(f)))
        usedInbuilts.map(inbuilt => writer.append("\n" + inbuiltMap(inbuilt)))
        writer.append("\n")
        writer.close()
    }

    private def newFileName(fileName: String): String = fileName.dropRight(5).split("/").last + ".s"

    def assemblyToString(program: AssProg, usedStrings: Map[String, String]): StringBuilder = {
        val assembly = new StringBuilder()
        assembly.append(".data\n")
        usedStrings.map{case (label, str) => assembly.append(makeLabel(label, str))}
        assembly.append(".text\n")
        assembly.append(".global main\n")
        program.blocks.map(b => assembly.append(blockToString(b)))
        return assembly
    }

    def makeLabel(label: String, str: String): String = s"  .word ${(str.length + 1).toString}\n$label:\n    .asciz \"${str}\"\n"

    def blockToString(block: Block): StringBuilder = {
        val blockSB = new StringBuilder()
        if (block.label.label == ("main")) {
            blockSB.append("main:\n")
        } else {
            // TODO: ensure function names are consistent
            blockSB.append(s"wacc_${block.label.label}:\n")
        }
        var lineCount = 0
        block.instrs.map(i => {
            if (lineCount > 100) {
                lineCount = 0
                blockSB.append("b 1f\n.ltorg\n1:\n")
            } else {
                lineCount = lineCount + 1
            }
            blockSB.append(instructionToString(i))
            blockSB.append("\n")})
        blockSB.append(".ltorg\n")
        return blockSB
    }

    def instructionToString(instr: AssInstr): StringBuilder = {
        val instructionBuilder = new StringBuilder()
        instr match {
            case QuaternaryAssInstr(op, cond, op1, op2, op3, op4) => {
                instructionBuilder.append(opcodeMap(op) + " ")
                instructionBuilder.append(operandToString(op1) + ", ")
                instructionBuilder.append(operandToString(op2) + ", ")
                instructionBuilder.append(operandToString(op3) + ", ")
                instructionBuilder.append(operandToString(op4))
            }
            case TernaryAssInstr(op: Condition, cond, op1, op2, op3) => {
                instructionBuilder.append(opcodeMap(op) + " ")
                instructionBuilder.append(operandToString(op2) + ", ")
                instructionBuilder.append(operandToString(op3) + "\n")
                instructionBuilder.append(op match {
                    case EQ => constructEquality(op1)
                    case NE => constructNoEquality(op1)
                    case LT => constructLessThan(op1)
                    case GT => constructGreaterThan(op1)
                    case GE => constructGreaterThanOrEqual(op1)
                    case LE => constructLessThanOrEqual(op1)
                    case _ => 
                })
            }
            case TernaryAssInstr(op, cond, op1, op2, op3) => {
                instructionBuilder.append(opcodeMap(op) + " ")
                instructionBuilder.append(operandToString(op1) + ", ")
                instructionBuilder.append(operandToString(op2) + ", ")
                instructionBuilder.append(operandToString(op3))
            }
            case BinaryAssInstr(op, cond, op1, op2) => {
                instructionBuilder.append(opcodeMap(op) + " ")
                instructionBuilder.append(operandToString(op1) + ", ")
                (op, op2) match {
                    case (Ldr, Imm(x)) => instructionBuilder.append(s"=${operandToString(op2)}")
                    case _ => instructionBuilder.append(operandToString(op2))
                }
            }
            case UnaryAssInstr(op, cond, op1) => {
                instructionBuilder.append(opcodeMap(op) + " ")
                if (op == Push || op == Pop){
                    instructionBuilder.append("{" + operandToString(op1) + "}")
                } else {
                    instructionBuilder.append(operandToString(op1))
                }
            }
            case BranchLinked(function, condCode) => {
                condCode match {
                    case None => instructionBuilder.append("bl " + inbuiltNameMap(function))
                    case Some(cond) => instructionBuilder.append("bl" + condMap(cond) + " " + inbuiltNameMap(function))
                }
            }
            case BranchEq(label) => {
                instructionBuilder.append(s"beq $label")
            }
            case BranchNe(label) => {
                instructionBuilder.append(s"bne $label")
            }
            case BranchUnconditional(label) => {
                instructionBuilder.append(s"b $label")
            }
            case NewLabel(label) => {
                instructionBuilder.append(s"$label:")
            }
            case MultiAssInstr(op, operands) => {
                instructionBuilder.append(opcodeMap(op) + " ")
                instructionBuilder.append(operands.head)
                operands.tail.map(o => instructionBuilder.append(", " + operandToString(o)))
            }
            case CallFunction(function) => {
                instructionBuilder.append("bl wacc_" + function)
            }
        }
        return instructionBuilder
    }

    // These equality builders assume that operand is the destination for the equality
    def constructEquality(operand: Operand): String = s"moveq ${operandToString(operand)}, #1\nmovne ${operandToString(operand)}, #0"

    def constructNoEquality(operand: Operand): String = s"movne ${operandToString(operand)}, #1\nmoveq ${operandToString(operand)}, #0"

    def constructGreaterThan(operand: Operand): String = s"movgt ${operandToString(operand)}, #1\nmovle ${operandToString(operand)}, #0"

    def constructLessThan(operand: Operand): String = s"movlt ${operandToString(operand)}, #1\nmovge ${operandToString(operand)}, #0"

    def constructGreaterThanOrEqual(operand: Operand): String = s"movge ${operandToString(operand)}, #1\nmovlt ${operandToString(operand)}, #0"

    def constructLessThanOrEqual(operand: Operand): String = s"movle ${operandToString(operand)}, #1\nmovgt ${operandToString(operand)}, #0"

    def operandToString(operand: Operand): String = operand match{
        case r: Register => registerMap(r)
        case Label(label) => s"=$label"
        case Offset(reg, offset) => s"[${operandToString(reg)}, ${operandToString(offset)}]"
        case Imm(x) => s"#${x.toString()}"
        case ASR(x) => s"asr #${x.toString()}"
    }
}