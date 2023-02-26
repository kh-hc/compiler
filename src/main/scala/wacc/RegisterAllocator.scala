package wacc



class RegisterAllocator() {
    import assemblyCode._
    import scala.collection.mutable.{Map, ListBuffer, Stack, Queue, Set}
    import Storage._
    var stackSize: Int = 0
    var storage = Map.empty[String, Storage]
    var registerMap = Map.empty[Register, String]

    val accessRegisters = Set[Register]()

    val availableRegisters = Stack[Register]()
    availableRegisters.pushAll(generalRegisters)
    val usedRegisters = Queue[Register]()
    val usedEverRegisters = Set[Register]()

    // Note that this is heavily unoptimized
    def getRegister(name: String): (List[AssInstr], Register) = storage.get(name) match{
        case Some(s) => s match {
            case Reg(r) => {
                return (Nil, r)
            }
            case Stored(offset) => {
                val (instructions, register) = getFreeRegister()
                store(name, register)
                return (instructions :+ BinaryAssInstr(Ldr, None, register, Offset(FP, Imm(-(4 * offset)))), register)
            }
        }
        case None => {
            val (instructions, register) = getFreeRegister()
            store(name, register)
            return (instructions, register)
        }
    }

    // Gets registers for nested array/pair accesses
    // Assumes that there are more than two registers available
    def getNewAccessRegister(accessLocation: Register): (List[AssInstr], Register) = {
        if (availableRegisters.isEmpty) {
            if (usedRegisters.front != accessLocation){
                return getFreeRegister()
            } else {
                usedRegisters.dequeue()
                usedRegisters.enqueue(accessLocation)
                return getFreeRegister()
            }
        } else{
            return getFreeRegister()
        }
    }

    private def getFreeRegister(): (List[AssInstr], Register) = {
        val freeingInstructions = new ListBuffer[AssInstr]
        if (availableRegisters.isEmpty) {
            // If there are no available registers, store a value on the stack
            val registerToFree = usedRegisters.dequeue()
            if (registerMap.contains(registerToFree)) {
                val variableToStore = registerMap.get(registerToFree)
                stackSize = stackSize + 1
                freeingInstructions += BinaryAssInstr(Str, None, registerToFree, Offset(FP, Imm(-(4 * stackSize))))
                storage(variableToStore.get) = Stored(stackSize)
            }
            availableRegisters.push(registerToFree)
        }
        val reg = availableRegisters.pop()
        usedEverRegisters.add(reg)
        usedRegisters.enqueue(reg)
        return (freeingInstructions.toList, reg)
    }

    private def store(name: String, register: Register) = {
        // Assuming the old value in the register has been stored already
        registerMap(register) = name
        storage(name) = Reg(register)
    }

    def generateBoilerPlate(): (List[AssInstr], List[AssInstr]) = {
        val pushInstr = ListBuffer[AssInstr]()
        pushInstr.append(UnaryAssInstr(Push, None, FP))
        pushInstr.append(UnaryAssInstr(Push, None, LR))
        pushInstr.append(BinaryAssInstr(Mov, None, FP, SP))
        if (stackSize > 0) {
            pushInstr.append(TernaryAssInstr(Sub, None, SP, SP, Imm(stackSize * 4)))
        }
        val popInstr = ListBuffer[AssInstr]()
        for (reg <- usedEverRegisters) {
            pushInstr.append(UnaryAssInstr(Push, None, reg))
            popInstr.append(UnaryAssInstr(Pop, None, reg))
        }
        val revPop = popInstr.reverse
        if (stackSize > 0) {
            revPop.prepend(TernaryAssInstr(Add, None, SP, SP, Imm(stackSize * 4)))
        }
        revPop.append(UnaryAssInstr(Pop, None, PC))
        revPop.append(UnaryAssInstr(Pop, None, FP))
        return (pushInstr.toList, revPop.toList)
    }

    def saveArgs(regs: List[Register]): (List[AssInstr], List[AssInstr]) = {
        // TODO: Save these properly
        return (regs.map(r => UnaryAssInstr(Push, None, r)).toList, regs.map(r => UnaryAssInstr(Pop, None, r)).reverse.toList)
    }
}

object Storage {
    import assemblyCode._
    sealed trait Storage
    case class Reg(reg: Register) extends Storage
    case class Stored(offset: Int) extends Storage
}