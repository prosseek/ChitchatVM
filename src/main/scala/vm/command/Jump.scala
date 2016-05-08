package vm.command

import vm.Registers

trait Jump {
  def jmp(cmd:Seq[String], registers:Registers) = {
    val address = registers.registerValueToString(cmd(1)).toInt
    registers.ip = address - 1 // ip + 1 is applied at the end, so 1 should be subtracted
  }
  def jfalse(cmd:Seq[String], registers:Registers) = {
    val stack = registers.stack

    val result = stack.pop()
    if (result.isInstanceOf[Boolean]) {
      if (result == false) {
        val address = registers.registerValueToString(cmd(1)).toInt
        registers.ip = address - 1
      }
    }
  }

  /**
    * Why do we have jpeekfalse when we have jfalse?
    *
    *  1. jfalse consumes (pops) the false value in a stack and jump.
    *  2. we need another jump that peeks (not remove the false value) and jump to end the program
    *  3. we need this for read a summary value and returns "false" when the return is null (no associcated value)
    *  4. jpeekfalse is actively used in filters.
    *
    *
    * @param cmd
    * @param registers
    */
  def jpeekfalse(cmd:Seq[String], registers:Registers) = {
    val stack = registers.stack
    if (false == stack.peek()) {
      val address = registers.registerValueToString(cmd(1)).toInt
      registers.ip = address - 1
    }
  }
}
