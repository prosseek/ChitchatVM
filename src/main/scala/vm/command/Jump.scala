package vm.command

import vm.Registers

trait Jump {
  def jmp(cmd:Seq[String], registers:Registers) = {
    val address = registers.registerValueToString(cmd(1)).toInt
    registers.ip = address - 1 // ip + 1 is applied at the end, so 1 should be subtracted
  }
  def jfalse(cmd:Seq[String], registers:Registers) = {
    val stack = registers.stack
    val result = stack.pop().asInstanceOf[Boolean]
    if (result == false) {
      val address = registers.registerValueToString(cmd(1)).toInt
      registers.ip = address - 1
    }
  }
  def jmpnull(cmd:Seq[String], registers:Registers) = {
    val stack = registers.stack
    if (null == stack.peek()) {
      val address = registers.registerValueToString(cmd(1)).toInt
      registers.ip = address - 1
    }
  }
}
