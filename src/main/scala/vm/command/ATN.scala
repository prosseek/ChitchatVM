package vm.command

import vm.Registers

trait ATN {
  def push_summary(cmd:Seq[String], registers:Registers) = {
    val summaryMap = registers.summaryMap
    val stack = registers.stack

    stack.push(summaryMap)
  }
  def register(cmd:Seq[String], registers:Registers) = {
    val summaryMap = registers.summaryMap
    val stack = registers.stack

    val label = cmd(1) + (if (cmd.length > 2) registers.registerValueToString(cmd(2)) else "")
    val value = stack.pop()
    summaryMap(label) = value
  }
}
