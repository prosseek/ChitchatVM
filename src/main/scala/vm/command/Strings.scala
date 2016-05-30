package vm.command

import vm.Machine

trait Strings {
  /**
    * Returns current location, the whereami file should be installed
    *
    * @return
    */
  def concat(cmd:Seq[String], registers: Machine) = {
    val stack = registers.stack
    val pop2 = stack.pop().toString
    val pop1 = stack.pop().toString

    stack.push(pop1 + pop2)
  }
}
