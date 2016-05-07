package vm.command

import java.nio.file.{Files, Paths}

import vm.Registers

import sys.process._

trait System {
  /**
    * Returns current location, the whereami file should be installed
    * @return
    */
  def here(registers:Registers) = {
    val stack = registers.stack
    val (latitude, longitude) = vm.System.here()
    stack.push(longitude)
    stack.push(latitude)
  }

  def now(registers:Registers) = {
    val stack = registers.stack
    val result = vm.System.now().reverse
    stack.push(result(0)) // sec
    stack.push(result(1)) // minute
    stack.push(result(2)) // hour
    stack.push(result(3)) // day
    stack.push(result(4)) // month
    stack.push(result(5) - 2000) // year - always 2000 is based
  }
}
