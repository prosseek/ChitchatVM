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

  def now() = {

  }
}
