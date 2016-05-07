package vm.command

import vm.Registers

import scala.collection.mutable.ListBuffer

trait FunctionCall {
  def link(stack:vm.Stack) = {
    stack.push(stack.bp)
    stack.bp = (stack.sp - 1) // TOS is sp - 1, and it's also bp
  }

  def function_call(cmd:Seq[String], registers:Registers) = {
    val function_location = cmd(1).toInt
    val params = cmd.slice(2, cmd.size)
    val stack = registers.stack

    // 1. reserve return value
    stack.push(0)
    // 2. push the parameters
    params foreach {
      p => stack.pushFromParameter(registers.registerValueToString(p))
    }
    // 3. link
    link(stack)
    // 4. push next ip
    stack.push(registers.ip + 1)
    // 5. jmp to the location
    registers.ip = function_location - 1 // later 1 is added
  }
  def function_call_stack(cmd:Seq[String], registers:Registers) = {
    val function_location = cmd(1).toInt
    val param_count = cmd(2).toInt
    val stack = registers.stack

    // get all the parameters from stack
    val params = ListBuffer[String]()
    for (i <- 0 until param_count) {
      params += stack.pop().toString()
    }
    // 1. reserve return value
    stack.push(0)
    // 2. push the parameters
    params.reverse foreach {
      p => stack.pushFromParameter(registers.registerValueToString(p))
    }
    // 3. link
    link(stack)
    // 4. push next ip
    stack.push(registers.ip + 1)
    // 5. jmp to the location
    registers.ip = function_location - 1 // later 1 is added
  }

  def return_from_function(cmd:Seq[String], registers:Registers) = {
    val stack = registers.stack
    val number_of_params = cmd(1).toInt

    // 1. copy the return value in temp to the RV
    val rv = stack.bp - (number_of_params + 1)
    stack.stack(rv) = stack.pop
    // 2. get return address
    val return_address = stack.pop.asInstanceOf[Int]
    // 3. retore bp
    stack.bp = stack.pop().asInstanceOf[Int]
    // 4. pop number_of_params times
    for (i <- 0 until number_of_params)
      stack.pop()
    return_address - 1
  }
}
