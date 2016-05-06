package vm

import summary.Summary

import scala.util.control.Breaks._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class ChitchatVM(val summary:Summary = null)
          extends StackHelper with MacroCommand {
  // stack
  val stack = new Stack()

  // registers
  var ip: Int = -1
  var temp:Any = -1

  def process(cmd: Seq[String]) = {
    /**
      * Returns the register value with firstCommand input as name
      * When there is no matching name, the input is returned.
      *
      * ==== Warning ====
      *  1. the register values should be changed to string as this is used only in pushFromParameter function
      *
      * @param firstCommand
      * @return
      */
    def registerValueToString(firstCommand:String) = {
      firstCommand match {
        case "temp" => temp.toString
        case "bp" => stack.bp.toString
        case "ip" => ip.toString
        case "sp" => stack.sp.toString
        case _ => firstCommand
      }
    }
    def storeToRegister(firstCommand:String, value:Any) = {
      firstCommand match {
        case "temp" => temp = value
        case "bp" => stack.bp = value.asInstanceOf[Int]
        case "ip" => ip = value.asInstanceOf[Int]
        case "sp" => stack.sp = value.asInstanceOf[Int]
        case _ => throw new RuntimeException(s"Only registers should be used in popping ${firstCommand}")
      }
    }

    cmd(0) match {
      // stack command
      case "push" => {
        stack.pushFromParameter(registerValueToString(cmd(1)))
      }
      case "pop" => {
        val res = stack.pop()
        if (cmd.length >= 2) {
          storeToRegister(cmd(1), res)
        }
      }
      // load a location in a stack into tos
      case x if (x == "load" || x == "store") => {
        val register_value = registerValueToString(cmd(1))
        var address = register_value.toInt

        // ex) load bp - 3
        if (cmd.length == 4) {
          val operator = cmd(2)
          operator match {
            case "+" => address += cmd(3).toInt
            case "-" => address -= cmd(3).toInt
            case _ => throw new RuntimeException(s"only +/- operator allowed not ${operator}")
          }
        }
        if (x == "store") {
          val value = stack.pop()
          stack.stack(address) = value
        }
        else {
          val value = stack.stack(address)
          stack.push(value)
        }
      }

      case "link" => {
        link(stack)
      }

      // macro level
      // function_call 123 "p1" "p2" "p3" <- 123 is the location of a function
      case "function_call" => {
        val function_location = cmd(1).toInt
        val params = cmd.slice(2, cmd.size)

        // 1. reserve return value
        stack.push(0)
        // 2. push the parameters
        params foreach {
          p => stack.pushFromParameter(registerValueToString(p))
        }
        // 3. link
        link(stack)
        // 4. push next ip
        stack.push(ip + 1)
        // 5. jmp to the location
        ip = function_location - 1 // later 1 is added
      }

      case "function_call_stack" => {
        val function_location = cmd(1).toInt
        val param_count = cmd(2).toInt

        // get all the parameters from stack
        val params = ListBuffer[String]()
        for (i <- 0 until param_count) {
          params += stack.pop().toString()
        }
        // 1. reserve return value
        stack.push(0)
        // 2. push the parameters
        params foreach {
          p => stack.pushFromParameter(registerValueToString(p))
        }
        // 3. link
        link(stack)
        // 4. push next ip
        stack.push(ip + 1)
        // 5. jmp to the location
        ip = function_location - 1 // later 1 is added
      }

      // return 5 <- number of params
      case "return" => {
        ip = return_from_function(cmd = cmd, stack = stack)
      }

      // all elements exists in summary
      case "allexists" => {
        allexists(cmd = cmd, summary = summary, stack = stack)
      }

      // summary
      case "read" => {
        if (summary != null) {
          val label = cmd(1)
          val value = summary.get(label)
          if (value.isEmpty) stack.push(null)
          else stack.push(value.get)
        }
        else // maybe return something else
          stack.push(null)
      }

      // control
      // unconditional jump
      case "jmp" => {
        val address = registerValueToString(cmd(1)).toInt
        ip = address - 1 // ip + 1 is applied at the end, so 1 should be subtracted
      }
      // load from stack and jump if it is false
      case "jfalse" => {
        val result = stack.pop().asInstanceOf[Boolean]
        if (result == false) {
          val address = registerValueToString(cmd(1)).toInt
          ip = address - 1
        }
      }
      // Expression
      case "cmp" => {
        val val1 = stack.pop()
        val val2= stack.pop()
        stack.push(val1 == val2)
      }
      // Work as integer
      // X (val2) < Y (val1)
      case x if (x == "less" || x == "leq" || x == "greater" || x == "geq") => {
        val val2 = stack.pop().asInstanceOf[Int]
        val val1= stack.pop().asInstanceOf[Int]
        var result = false
        x match  {
          case "less"    => if (val1 < val2) result = true
          case "leq"     => if (val1 <= val2) result = true
          case "greater" => if (val1 > val2) result = true
          case "geq"     => if (val1 >= val2) result = true
        }
        stack.push(result)
      }

      // integer expression
      case x if (x == "iadd" || x == "isub" || x == "imul" || x == "idiv") => {
        val (val1, val2) = getBinaryIntValues(stack)
        x match {
          case "iadd" => stack.push(val1 + val2)
          case "isub" => stack.push(val1 - val2)
          case "imul" => stack.push(val1 * val2)
          case "idiv" => {
            if (val2 == 0) throw new RuntimeException(s"Divide by zero error")
            stack.push(val1 / val2)
          }
        }
      }
      // double expression
      case x if (x == "fadd" || x == "fsub" || x == "fmul" || x == "fdiv") => {
        val (val1, val2) = getBinaryDoubleValues(stack)
        x match {
          case "fadd" => stack.push(val1 + val2)
          case "fsub" => stack.push(val1 - val2)
          case "fmul" => stack.push(val1 * val2)
          case "fdiv" => {
            if (val2 == 0.0) throw new RuntimeException(s"Divide by zero error")
            stack.push(val1 / val2)
          }
        }
      }
      // utility - print
      case "print" => {
        println(registerValueToString(cmd(1)))
      }
    }
    ip += 1 // next command including the jmp command
  }

  /**
    * breaks down the input string
    *  1. When the input has string, it keeps the string
    *  2. Otherwise, split the input by the spaces
    *
    * ==== Example ====
    * {{{
    *   (A) (B) means the input is sepparated into A/B
    *
    *   print "hello, world" -> (print) (hello, world)
    *   mov a b c -> (mov)(a)(b)(c)
    * }}}
    *
    * @param code
    */
  def split(code:String): List[String] = {
    val regex = "([^\"]+)(\"([^\"]+)\")?".r
    val regex(content, stringPattern, stringContent) = code

    if (stringPattern != null) {
      // println(content)
      List(content.trim(), stringContent)
    }
    else {
      code.split("\\s+").toList
    }
  }

  def evalCommand(command:String, summary:Summary) = {
    val e = split(command)
    process(e)
  }

  /**
    * Run the commands in code sequence.
    * It stops when
    *  1. the command is "stop"
    *  2. the ip points outside the code sequence.
    *
    * @param code
    * @param summary
    * @return
    */
  def eval(code:Seq[String], summary:Summary) = {
    // initial condition
    ip = 0
    var proceed = true

    // make progress5
    while (proceed) {
      if (ip >= code.length)
        proceed = false
      else {
        val c = code(ip)
        if (c.startsWith("stop"))
          proceed = false
        else evalCommand(c, summary)
      }
    }
    stack.tos
  }
}
