package vm

import summary.Summary

import scala.collection.mutable

class ChitchatVM(val summary:Summary = null) {
  // stack
  val stack = new Stack()

  // registers
  var ip: Int = -1
  var temp:Any = -1

  def process(cmd: Seq[String]) = {
    def getBinaryIntValues() = {
      val val1:Int = stack.pop().asInstanceOf[Int]
      val val2:Int = stack.pop().asInstanceOf[Int]
      (val1, val2)
    }
    def getBinaryDoubleValues() = {
      val val1:Double = stack.pop().asInstanceOf[Double]
      val val2:Double = stack.pop().asInstanceOf[Double]
      (val1, val2)
    }
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
    def registerValue(firstCommand:String) = {
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

    def link() = {
      stack.push(stack.bp)
      stack.bp = (stack.sp - 1) // TOS is sp - 1, and it's also bp
    }

    cmd(0) match {
      // stack command
      case "push" => {
        stack.pushFromParameter(registerValue(cmd(1)))
      }
      case "pop" => {
        val res = stack.pop()
        if (cmd.length >= 2) {
          storeToRegister(cmd(1), res)
        }
      }
      // load a location in a stack into tos
      case "load" => {
        val register_value = registerValue(cmd(1))
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
        val value = stack.stack(address)
        stack.push(value)
      }
      // store tos into the location
      // ex) store bp + 3
      case "store" => {
        val register_value = registerValue(cmd(1))
        var address = register_value.toInt

        // ex) store bp - 3
        if (cmd.length == 4) {
          val operator = cmd(2)
          operator match {
            case "+" => address += cmd(3).toInt
            case "-" => address -= cmd(3).toInt
            case _ => throw new RuntimeException(s"only +/- operator allowed not ${operator}")
          }
        }
        val value = stack.pop()
        stack.stack(address) = value
      }
      case "link" => {
        link()
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
          p => stack.pushFromParameter(registerValue(p))
        }
        // 3. link
        link()
        // 4. push next ip
        stack.push(ip + 1)
        // 5. jmp to the location
        ip = function_location - 1 // later 1 is added
      }

      // return 5 <- number of params
      case "return" => {
        val number_of_params = cmd(1).toInt

        // 1. copy the return value in temp to the RV
        val rv = stack.bp - (number_of_params + 1)
        temp = stack.pop
        stack.stack(rv) = temp
        // 2. get return address
        val return_address = stack.pop.asInstanceOf[Int]
        // 3. retore bp
        stack.bp = stack.pop().asInstanceOf[Int]
        // 4. pop number_of_params times
        for (i <- 0 until number_of_params)
          stack.pop()
        ip = return_address - 1 // later 1 is added
      }

      // summary
      case "read" => {
        if (summary != null) {
          val label = cmd(1)
          val value = summary.get(label)
          if (value.isEmpty) stack.push(null)
          else stack.push(value.get)
        }
      }

      // control
      // unconditional jump
      case "jmp" => {
        val address = registerValue(cmd(1)).toInt
        ip = address - 1 // ip + 1 is applied at the end, so 1 should be subtracted
      }
      // load from stack and jump if it is false
      case "jfalse" => {
        val result = stack.pop().asInstanceOf[Int]
        if (result == 0) {
          val address = registerValue(cmd(1)).toInt
          ip = address - 1
        }
      }
      // Expression
      case "cmp" => {
        val val1 = stack.pop()
        val val2= stack.pop()
        if (val1 == val2) stack.push(1)
        else stack.push(0)
      }
      // Work as integer
      // X (val2) < Y (val1)
      case "less" => {
        val val1 = stack.pop().asInstanceOf[Int]
        val val2= stack.pop().asInstanceOf[Int]
        if (val2 < val1) stack.push(1)
        else stack.push(0)
      }
      case "leq" => {
        val val1 = stack.pop().asInstanceOf[Int]
        val val2= stack.pop().asInstanceOf[Int]
        if (val2 <= val1) stack.push(1)
        else stack.push(0)
      }
      case "greater" => {
        val val1 = stack.pop().asInstanceOf[Int]
        val val2= stack.pop().asInstanceOf[Int]
        if (val2 > val1) stack.push(1)
        else stack.push(0)
      }
      case "geq" => {
        val val1 = stack.pop().asInstanceOf[Int]
        val val2= stack.pop().asInstanceOf[Int]
        if (val2 >= val1) stack.push(1)
        else stack.push(0)
      }

      // integer expression
      case "iadd" => {
        val (val1, val2) = getBinaryIntValues()
        stack.push(val1 + val2)
      }
      case "isub" => {
        val (val1, val2) = getBinaryIntValues()
        stack.push(val1 - val2)
      }
      case "imul" => {
        val (val1, val2) = getBinaryIntValues()
        stack.push(val1 * val2)
      }
      case "idiv" => {
        val (val1, val2) = getBinaryIntValues()
        stack.push(val1 / val2)
      }
      // double
      case "fadd" => {
        val (val1, val2) = getBinaryDoubleValues()
        stack.push(val1 + val2)
      }
      case "fsub" => {
        val (val1, val2) = getBinaryDoubleValues()
        stack.push(val1 - val2)
      }
      case "fmul" => {
        val (val1, val2) = getBinaryDoubleValues()
        stack.push(val1 * val2)
      }
      case "fdiv" => {
        val (val1, val2) = getBinaryDoubleValues()
        stack.push(val1 / val2)
      }

      // utility - print
      case "print" => {
        println(registerValue(cmd(1)))
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
