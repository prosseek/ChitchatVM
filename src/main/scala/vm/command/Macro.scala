package vm.command

import summary.Summary
import vm.Registers
import vm.util.Geolocation

import scala.util.control.Breaks._

trait Macro {
  def inrange(cmd:Seq[String], registers:Registers) = {
    val stack = registers.stack
    val type_of_operation = cmd(1).toInt

    // convert them into double
    // as the comparision can be both double and int
    val value = stack.pop().asInstanceOf[Int]
    val value2 = stack.pop().asInstanceOf[Int]
    val value1 = stack.pop().asInstanceOf[Int]
    var result = false
    type_of_operation match {
      // v1 <= v <= v2
      case 0 => if (value1 <= value && value <= value2) result = true
      // v1 < v <= v2
      case 1 => if (value1 < value && value <= value2) result = true
      // v1 <= v < v2
      case 2 => if (value1 <= value && value < value2) result = true
      // v1 <= v <= v2
      case 3 => if (value1 < value && value < value2) result = true
    }
    stack.push(result)
  }

  def abs(cmd:Seq[String], registers:Registers) = {
    val stack = registers.stack

    def autoCoversionToDegree(input:Any) = {
      if (input.isInstanceOf[List[_]])
        Geolocation.dd2d(input.asInstanceOf[List[Int]])
      else if (input.isInstanceOf[Double])
        input.asInstanceOf[Double]
      else
        throw new RuntimeException(s"Location should be double or List[Int]")

    }

    cmd(1).toString() match {
      /*
        read latitude
        read longitude
        push [30, 25, 1, 74]
        push [-97, 47, 21, 83]
       */
      case "location" => {
        val long2 = autoCoversionToDegree(stack.pop())
        val lat2 = autoCoversionToDegree(stack.pop())
        val long1 = autoCoversionToDegree(stack.pop())
        val lat1 = autoCoversionToDegree(stack.pop())
        val result = Geolocation.getDistance(lat1 = lat1, long1 = long1, lat2 = lat2, long2 = long2)
        stack.push(result)
      }
    }
  }

  def allexists(cmd:Seq[String], registers:Registers, summary:Summary) = {
    val stack = registers.stack
    var result = 1 // be pessimistic
    if (summary != null) {
      breakable {
        cmd.slice(1, cmd.size) foreach {
          p => {
            val value = summary.get(p)
            if (value.isEmpty) {
              result = 0
              break
            }
          }
        }
      }
      stack.push(result)
    } else {
      stack.push(0) // no summary will return false
    }
  }

  def read(cmd:Seq[String], registers:Registers, summary:Summary) = {
    val stack = registers.stack

    if (summary != null) {
      val label = cmd(1)
      val value = summary.get(label)
      if (value.isEmpty) stack.push(null)
      else stack.push(value.get)
    }
    else // maybe return something else
      stack.push(null)
  }
}