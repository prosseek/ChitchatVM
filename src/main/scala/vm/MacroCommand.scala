package vm

import summary.Summary

import scala.util.control.Breaks._

/**
  * Created by smcho on 5/6/16.
  */
trait MacroCommand {

  def return_from_function(cmd:Seq[String], stack:Stack) = {
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

  def allexists(cmd:Seq[String], summary:Summary, stack:Stack) = {
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
}
