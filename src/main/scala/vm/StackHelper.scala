package vm

/**
  * Created by smcho on 5/6/16.
  */
trait StackHelper {
  def getBinaryIntValues(stack:Stack) = {
    val val1:Int = stack.pop().asInstanceOf[Int]
    val val2:Int = stack.pop().asInstanceOf[Int]
    (val1, val2)
  }
  def getBinaryDoubleValues(stack:Stack) = {
    val val1:Double = stack.pop().asInstanceOf[Double]
    val val2:Double = stack.pop().asInstanceOf[Double]
    (val1, val2)
  }

  def link(stack:Stack) = {
    stack.push(stack.bp)
    stack.bp = (stack.sp - 1) // TOS is sp - 1, and it's also bp
  }
}
