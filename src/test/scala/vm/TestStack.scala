package vm

import org.scalatest.FunSuite

class TestStack extends FunSuite {
  test("simple test") {
    val s = new Stack()
    s.push(100)
    val res = s.pop()
    println(res)
  }
}
