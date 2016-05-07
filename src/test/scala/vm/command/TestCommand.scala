package vm.command

import file.Reader
import org.scalatest.FunSuite
import vm.ChitchatVM

import scala.collection.mutable.ListBuffer

class TestCommand extends FunSuite {
  val testsourcesDir = "./src/test/resources/assembly_example/"
  test ("empty code test") {
    val vm = new ChitchatVM
    val code = ListBuffer[String]()
    vm.eval(code, null)
  }

  test("arith file test") {
    val r = Reader(testsourcesDir + "arith.asm")
    val vm = new ChitchatVM
    val code = r.assemble()
    val res = vm.eval(code, null)
    assert(res == 303)
  }

  test("function call test") {
    val r = Reader(testsourcesDir + "function_call.asm")
    val vm = new ChitchatVM
    val code = r.assemble()
    val res = vm.eval(code, null)
    assert(res == 90)
  }
  test("function call stack test") {
    val r = Reader(testsourcesDir + "function_call2.asm")
    val vm = new ChitchatVM
    val code = r.assemble()
    val res = vm.eval(code, null)
    assert(res == 90)
  }

  test("if test") {
    val r = Reader(testsourcesDir + "if.asm")
    val vm = new ChitchatVM
    val code = r.assemble()
    val res = vm.eval(code, null)
  }

  test("loop test") {
    val r = Reader(testsourcesDir + "loop.asm")
    val vm = new ChitchatVM
    val code = r.assemble()
    val res = vm.eval(code, null)
  }
}
