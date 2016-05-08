package vm.asm

import api.API
import file.Reader
import org.scalatest.FunSuite
import vm.ChitchatVM

class TestSchemas extends FunSuite {

  val testsourcesDir = "./src/test/resources/schema_example/"

  test ("simple") {
    val simpleJson =
      """|{
        |  "a": "James",
        |  "b": "John",
        |  "c": "hello"
        |}""".stripMargin

    val fbf = API.create_fbf_summary(simpleJson, Q = 4)
    val r = Reader(testsourcesDir + "simple.asm")
    val code = r.assemble()

    val vm = new ChitchatVM(fbf)
    val res = vm.eval(code, null)
    val expected = """Map(b -> John, a -> James, c -> hello)"""
    assert(res.toString == expected)
  }

  test ("classic test") {
    // (name , event |advertisement , time ?)
    val case1 =
      """|{
        |  "name": "James",
        |  "event": "Lets see",
        |  "time": [10, 11]
        |}""".stripMargin

    val case2 =
      """|{
        |  "name": "James",
        |  "advertisement": "Lets see",
        |  "time": [10, 11]
        |}""".stripMargin

    val case3 =
      """|{
        |  "name": "James",
        |  "advertisement": "Lets see"
        |}""".stripMargin

    val case4 =
      """|{
         |  "name": "James",
         |  "ment": "Lets see"
         |}""".stripMargin

    val fbfcase1 = API.create_fbf_summary(case1, Q = 4)
    val fbfcase2 = API.create_fbf_summary(case2, Q = 4)
    val fbfcase3 = API.create_fbf_summary(case3, Q = 4)
    val fbfcase4 = API.create_fbf_summary(case4, Q = 4)

    val r = Reader(testsourcesDir + "classic.asm")
    val code = r.assemble()

    var vm = new ChitchatVM(fbfcase1)
    var res = vm.eval(code, null)
    assert(res.toString == "Map(event -> Lets see, name -> James, time -> List(10, 11))")

    vm = new ChitchatVM(fbfcase2)
    res = vm.eval(code, null)
    assert(res.toString == "Map(name -> James, advertisement -> Lets see, time -> List(10, 11))")

    vm = new ChitchatVM(fbfcase3)
    res = vm.eval(code, null)
    assert(res.toString == "Map(name -> James, advertisement -> Lets see)")

    vm = new ChitchatVM(fbfcase4)
    res = vm.eval(code, null)
    assert(res == false)
  }

  test ("classic + test") {
    // (name , event , (sensor, value)+ )
    val case1 =
      """|{
        |  "name": "James",
        |  "event": "Lets see",
        |  "sensor0": "x",
        |  "value0" : "100"
        |}""".stripMargin

    val case2 =
      """|{
        |  "name": "James",
        |  "event": "Lets see"
        |}""".stripMargin

    val fbfcase1 = API.create_fbf_summary(case1, Q = 4)
    val fbfcase2 = API.create_fbf_summary(case2, Q = 4)

    val r = Reader(testsourcesDir + "classic_rep.asm")
    val code = r.assemble()

    var vm = new ChitchatVM(fbfcase1)
    var res = vm.eval(code, null)
    assert(res.toString == "Map(event -> Lets see, sensor0 -> x, name -> James, value0 -> 100)")

    vm = new ChitchatVM(fbfcase2)
    res = vm.eval(code, null)
    assert(res == false)
  }
}
