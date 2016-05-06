package vm

import file.Reader
import org.scalatest.FunSuite
import api.API
import scala.collection.mutable.ListBuffer

class TestFilterChitchatVM extends FunSuite {
  val simpleJson =
    """|{
       |  "string": "James",
       |  "age": 10,
       |  "longitude": [11,12,13,14],
       |  "latitude": [1,2,3,4],
       |  "date": [10,3,17],
       |  "time": [12,14]
       |}""".stripMargin

  val testsourcesDir = "./src/test/resources/filter_example/"
  val fbf = API.create_fbf_summary(simpleJson, Q = 4)

  test("correlated test") {
    val r = Reader(testsourcesDir + "correlated.asm")
    val code = r.assemble()

    val vm = new ChitchatVM(fbf)
    val res = vm.eval(code, null)
    assert(res == 1)
  }
  test("correlated wrong test") {
    val r = Reader(testsourcesDir + "correlated_wrong.asm")
    val code = r.assemble()

    val vm = new ChitchatVM(fbf)
    val res = vm.eval(code, null)
    assert(res == 0)
  }
}
