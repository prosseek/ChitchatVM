package vm

import org.scalatest.FunSuite
import vm.util.Geolocation

class TestGeolocation extends FunSuite {
  test ("test dd2d") {
    val res = Geolocation.dd2d(List(1,59,59,99))
    assert(res - 1.99999 < 0.0001)
  }

  test ("test diff") {
    val res = Geolocation.diff(List(1,2,3,4), List(2,3,4,5))
    println(res)
  }

  test("test getDistance") {
    // http://www.movable-type.co.uk/scripts/latlong.html
    val lat1 = List(50, 3, 59, 0)
    val long1 = List(5, 42, 53, 0)
    val lat2 = List(58, 38, 38, 0)
    val long2 = List(3, 4, 12, 0)
    val d1 = Geolocation.dd2d(lat1)
    val d2 = Geolocation.dd2d(long1)
    val d3 = Geolocation.dd2d(lat2)
    val d4 = Geolocation.dd2d(long2)
    // (50.06638888888889,5.714722222222222,58.64388888888889,3.0700000000000003)
    // println(d1, d2, d3, d4)

    val res = Geolocation.getDistance(lo1 = long1, la1 = lat1, lo2 = long2, la2 = lat2)
    assert(res - 968.853546 < 0.001)
  }
}
