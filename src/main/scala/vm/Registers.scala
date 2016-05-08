package vm

class Registers(var ip:Int = -1, var temp:Any = -1, val stack:Stack, val summaryMap:collection.mutable.Map[String, Any]) {
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
}