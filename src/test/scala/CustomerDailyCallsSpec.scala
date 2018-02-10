import org.scalatest.{FlatSpec, Matchers}

class CustomerDailyCallsSpec extends FlatSpec with Matchers {

  "CustomerDailyCalls" should "group together calls by customer Id and " in {
    CustomerDailyCalls(Seq(
      Call("1", "07512345671", 156),
      Call("2", "07512345672", 212),
      Call("1", "07512345671", 222),
      Call("2", "07512345672", 321),
      Call("1", "07512345679", 176),
      Call("2", "07512345678", 217),
      Call("1", "07512345679", 56),
      Call("2", "07512345678", 218),
      Call("3", "07512345673", 321)
    )) should contain allElementsOf Seq(
      CustomerDailyCalls("1", Map("07512345671" -> 378, "07512345679" -> 232)),
      CustomerDailyCalls("2", Map("07512345672" -> 533, "07512345678" -> 435)),
      CustomerDailyCalls("3", Map("07512345673" -> 321))
    )
  }
}
