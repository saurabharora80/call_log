import org.scalatest.{FlatSpec, Matchers}

class PromotionSpec extends FlatSpec with Matchers {

  "Promotion" should "remove the most called number from Customer daily call records" in {
    Promotion(Seq(
      CustomerDailyCalls("1", Map("07512345671" -> 378, "07512345679" -> 232)),
      CustomerDailyCalls("2", Map("07512345672" -> 533, "07512345678" -> 435)),
      CustomerDailyCalls("3", Map("07512345673" -> 321))
    )) should contain allElementsOf Seq(
      CustomerDailyCalls("1", Map("07512345679" -> 232)),
      CustomerDailyCalls("2", Map("07512345678" -> 435)),
      CustomerDailyCalls("3", Map())
    )
  }
}
