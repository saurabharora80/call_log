import org.scalatest.{FlatSpec, Matchers}

class CostPerDaySpec extends FlatSpec with Matchers {

  "cost per day for call under 3 mins" should "be duration in seconds * 0.05p" in {
    CostPerDay(Seq(
      "1,07512345671,120",
      "2,07512345672,180",
      "1,07512345673,59"),
      Seq.empty) shouldBe Map("1" -> 8.95, "2" -> 9.0)
  }

  "cost per day for call over 3 mins" should "be 180 * 0.05p + duration above 180 seconds * 0.03p" in {
    CostPerDay(Seq(
      "1,07512345671,156",
      "2,07512345672,212",
      "1,07512345673,222"),
      Seq.empty) shouldBe Map("1" -> 14.94, "2" -> 9.96)
  }

  "cost per day for each customer" should "not include the cost to the most called number" in {
    CostPerDay(Seq(
      "1,07512345671,156",
      "2,07512345672,212",
      "1,07512345671,222",
      "2,07512345672,321",
      "1,07512345679,176",
      "1,07512345679,56",
      "2,07512345678,217",
      "2,07512345678,218"), Seq(Promotion.apply _)
    ) shouldBe Map("1" -> 10.56, "2" -> 16.65)
  }

  "cost per day for each customer" should "be able to read values from call log" in {
    CostPerDay("calls.log") shouldBe Map("1" -> 7.8, "2" -> 9.96, "3" -> 13.2)
  }

  "cost per day for each customer" should "be able to read values from call log with empty lines" in {
    CostPerDay("calls_empty_lines.log") shouldBe Map("1" -> 7.8, "2" -> 9.96, "3" -> 13.2)
  }

}
