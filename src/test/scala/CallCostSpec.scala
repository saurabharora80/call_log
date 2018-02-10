import org.scalatest.{FlatSpec, Matchers}

class CallCostSpec extends FlatSpec with Matchers {

  "cost of a call under 3 mins" should "be 0.05p per sec" in {
    CallCost(179) shouldBe 8.95
    CallCost(180) shouldBe 9.0
  }

  "cost of a call over 3 mins" should "be 0.05p sec for 180 secs and 0.03p per sec for duration above 3 mins" in {
    CallCost(181) shouldBe 9.03
    CallCost(212) shouldBe 9.96
    CallCost(321) shouldBe 13.23
  }
}
