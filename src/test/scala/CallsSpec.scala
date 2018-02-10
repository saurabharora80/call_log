
import org.scalatest.{FlatSpec, Matchers}

class CallsSpec extends FlatSpec with Matchers {

  "Calls" should "convert comma separated values into Call" in {
    Calls(Seq(
      "1,07512345671,156",
      "2,07512345672,200,",
      "3,07512345673,300,"
    )) shouldBe Seq(
      Call("1", "07512345671", 156),
      Call("2", "07512345672", 200),
      Call("3", "07512345673", 300)
    )
  }

  "Calls" should "fail if call information is not complete" in {
    assertThrows[IllegalArgumentException] {
      Calls(Seq(
        "1,07512345671,156",
        "2,",
        "3,07512345673,300,"
      ))
    }
  }

  "Calls" should "fail if customer id is empty" in {
    assertThrows[IllegalArgumentException] {
      Calls(Seq(
        "1,07512345671,100",
        s",07512345672,200",
        "3,07512345673,300,"
      ))
    }
  }

  "Calls" should "fail if phone number has incorrect format" in {
    Seq("-", "075123", "07dsff", "fdfs", "").foreach { invalidPhNo =>
      assertThrows[IllegalArgumentException] {
        Calls(Seq(
          "1,07512345671,100",
          s"2,$invalidPhNo,200",
          "3,07512345673,300,"
        ))
      }
    }
  }

  "Calls" should "fail if call duration has incorrect format" in {
    Seq("-", "", "07dsff", "fdfs", "12d").foreach { invalidDuration =>
      assertThrows[IllegalArgumentException] {
        Calls(Seq(
          "1,07512345671,100",
          s"2,07512345672,$invalidDuration",
          "3,07512345673,300,"
        ))
      }
    }
  }
}
