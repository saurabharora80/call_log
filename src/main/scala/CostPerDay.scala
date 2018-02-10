import scala.io.Source
import scala.math.BigDecimal.RoundingMode

case class Call(customerId: String, phoneNumberCalled: String, durationInSeconds: Int)

object Calls {

  object Validate {

    case class Validation(fn: Array[String] => Boolean, message: Array[String] => String)

    private val validations = Seq(
      Validation(_.length < 3, d => s"Incomplete call data: ${d.mkString(",")}"),
      Validation(d => d.length >= 1 && d(0).isEmpty, d => s"Invalid customer id ${d.mkString(",")}"),
      Validation(d => d.length >= 2 && !d(1).matches("07\\d{9}"), d => s"Invalid phone number: ${d.mkString(",")}"),
      Validation(d => d.length >= 3 && !d(2).matches("\\d{1,5}"), d => s"Invalid call duration: ${d.mkString(",")}")
    )

    def apply(data: Array[String]): List[String] = {

      validations.foldLeft(List.empty[String]) { (errors, validation) => if (validation.fn(data)) validation.message(data) :: errors else errors }
    }
  }

  def apply(lines: Seq[String]): Seq[Call] = lines.map { line =>
    val data = line.trim.split(",")

    val errors = Validate(data)

    if(errors.nonEmpty) throw new IllegalArgumentException(errors.mkString(","))
    else Call(data(0), data(1), data(2).toInt)
  }
}

case class CustomerDailyCalls(customerId: String, durationPerPhoneNo: Map[String, Int])

object CustomerDailyCalls {
  def apply(calls: Seq[Call]): Seq[CustomerDailyCalls] = calls.groupBy(_.customerId).map {
    case (customerId, callsByCustomer) => CustomerDailyCalls(customerId, callsByCustomer.groupBy(_.phoneNumberCalled).map {
      case (phoneNumber, callsByNumber) => (phoneNumber, callsByNumber.map(_.durationInSeconds).sum)
    })
  }.toSeq
}

object CallCost {
  /*
    maintaining the cost precision to 2 decimal points, on the pence, to produce a accurate monthly customer bill
   */
  def apply(durationInSeconds: Int): Double = {
    val cost = if(durationInSeconds <= 180) durationInSeconds * 0.05 else 180 * 0.05 + (durationInSeconds - 180) * 0.03
    BigDecimal(cost).setScale(2, RoundingMode.HALF_UP).doubleValue()
  }
}

object Promotion {
  def apply(calls: Seq[CustomerDailyCalls]): Seq[CustomerDailyCalls] = calls.map { dailyCalls =>
    CustomerDailyCalls(dailyCalls.customerId, dailyCalls.durationPerPhoneNo.toSeq.sortBy(_._2)(Ordering.Int.reverse).tail.toMap)
  }
}

object CostPerDay {
  def apply(fileName: String, promotions: Seq[(Seq[CustomerDailyCalls]) => Seq[CustomerDailyCalls]] = Seq.empty): Map[String, Double] = {
    val callsData = Source.fromInputStream(getClass.getResourceAsStream(fileName)).getLines().drop(1).filterNot(_.isEmpty).toSeq
    apply(callsData, promotions)
  }

  /*
    Used only for unit testing
   */
  def apply(callsData: Seq[String], promotions: Seq[Seq[CustomerDailyCalls] => Seq[CustomerDailyCalls]]): Map[String, Double] = {
    promotions.foldLeft(CustomerDailyCalls(Calls(callsData))) { (calls, promotion) => promotion(calls) }.map { customerCalls =>
      (customerCalls.customerId, CallCost(customerCalls.durationPerPhoneNo.values.sum))
    }.toMap
  }
}
