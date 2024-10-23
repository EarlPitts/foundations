package exercises.errorhandling.project

import exercises.errorhandling.NEL
import org.scalacheck.Gen

import java.time.{Duration, Instant}

object OrderGenerator {

  val orderIdGen: Gen[OrderId] = Gen.alphaNumStr.map(OrderId(_))
  val itemIdGen: Gen[String]   = Gen.alphaNumStr

  val instantGen: Gen[Instant] =
    for {
      seconds <- Gen.choose(Instant.MIN.getEpochSecond, Instant.MAX.getEpochSecond)
      nano    <- Gen.choose(0, 1000_000_000L)
    } yield Instant.ofEpochSecond(seconds, nano)

  val durationGen: Gen[Duration] =
    Gen
      .chooseNum(0L, Duration.ofDays(400).getSeconds)
      .map(Duration.ofSeconds)

  def nelOf[A](gen: Gen[A]): Gen[NEL[A]] =
    Gen.nonEmptyListOf(gen).map {
      case Nil          => sys.error("Impossible")
      case head :: tail => NEL(head, tail)
    }

  val itemGen: Gen[Item] =
    for {
      itemId   <- itemIdGen
      quantity <- Gen.chooseNum(1, 999999)
      price    <- Gen.chooseNum(0.0001, 999999999)
    } yield Item(itemId, quantity, price)

  val addressGen: Gen[Address] =
    for {
      streetNumber <- Gen.chooseNum(1, 99999)
      postCode     <- Gen.alphaNumStr
    } yield Address(streetNumber, postCode)

  val draftGen: Gen[DraftOrder] =
    for {
      orderId   <- orderIdGen
      createdAt <- instantGen
      items     <- Gen.listOf(itemGen)
    } yield DraftOrder(orderId, items, createdAt)

  val checkoutGen: Gen[CheckoutOrder] =
    for {
      orderId   <- orderIdGen
      createdAt <- instantGen
      items     <- nelOf(itemGen)
      address   <- Gen.option(addressGen)
    } yield CheckoutOrder(orderId, items, address, createdAt)

  val submittedGen: Gen[SubmittedOrder] =
    for {
      orderId   <- orderIdGen
      createdAt <- instantGen
      items     <- nelOf(itemGen)
      address   <- addressGen
      delay     <- durationGen
      submittedAt = createdAt.plus(delay)
    } yield SubmittedOrder(orderId, items, address, createdAt, submittedAt)

  val deliveredGen: Gen[DeliveredOrder] =
    for {
      orderId   <- orderIdGen
      createdAt <- instantGen
      items     <- nelOf(itemGen)
      address   <- addressGen
      delay1    <- durationGen
      submittedAt = createdAt.plus(delay1)
      delay2 <- durationGen
      deliveredAt = submittedAt.plus(delay2)
    } yield DeliveredOrder(orderId, items, address, createdAt, submittedAt, deliveredAt)

  val orderGen: Gen[Order] =
    Gen.oneOf(draftGen, checkoutGen, submittedGen, deliveredGen)

}
