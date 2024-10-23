package exercises.errorhandling.project

// import cats._
import cats.implicits._

import exercises.errorhandling.project.OrderError._
import exercises.errorhandling.project.OrderGenerator._
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{Duration, Instant}
import exercises.errorhandling.NEL

class OrderTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("checkout successful example") {
    val order = DraftOrder(
      id = OrderId("AAA"),
      basket = List(Item("A1", 2, 12.99)),
      createdAt = Instant.now()
    )

    assert(order.checkout.isRight)
  }

  test("checkout empty basket example") {
    val order = DraftOrder(
      id = OrderId("AAA"),
      basket = Nil,
      createdAt = Instant.now()
    )

    assert(order.checkout == Left(EmptyBasket))
  }

  test("submit successful example") {
    val order = CheckoutOrder(
      id = OrderId("AAA"),
      basket = NEL(Item("A1", 2, 12.99)),
      deliveryAddress = Some(Address(12, "E16 8TR")),
      createdAt = Instant.now()
    )

    assert(order.submit(Instant.now()).isRight)
  }

  test("submit no address example") {
    val order = CheckoutOrder(
      id = OrderId("AAA"),
      basket = NEL(Item("A1", 2, 12.99)),
      deliveryAddress = None,
      createdAt = Instant.now()
    )

    assert(order.submit(Instant.now()) == Left(NoAddress)) // replace ??? by the error you created for that scenario
  }

  test("happy path") {
    val orderId         = OrderId("ORD0001")
    val createdAt       = Instant.now()
    val submittedAt     = createdAt.plusSeconds(5)
    val deliveredAt     = submittedAt.plusSeconds(3600 * 30) // 30 hours
    val order           = Order.empty(orderId, createdAt)
    val item1           = Item("AAA", 2, 24.99)
    val item2           = Item("BBB", 1, 15.49)
    val deliveryAddress = Address(23, "E16 8FV")

    val oneItem  = order.addItem(item1)
    val twoItems = oneItem.addItem(item2)
    val result = for {
      order <- twoItems.checkout
      withAddress = order.updateDeliveryAddress(deliveryAddress)
      order <- withAddress.submit(submittedAt)
      orderDuration = order.deliver(deliveredAt)
    } yield orderDuration

    assert(
      result.map(_._1) == Right(
        DeliveredOrder(
          id = orderId,
          basket = NEL(item1, item2),
          deliveryAddress = deliveryAddress,
          createdAt = createdAt,
          submittedAt = submittedAt,
          deliveredAt = deliveredAt
        )
      )
    )

    assert(result.map(_._2) == Right(Duration.ofHours(30)))
  }

  test("property") {
    forAll(
      orderIdGen,
      instantGen,
      nelOf(itemGen),
      addressGen
    ) { (orderId, createdAt, items, deliveryAddress) =>
      val submittedAt = createdAt.plusSeconds(5)
      val deliveredAt = submittedAt.plusSeconds(3600 * 30) // 30 hours
      val order       = Order.empty(orderId, createdAt)

      val itemsAdded = order.addItems(items)
      val result = for {
        order <- itemsAdded.checkout
        addressUpdated = order.updateDeliveryAddress(deliveryAddress)
        order <- addressUpdated.submit(submittedAt)
      } yield order.deliver(deliveredAt)

      assert(
        result.map(_._1) == Right(
          DeliveredOrder(
            id = orderId,
            basket = items,
            deliveryAddress = deliveryAddress,
            createdAt = createdAt,
            submittedAt = submittedAt,
            deliveredAt = deliveredAt
          )
        )
      )
      assert(result.map(_._2) == Right(Duration.ofHours(30)))
    }
  }

}
