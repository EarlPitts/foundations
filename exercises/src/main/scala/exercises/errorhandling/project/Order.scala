package exercises.errorhandling.project

import exercises.errorhandling.project.OrderError._
import exercises.errorhandling.project.OrderStatus._

import java.time.{Duration, Instant}
import exercises.errorhandling.NEL

case class OrderId(value: String) extends AnyVal

sealed trait Order

case class DraftOrder(
  id: OrderId,
  basket: List[Item], // basket can be modified only in "Draft" or "Checkout"
  createdAt: Instant  // set when the order is created ("Draft")
) extends Order {
  // Adds an `Item` to the basket.
  // This action is only permitted if the `Order` is in "Draft" or "Checkout" statuses.
  // If the `Order` is in "Checkout" status, move it back to "Draft".
  // Note: We don't verify if the `Item` is already in the basket.
  def addItem(item: Item): DraftOrder =
    addItems(NEL(item))

  def addItems(items: NEL[Item]): DraftOrder =
    copy(basket = basket ++ items.toList)

  def checkout: Either[OrderError, CheckoutOrder] = NEL.fromList(basket) match {
    case None         => Left(EmptyBasket)
    case Some(basket) => Right(CheckoutOrder(id, basket, None, createdAt))
  }

}

case class CheckoutOrder(
  id: OrderId,
  basket: NEL[Item],                // basket can be modified only in "Draft" or "Checkout"
  deliveryAddress: Option[Address], // can only be set during "Checkout"
  createdAt: Instant                // set when the order is created ("Draft")
) extends Order {
  // Adds an `Item` to the basket.
  // This action is only permitted if the `Order` is in "Draft" or "Checkout" statuses.
  // If the `Order` is in "Checkout" status, move it back to "Draft".
  // Note: We don't verify if the `Item` is already in the basket.
  def addItem(item: Item): Order =
    addItems(NEL(item))

  def addItems(items: NEL[Item]): Order =
    copy(basket = basket ++ items)

  def updateDeliveryAddress(address: Address): CheckoutOrder =
    copy(deliveryAddress = Some(address))

  def submit(now: Instant): Either[OrderError, SubmittedOrder] = deliveryAddress match {
    case None => Left(NoAddress)
    case Some(deliveryAddress) =>
      Right(SubmittedOrder(id, basket, deliveryAddress, createdAt, submittedAt = now))
  }

}

case class SubmittedOrder(
  id: OrderId,
  basket: NEL[Item],        // basket can be modified only in "Draft" or "Checkout"
  deliveryAddress: Address, // can only be set during "Checkout"
  createdAt: Instant,       // set when the order is created ("Draft")
  submittedAt: Instant      // set when the order is moved to "Submitted"
) extends Order {

  def deliver(now: Instant): (Order, Duration) = {
    val delivered =
      DeliveredOrder(id, basket, deliveryAddress, createdAt, submittedAt, deliveredAt = now)
    val duration = Duration.between(submittedAt, now)
    (delivered, duration)
  }
}

case class DeliveredOrder(
  id: OrderId,
  basket: NEL[Item],        // basket can be modified only in "Draft" or "Checkout"
  deliveryAddress: Address, // can only be set during "Checkout"
  createdAt: Instant,       // set when the order is created ("Draft")
  submittedAt: Instant,     // set when the order is moved to "Submitted"
  deliveredAt: Instant      // set when the order is moved to "Delivered"
) extends Order

object Order {
  // Creates an empty draft order.
  def empty(id: OrderId, now: Instant): DraftOrder =
    DraftOrder(
      id = id,
      basket = Nil,
      createdAt = now
    )
}
