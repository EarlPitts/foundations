package exercises.errorhandling.project

sealed trait OrderError

object OrderError {
  case object EmptyBasket                              extends OrderError
  case class InvalidStatus(currentStatus: OrderStatus) extends OrderError
  case object NoAddress                                extends OrderError
}
