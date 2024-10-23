package exercises.dataprocessing

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import cats._
import cats.implicits._
import exercises.dataprocessing.MyMonoid

// For example, here is a ParList[Int] with two partitions:
// ParList(
//  List(1,2,3,4,5,6,7,8), // partition 1
//  List(9,10)             // partition 2
// )
// Note that we used the `apply` method with a varargs argument.
case class ParList[A](partitions: List[List[A]])(implicit ec: ExecutionContext) {
  def toList = partitions.flatten

  def monoFoldLeft(z: A)(f: (A, A) => A): A =
    partitions
      .map(_.foldLeft(z)(f))
      .foldLeft(z)(f)

  def monoFoldLeft(implicit m: MyMonoid[A]): A =
    partitions
      .map(_.foldLeft(m.empty)(m.combine))
      .foldLeft(m.empty)(m.combine)

  def foldMap[B: MyMonoid](f: A => B): B = {
    val m = MyMonoid[B]
    partitions
      .map(_.foldLeft(m.empty)((acc: B, a: A) => m.combine(acc, f(a))))
      .foldLeft(m.empty)(m.combine)
  }

  def parFoldMap[B: MyMonoid](f: A => B): B = {
    val m = MyMonoid[B]
    partitions
      .map { p: List[A] =>
        Future {
          p.foldLeft(m.empty)((acc: B, a: A) => m.combine(acc, f(a)))
        }
      }
      .map(Await.result(_, Duration.Inf))
      .foldLeft(m.empty)(m.combine)
  }

  def size: Int = parFoldMap(_ => 1)
}

object ParList {
  implicit def functorInstance(implicit ec: ExecutionContext): Functor[ParList] = new Functor[ParList] {
    def map[A, B](fa: ParList[A])(f: A => B): ParList[B] =
      ParList(fa.partitions.map(_.map(f)))
  }

  // The `*` at the end of List[A] is called a varargs. It means we can put as many arguments
  // as we want of type List[A] and the Scala compiler will automatically packs these arguments
  // into a collection.
  // For example, ParList(List(1,2), List(3,4)) == ParList(List(List(1,2), List(3,4)))
  // This is why we can create a List using the syntax List(1,2,3) instead of 1 :: 2 :: 3 :: Nil
  def apply[A](partitions: List[A]*)(implicit ec: ExecutionContext): ParList[A] =
    ParList(partitions.toList)

  // Creates a ParList by grouping a List into partitions of fixed size.
  // If the length of input list is not divisible by the partition size, then
  // the last partition will be smaller. For example:
  // byPartitionSize(3, List(1,2,3,4,5,6,7,8,9,10)) == ParList(
  //   List(1,2,3),
  //   List(4,5,6),
  //   List(7,8,9),
  //   List(10)
  // )
  def byPartitionSize[A](partitionSize: Int, items: List[A])(implicit ec: ExecutionContext): ParList[A] =
    if (items.isEmpty) ParList()
    else ParList(items.grouped(partitionSize).toList)
}
