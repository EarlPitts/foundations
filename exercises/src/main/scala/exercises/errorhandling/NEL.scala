package exercises.errorhandling

import cats._
import cats.implicits._

// Non-Empty List
// It is called `NonEmptyList` in cats and zio-prelude.
// There is not an exact equivalent to Nel in the standard library.
// The closest is `::`, the cons constructor of List.
case class NEL[+A](head: A, tail: List[A]) {
  def toList: List[A] =
    head :: tail

  def map[Next](update: A => Next): NEL[Next] =
    NEL(update(head), tail.map(update))

  // concat
  def ++[Other >: A](other: NEL[Other]): NEL[Other] =
    NEL(head, tail ++ other.toList)

  // prepend
  def +:[Other >: A](other: Other): NEL[Other] =
    NEL(other, head +: tail)

  // append
  def :+[Other >: A](other: Other): NEL[Other] =
    NEL(head, tail :+ other)
}

object NEL {
  def apply[A](head: A, tail: A*): NEL[A] =
    NEL(head, tail.toList)

  def one[A](value: A): NEL[A] =
    NEL(value)

  def fromList[A](values: List[A]): Option[NEL[A]] =
    values match {
      case Nil          => None
      case head :: next => Some(NEL(head, next))
    }

  implicit val nelTraversable: Traverse[NEL] = new Traverse[NEL] {
    def foldLeft[A, B](fa: NEL[A], b: B)(f: (B, A) => B): B = {
      def go(fa: NEL[A], acc: B): B = fa.tail match {
        case head :: tail => go(NEL(head,tail), f(acc, fa.head))
        case Nil          => f(acc, fa.head)
      }
      go(fa, b)
    }
    def foldRight[A, B](fa: NEL[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.tail match {
      case head :: tail => f(fa.head, foldRight(NEL(head, tail), lb)(f))
      case Nil          => f(fa.head, lb)
    }
    def traverse[G[_]: Applicative, A, B](fa: NEL[A])(f: A => G[B]): G[NEL[B]] = fa.tail match {
      case a :: as => Applicative[G].map2(f(fa.head), traverse(NEL(a,as))(f))(_ +: _)
      case Nil => f(fa.head).map(NEL(_))
    }
  }
}
