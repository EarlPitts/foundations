package toimpl.typeclass

import cats.data.NonEmptyList
import cats.kernel.Order
import exercises.typeclass._

trait TypeclassToImpl {
  implicit val intMonoid: Monoid[Int]
  implicit val doubleMonoid: Monoid[Double]
  implicit val stringMonoid: Monoid[String]
  implicit val unitMonoid: Monoid[Unit]
  implicit def listMonoid[A]: Monoid[List[A]]
  implicit def vectorMonoid[A]: Monoid[Vector[A]]
  implicit def setMonoid[A]: Monoid[Set[A]]
  implicit val intAndStringMonoid: Monoid[(Int, String)]
  implicit def tuple2Monoid[A: Monoid, B: Monoid]: Monoid[(A, B)]
  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]]
  implicit def mapMonoid[K, A: Semigroup]: Monoid[Map[K, A]]

  def fold[A](fa: List[A])(implicit ev: Monoid[A]): A
  def sum(xs: List[Int]): Int
  def averageWordLength(xs: List[String]): Double
  def isEmpty[A: Monoid](x: A): Boolean
  def ifEmpty[A: Monoid](x: A)(other: => A): A
  def repeat[A: Monoid](n: Int)(x: A): A
  def intercalate[A](xs: List[A], before: A, between: A, after: A)(implicit ev: Monoid[A]): A
  def intercalate[A](xs: List[A], between: A)(implicit ev: Monoid[A]): A
  def scsvFormat(xs: List[String]): String
  def tupleFormat(xs: List[String]): String
  def foldMap[A, B](xs: List[A])(f: A => B)(implicit ev: Monoid[B]): B
  def charSequence(xs: List[String]): List[Char]
  def fold2[A](xs: List[A])(implicit ev: Monoid[A]): A

  val stringSpaceMonoid: Monoid[String]
  def splitFold[A: Monoid](xs: List[A])(split: List[A] => List[List[A]]): A
  val productIntMonoid: Monoid[Int]
  val booleanMonoid: Monoid[Boolean]
  implicit val productMonoid: Monoid[Product]
  def product(xs: List[Int]): Int
  implicit val allMonoid: Monoid[All]
  def forAll(xs: List[Boolean]): Boolean
  implicit def endoMonoid[A]: Monoid[Endo[A]]
  def pipe[A](xs: List[A => A]): A => A

  implicit def nelSemigroup[A]: Semigroup[NonEmptyList[A]]
  def reduceMap[A, B: Semigroup](fa: List[A])(f: A => B): Option[B]
  implicit def minSemigroup[A: Ordering]: Semigroup[Min[A]]
  def minOption[A: Ordering](xs: List[A]): Option[A]
  implicit def firstSemigroup[A]: Semigroup[First[A]]
  def headOption[A](xs: List[A]): Option[A]
  implicit def dualSemigroup[A: Semigroup]: Semigroup[Dual[A]]
  def lastOption[A: Ordering](xs: List[A]): Option[A]

  def foldMap[A, B](xs: Vector[A])(f: A => B)(implicit ev: Monoid[B]): B
  def foldMap[A, B](xs: Option[A])(f: A => B)(implicit ev: Monoid[B]): B
  def foldMap[E, A, B](xs: Either[E, A])(f: A => B)(implicit ev: Monoid[B]): B
  def foldMap[K, A, B](xs: Map[K, A])(f: A => B)(implicit ev: Monoid[B]): B
  implicit val listFoldable: Foldable[List]
  implicit val optionFoldable: Foldable[Option]
  implicit def eitherFoldable[E]: Foldable[Either[E, ?]]
  implicit def mapFoldable[K]: Foldable[Map[K, ?]]
  def isEmpty[F[_]: Foldable, A](fa: F[A]): Boolean
  def size[F[_]: Foldable, A](fa: F[A]): Int
  def headOption[F[_]: Foldable, A](fa: F[A]): Option[A]
  def find[F[_]: Foldable, A](fa: F[A]): Option[A]
  def minimumOption[F[_]: Foldable, A: Ordering](fa: F[A]): Option[A]
  def lookup[F[_]: Foldable, A](fa: F[A], index: Int): Option[A]
}
