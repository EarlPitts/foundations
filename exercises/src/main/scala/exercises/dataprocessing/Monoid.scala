package exercises.dataprocessing

import cats._

trait MyMonoid[A] {
  def empty: A
  def combine(x: A, y: A): A
}

object MyMonoid {
  def apply[A](implicit instance: MyMonoid[A]) = instance

  implicit val intAdditionMonoid: MyMonoid[Int] = new MyMonoid[Int] {
    def combine(x: Int, y: Int): Int = x + y
    def empty: Int                   = 0
  }

  implicit val doubleAdditionMonoid: MyMonoid[Double] = new MyMonoid[Double] {
    def combine(x: Double, y: Double): Double = x + y
    def empty: Double                         = 0.0
  }

  implicit val doubleMinMonoid: MyMonoid[Option[Double]] = new MyMonoid[Option[Double]] {
    def combine(x: Option[Double], y: Option[Double]): Option[Double] = (x, y) match {
      case (Some(x), Some(y)) => Some(x min y)
      case _                  => None
    }
    def empty: Option[Double] = None
  }

  val doubleMaxMonoid: MyMonoid[Option[Double]] = new MyMonoid[Option[Double]] {
    def combine(x: Option[Double], y: Option[Double]): Option[Double] = (x, y) match {
      case (Some(x), Some(y)) => Some(x max y)
      case _                  => None
    }
    def empty: Option[Double] = None
  }

  val orderAscending: Order[Sample] = new Order[Sample] {
    def compare(x: Sample, y: Sample): Int =
      if (x.temperatureFahrenheit < y.temperatureFahrenheit) -1
      else if (x.temperatureFahrenheit > y.temperatureFahrenheit) 1
      else 0
  }

  implicit def mapMonoid[A, B: MyMonoid]: MyMonoid[Map[A, B]] = new MyMonoid[Map[A, B]] {
    def empty: Map[A, B] = Map.empty[A, B]
    def combine(x: Map[A, B], y: Map[A, B]): Map[A, B] =
      y.foldLeft(x) { case (acc, (key, value)) =>
        if (acc.contains(key))
          acc.updated(key, MyMonoid[B].combine(acc(key), value))
        acc + (key -> value)
      }
    // (x.keySet ++ y.keySet).foldLeft(Map.empty[A, B]) { case (acc, key) =>
    //   (x.contains(key), y.contains(key)) match {
    //     case (true, true)  => acc + (key -> MyMonoid[B].combine(x(key), y(key)))
    //     case (true, false) => acc + (key -> x(key))
    //     case (false, true) => acc + (key -> y(key))
    //     case _             => throw new Exception("not possible")
    //   }
    // }
  }

  def compareSamples(comp: (Sample, Sample) => Sample): MyMonoid[Option[Sample]] = new MyMonoid[Option[Sample]] {
    def combine(x: Option[Sample], y: Option[Sample]): Option[Sample] = (x, y) match {
      case (Some(x), Some(y)) => Some(comp(x, y))
      case (Some(x), None)    => Some(x)
      case (None, Some(y))    => Some(y)
      case _                  => None
    }
    def empty: Option[Sample] = None
  }

  implicit val sampleMinMonoid: MyMonoid[Option[Sample]] =
    compareSamples((s1, s2) =>
      if (s1.temperatureFahrenheit < s2.temperatureFahrenheit) s1
      else s2
    )

  val sampleMaxMonoid: MyMonoid[Option[Sample]] =
    compareSamples((s1, s2) =>
      if (s1.temperatureFahrenheit > s2.temperatureFahrenheit) s1
      else s2
    )

  implicit def tupleMonoid[A: MyMonoid, B: MyMonoid]: MyMonoid[(A, B)] = new MyMonoid[(A, B)] {
    def combine(x: (A, B), y: (A, B)): (A, B) = (MyMonoid[A].combine(x._1, y._1), MyMonoid[B].combine(x._2, y._2))
    def empty: (A, B)                         = (MyMonoid[A].empty, MyMonoid[B].empty)
  }
}
