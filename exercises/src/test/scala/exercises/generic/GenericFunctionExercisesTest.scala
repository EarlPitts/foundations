package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import exercises.generic.GenericFunctionExercises._
import exercises.generic.GenericFunctionExercises.Predicate._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Try

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    forAll { (a: Int, b: Int) =>
      assert(Pair(a, b).swap == Pair(b, a))
      assert(Pair(a, b).swap.swap == Pair(a, b))
    }
  }

  test("Pair map") {
    forAll { (x: String, y: String) =>
      val p = Pair(x, y)
      assert(p.bimap(identity) == p)
    }
  }

  test("Pair decoded") {
    assert(decoded == Pair("Functional", "Programming"))
  }

  test("Pair zipWith") {
    forAll { (x: Int, y: Int) =>
      val p = Pair(x, y)
      val q = Pair(0, 0)
      assert(p.zipWith(q)(_ + _) == p)
    }
  }

  test("Pair productNames") {
    assert(products == Pair(Product("Coffee", 2.5), Product("Plane ticket", 329.99)))
  }

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  test("Predicate &&") {
    forAll { (f: Int => Boolean, x: Int) =>
      val p = Predicate(f)
      val n = (x * 2).max(0)

      assert((isEven && isPositive)(n))
      assert((p && False)(x) == false)
      assert((p && True)(x) == p(x))
      assert((True && True)(x) == True(x))
    }
  }

  test("Predicate ||") {
    assert((isEven || isPositive)(12) == true)
    assert((isEven || isPositive)(11) == true)
    assert((isEven || isPositive)(-4) == true)
    assert((isEven || isPositive)(-7) == false)
  }

  test("Predicate flip") {
    assert(isEven.flip(11) == true)
  }

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {
    forAll { (n: Int) =>
      val valid = n.toString
      val invalid = "a" ++ valid ++ "b"

      assert(userIdDecoder.decode(valid) == Option(UserId(n)))
      assert(userIdDecoder.decode(invalid) == None)
    }
  }

  test("JsonDecoder LocalDate") {
    assert(localDateDecoder.decode("\"2020-03-26\"") == Option(LocalDate.of(2020,3,26)))
    assert(localDateDecoder.decode("2020-03-26") == None)
    assert(localDateDecoder.decode("hello") == None)
  }

  implicit val numGen: Arbitrary[Int] = 
    Arbitrary(Gen.chooseNum(0,10000))

  implicit val dateGen: Arbitrary[LocalDate] = 
    Arbitrary(Gen.chooseNum(0,10000).map(LocalDate.ofEpochDay(_)))

  test("JsonDecoder LocalDate round-trip") {

    forAll { (date: LocalDate) =>
      val dateStr = "\"" ++ date.toString ++ "\""
      assert(localDateDecoder.decode(dateStr) == Option(date))
    }
  }

  test("JsonDecoder weirdLocalDateDecoder") {
    forAll { (n: Int) =>
      val date = LocalDate.ofEpochDay(n)
      val dateStr = "\"" ++ date.toString ++ "\""

      assert(weirdLocalDateDecoder.decode(dateStr)    == Option(date))
      assert(weirdLocalDateDecoder.decode(n.toString) == Option(date))
      assert(weirdLocalDateDecoder.decode("hello")    == None)
    }
  }

}
