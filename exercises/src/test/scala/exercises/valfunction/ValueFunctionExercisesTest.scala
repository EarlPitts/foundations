package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  // replace `ignore` by `test` to enable the test
  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  // replace `ignore` by `test` to enable the test
  test("selectDigits length is smaller") {
    forAll { (text: String) =>
      assert(selectDigits(text).length <= text.length)
    }
  }

  test("selectDigits leaves only digits") {
    forAll { (text: String) =>
      assert(selectDigits(text.filterNot(_.isDigit)) == "")
    }
  }

  test("selectDigits really leaves only digits") {
    forAll { (text: String) =>
      assert(selectDigits(text).foldLeft(true)((b, c) => b && c.isDigit))
    }
  }

  test("secret output has the same length") {
    forAll { (text: String) =>
      assert(secret(text).length == text.length)
    }
  }

  test("secret output is idempotent") {
    def nTimes[A](n: Int)(f: A => A)(a: A): A =
      if (n == 0) a else nTimes(n - 1)(f)(f(a))

    forAll { (text: String) =>
      assert(secret(text) == secret(secret(text)))
    }
  }

  test("isValidusername filter out invalid") {
    forAll { (text: String) =>
      assert(!isValidUsername(text ++ "*"))
    }
  }

  test("isValidusername reversing doesn't matter") {
    forAll { (text: String) =>
      assert(isValidUsername(text) == isValidUsername(text.reverse))
    }
  }

  ///////////////////////
  // Exercise 2: Point
  ///////////////////////

  test("isPositive returns true for absolute values") {
    forAll { (x: Int, y: Int, z: Int) =>
      val p = Point(x.max(0), y.max(0), z.max(0))
      assert(p.isPositive)
    }
  }

  test("isEven works") {
    forAll { (x: Int, y: Int, z: Int) =>
      val p = Point(x * 2, y * 2, z * 2)
      assert(p.isEven)
    }
  }

}
