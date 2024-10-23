package exercises.dataprocessing

import exercises.dataprocessing.ForLoopExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ForLoopExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("sum") {
    assert(sum(List(1, 5, 2)) == 8)
    assert(sum(Nil) == 0)
  }

  test("sum is consistent with List sum") {
    forAll { (numbers: List[Int]) =>
      assert(sum(numbers) == numbers.sum)
    }
  }

  test("size") {
    forAll { (ns: List[Int]) =>
      assert(size(ns) == ns.length)
    }
  }

  test("min") {
    forAll { (ns: List[Int]) =>
      assert(min(ns) == ns.minOption)
    }
  }

  test("wordCount") {
    assert(wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1))
    assert(wordCount(Nil) == Map.empty)
  }

  // test("foldLeft cares about order") {
  //   forAll { (l: List[Int]) =>
  //     assert(catamorphism(l)(List.empty[Int])((a: Int) => (b: List[Int]) => a :: b)
  //     == l.reverse)
  //   }
  // }

}
