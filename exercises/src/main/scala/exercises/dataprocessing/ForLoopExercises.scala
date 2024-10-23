package exercises.dataprocessing

import cats._
import cats.implicits._

object ForLoopExercises {

  def sum(numbers: List[Int]): Int = {
    // var total = 0
    //
    // for (number <- numbers)
    //   total += number
    //
    // total
    catamorphism(numbers)(0)((n: Int) => (m: Int) => n + m)
  }

  // a. Implement `size` using a mutable state and a for loop
  // such as size(List(2,5,1,8)) == 4
  // and     size(Nil) == 0
  def size[A](items: List[A]): Int = {
    // var length = 0
    // for (_ <- items)
    //   length += 1
    // length
    catamorphism(items)(0)(_ => (m: Int) => 1 + m)
  }
    

  // b. Implement `min` using a mutable state and a for loop
  // such as min(List(2,5,1,8)) == Some(1)
  // and     min(Nil) == None
  // Note: Option is an enumeration with two values:
  // * Some when there is a value and
  // * None when there is no value (a bit like null)
  def min(numbers: List[Int]): Option[Int] = {
    // var min = Option.empty[Int]
    // 
    // for (n <- numbers) {
    //   if (min.isEmpty) min = Option(n) else {
    //     if (min.get > n) min = Option(n)
    //   }
    // }
    // min
    catamorphism(numbers)(Option.empty[Int])((n: Int) => (m: Option[Int]) => Option(m.fold(n)(cur => cur.min(n))))
  }

  // c. Implement `wordCount` using a mutable state and a for loop.
  // `wordCount` compute how many times each word appears in a `List`
  // such as wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1)
  // and     wordCount(Nil) == Map.empty
  // Note: You can lookup an element in a `Map` with the method `get`
  // and you can upsert a value using `updated`
  def wordCount(words: List[String]): Map[String, Int] = {
    // var wordMap = Map.empty[String, Int]

    // for (word <- words) {
    //   wordMap =
    //     wordMap
    //       .get(word)
    //       .fold(wordMap.updated(word, 1))(n => wordMap.updated(word, n + 1))
    // }
    //
    // wordMap
    catamorphism(words)(Map.empty[String, Int])((word: String) => (m: Map[String, Int]) =>
        m.get(word).fold(m.updated(word, 1))(num => m.updated(word, num + 1)))
  }
    
  // d. `sum`, `size`, `min` and `wordCount` are quite similar.
  // Could you write a higher-order function that captures this pattern?
  // How would you call it?
  def catamorphism[A, B](l: List[A])(z: B)(f: A => B => B): B = l match {
    case Nil => z
    case x :: xs => f(x)(catamorphism(xs)(z)(f))
  }

  // e. Refactor `sum`, `size`, `min` and `wordCount` using the higher-order
  // function you defined above.

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // f. `foldLeft` can be used to implement most of the List API.
  // Do you want to give it a try? For example, can you implement
  // `map`, `reverse` and `lastOption` in terms of `foldLeft`
  def map[From, To](elements: List[From])(update: From => To): List[To] =
    elements.foldl(List.empty[To])(((l: List[To]), (elem: From)) => update(elem) :: l)

  // reverse(List(3,8,1)) == List(1,8,3)
  // reverse(Nil) == Nil
  def reverse[A](elements: List[A]): List[A] =
    elements.foldl(List.empty[A])((as, a) => a :: as)

  // lastOption(List(3,8,1)) == Some(1)
  // lastOption(Nil) == None
  def lastOption[A](elements: List[A]): Option[A] =
    elements.foldl(Option.empty[A])((_, a) => Option(a))

  // g. Can you generalise `min` so that it applies to more types like `Long`, `String`, ...?
  // Note: You may want to use the class Ordering from the standard library
  def generalMin[A: Ordering](elements: List[A]): Option[A] =
    elements.foldl(Option.empty[A])((min, a) =>
      Option(min.fold(a)(cur => if (Ordering[A].gt(cur,a)) a else cur))
    )

}
