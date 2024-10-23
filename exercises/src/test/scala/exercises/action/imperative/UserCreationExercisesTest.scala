package exercises.action.imperative

import java.time.{Instant, LocalDate}

import exercises.action.imperative.UserCreationExercises._
import exercises.action.DateGenerator._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Arbitrary

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}
import java.time.format.DateTimeFormatter

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.imperative.UserCreationExercisesTest
class UserCreationExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  val dateOfBirthFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("dd-MM-yyyy")

  test("parseYesNo") {
    assert(parseYesNo("Y") == true)
    assert(parseYesNo("N") == false)
    assertThrows[IllegalArgumentException](parseYesNo("Never"))
  }

  test("readSubscribeToMailingList example") {
    val inputs  = ListBuffer("N")
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(inputs, outputs)
    val result  = readSubscribeToMailingList(console)

    assert(result == false)
    assert(outputs.toList == List("Would you like to subscribe to our mailing list? [Y/N]"))
  }

  test("readSubscribeToMailingList valid input") {
    forAll { (input: Boolean) =>
      val inputs  = ListBuffer(if (input) "Y" else "N")
      val outputs = ListBuffer.empty[String]
      val console = Console.mock(inputs, outputs)
      val result  = readSubscribeToMailingList(console)

      assert(result == input)
      assert(outputs.toList == List("Would you like to subscribe to our mailing list? [Y/N]"))
    }
  }

  test("readSubscribeToMailingList example failure") {
    val console = Console.mock(ListBuffer("Never"), ListBuffer())
    val result  = Try(readSubscribeToMailingList(console))

    assert(result.isFailure)
  }

  val invalidStringGen = Gen.alphaStr.retryUntil(s => s != "Y" && s != "N")

  test("readSubscribeToMailingList invalid input") {
    forAll(invalidStringGen) { (input: String) =>
      val console = Console.mock(ListBuffer("Never"), ListBuffer())
      val result  = Try(readSubscribeToMailingList(console))

      assert(result.isFailure)
    }
  }

  test("readDateOfBirth example success") {
    val console = Console.mock(ListBuffer("21-07-1986"), ListBuffer())
    val result  = readDateOfBirth(console)

    assert(result == LocalDate.of(1986, 7, 21))
  }

  val dateGen = Gen
    .choose(LocalDate.of(1975, 1, 1).toEpochDay(), LocalDate.of(2050, 1, 1).toEpochDay())
    .map(LocalDate.ofEpochDay)

  test("readDateOfBirth success") {
    forAll(dateGen) { (date: LocalDate) =>
      val console = Console.mock(ListBuffer(date.format(dateOfBirthFormatter)), ListBuffer())
      val result  = readDateOfBirth(console)

      assert(result == date)
    }
  }

  test("readDateOfBirth example failure") {
    val console = Console.mock(ListBuffer("21/07/1986"), ListBuffer())
    val result  = Try(readDateOfBirth(console))

    assert(result.isFailure)
  }

  test("readUser example") {
    val inputs  = ListBuffer("Eda", "a", "b", "18-03-2001", "Y")
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(inputs, outputs)
    val clock   = Clock.constant(Instant.ofEpochSecond(1234))
    val result  = readUser(console, clock)

    val expected = User(
      name = "Eda",
      dateOfBirth = LocalDate.of(2001, 3, 18),
      subscribed = true,
      createdAt = clock.now()
    )

    assert(result == expected)
  }

  //////////////////////////////////////////////
  // PART 2: Error handling
  //////////////////////////////////////////////

  // test("readSubscribeToMailingListRetry negative maxAttempt") {
  //   val console = Console.mock(ListBuffer.empty[String], ListBuffer.empty[String])
  //   val result  = Try(readSubscribeToMailingListRetry(console, maxAttempt = -1))
  //
  //   assert(result.isFailure)
  // }

  test("readSubscribeToMailingListRetry negative maxAttempt") {
    forAll(Gen.choose(Int.MinValue, 0)) { (attempts: Int) =>
      val outputs = ListBuffer.empty[String]
      val console = Console.mock(ListBuffer.empty[String], outputs)
      val result  = Try(readSubscribeToMailingListRetry(console, maxAttempt = attempts))

      assert(result.isFailure)
      assert(outputs == ListBuffer.empty[String])
    }
  }

  test("readSubscribeToMailingListRetry attempts consistent with error messages") {
    forAll(Gen.choose(2, 1000), Gen.oneOf("Y", "N")) { (attempts: Int, answer: String) =>
      val outputs = ListBuffer.empty[String]
      val inputs  = ListBuffer.fill(attempts - 1)("Never") :+ answer
      val console = Console.mock(inputs, outputs)
      val result  = readSubscribeToMailingListRetry(console, attempts)
      val expectedOutput = List
        .fill(attempts - 1)(
          List(
            "Would you like to subscribe to our mailing list? [Y/N]",
            """Incorrect format, enter "Y" for Yes or "N" for "No""""
          )
        )
        .flatten :+ "Would you like to subscribe to our mailing list? [Y/N]"

      assert(result == parseYesNo(answer))
      assert(outputs.toList == expectedOutput)
    }
  }

  test("readSubscribeToMailingListRetry example success") {
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(ListBuffer("Never", "N"), outputs)
    val result  = readSubscribeToMailingListRetry(console, maxAttempt = 2)

    assert(result == false)
    assert(
      outputs.toList == List(
        "Would you like to subscribe to our mailing list? [Y/N]",
        """Incorrect format, enter "Y" for Yes or "N" for "No"""",
        "Would you like to subscribe to our mailing list? [Y/N]"
      )
    )
  }

  test("readSubscribeToMailingListRetry example invalid input") {
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(ListBuffer("Never"), outputs)
    val result  = Try(readSubscribeToMailingListRetry(console, maxAttempt = 1))

    assert(result.isFailure)
    assert(
      outputs.toList == List(
        "Would you like to subscribe to our mailing list? [Y/N]",
        """Incorrect format, enter "Y" for Yes or "N" for "No""""
      )
    )

    // check that the error message is the same as `readSubscribeToMailingList`
    val console2 = Console.mock(ListBuffer("Never"), ListBuffer.empty[String])
    val result2  = Try(readSubscribeToMailingList(console2))
    assert(result.failed.get.getMessage == result2.failed.get.getMessage)
  }

  test("readDateOfBirthRetry negative maxAttempt") {
    val console = Console.mock(ListBuffer.empty[String], ListBuffer.empty[String])
    val result  = Try(readSubscribeToMailingListRetry(console, maxAttempt = -1))

    assert(result.isFailure)
  }

  test("readDateOfBirthRetry example success") {
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(ListBuffer("July 21st 1986", "21-07-1986"), outputs)
    val result  = readDateOfBirthRetry(console, maxAttempt = 2)

    assert(result == LocalDate.of(1986, 7, 21))
    assert(
      outputs.toList == List(
        """What's your date of birth? [dd-mm-yyyy]""",
        """Incorrect format, for example enter "18-03-2001" for 18th of March 2001""",
        """What's your date of birth? [dd-mm-yyyy]"""
      )
    )
  }

  test("readDateOfBirthRetry example failure") {
    val outputs        = ListBuffer.empty[String]
    val invalidAttempt = "July 21st 1986"
    val console        = Console.mock(ListBuffer(invalidAttempt), outputs)
    val result         = Try(readDateOfBirthRetry(console, maxAttempt = 1))

    assert(result.isFailure)
    assert(
      outputs.toList == List(
        """What's your date of birth? [dd-mm-yyyy]""",
        """Incorrect format, for example enter "18-03-2001" for 18th of March 2001"""
      )
    )
  }

}
