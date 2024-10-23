package exercises.errorhandling.either
import exercises.errorhandling.either.EitherExercises2.Country._
import exercises.errorhandling.either.EitherExercises2.CountryError._
import exercises.errorhandling.either.EitherExercises2.UsernameError._
import exercises.errorhandling.either.EitherExercises2._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck._
import org.scalacheck.Arbitrary._

class EitherExercises2Test extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("validateCountry example") {
    assert(validateCountry("FRA") == Right(France))
    assert(validateCountry("UK") == Left(InvalidFormat("UK")))
    assert(validateCountry("ARG") == Left(NotSupported("ARG")))
  }

  test("checkUsernameSize example") {
    assert(checkUsernameSize("bob_2167") == Right(()))
    assert(checkUsernameSize("bob_2") == Right(()))
    assert(checkUsernameSize("bo") == Left(TooSmall(2)))
  }

  test("checkUsernameCharacters example") {
    assert(checkUsernameCharacters("_abc-123_") == Right(()))
    assert(checkUsernameCharacters("foo!~23}AD") == Left(InvalidCharacters(List('!', '~', '}'))))
  }

  test("validateUsername example") {
    assert(validateUsername("bob_2167") == Right(Username("bob_2167")))
    assert(validateUsername("bo") == Left(TooSmall(2)))
    assert(validateUsername("foo!~23}AD") == Left(InvalidCharacters(List('!', '~', '}'))))
  }

  val validCountryCodeGen: Gen[String] =
    Gen.oneOf(Country.all.map(_.code))

  val invalidCountryCodeGen: Gen[String] =
    arbitrary[String].filter(_ != Country.all.map(_.code))

  val validUsernameStrGen: Gen[String] =
    Gen.stringOf(Gen.alphaChar).suchThat(_.length >= 5)

  val invalidUsernameStrGen: Gen[String] =
    arbitrary[String].filter(str => str.length < 5 || !str.forall(isValidUsernameCharacter))

  test("validateUser valid example") {
    forAll(validUsernameStrGen, validCountryCodeGen) { (username: String, country: String) =>
      assert(validateUser(username, country).isRight)
    }
  }

  test("validateUser invalid username") {
    forAll(invalidUsernameStrGen, validCountryCodeGen) { (username: String, country: String) =>
      assert(validateUser(username, country).isLeft)
    }
  }

  test("validateUser invalid country") {
    forAll(validUsernameStrGen, invalidCountryCodeGen) { (username: String, country: String) =>
      assert(validateUser(username, country).isLeft)
    }
  }

}
