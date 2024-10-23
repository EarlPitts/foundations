package exercises.dataprocessing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import TemperatureExercises._
import exercises.dataprocessing.MyMonoid

import cats.implicits._
import org.scalacheck.Arbitrary

class ParListTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with ParListTestInstances {

  test("minSampleByTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(
      minSampleByTemperature(parSamples) ==
        Some(Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1))
    )
  }

  test("minSampleByTemperature returns the coldest Sample") {
    forAll { (samples: List[Sample]) =>
      val parSamples = ParList.byPartitionSize(3, samples)

      for {
        coldest <- minSampleByTemperature(parSamples)
        sample  <- samples
      } assert(coldest.temperatureFahrenheit <= sample.temperatureFahrenheit)
    }
  }

  test("minSampleByTemperature consistent") {
    forAll { (samples: List[Sample]) =>
      val parSamples = ParList.byPartitionSize(3, samples)

      assert(
        minSampleByTemperature(parSamples) == samples.minByOption(
          _.temperatureFahrenheit
        )
      )
    }
  }

  test("averageTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(averageTemperature(parSamples) == Some(53.6))
  }

  test("averageTemperature consistent") {
    forAll { (parSamples: ParList[Sample], multiplier: Int) =>
      val multipliedSamples = parSamples.map(s => s.copy(temperatureFahrenheit = s.temperatureFahrenheit * multiplier))

      assert(
        averageTemperature(parSamples).map(_ * multiplier round)
          == averageTemperature(multipliedSamples).map(_ round)
      )
    }
  }

  test("averageTemperature consistent again") {
    forAll { (parSamples: ParList[Sample]) =>
      averageTemperature(parSamples) match {
        case None => assert(parSamples.toList.isEmpty)
        case Some(avg) =>
          val newSamples =
            parSamples.map(sample => sample.copy(temperatureFahrenheit = sample.temperatureFahrenheit - avg))
          averageTemperature(newSamples) match {
            case None       => fail("not possible")
            case Some(avg2) => assert((avg2 round) == 0)
          }
      }
    }
  }

  test("monoFoldLeft consistent") {
    forAll { (parSamples: ParList[Int]) =>
      assert(parSamples.monoFoldLeft == parSamples.toList.sum)
    }
  }

  def checkMonoidInstance[A: MyMonoid: Arbitrary](name: String) = {
    test(s"$name satisfies the identity laws") {
      forAll { (x: A) =>
        assert(MyMonoid[A].combine(x, MyMonoid[A].empty) == x)
        assert(MyMonoid[A].combine(MyMonoid[A].empty, x) == x)
      }
    }

    test(s"$name satisfies the associativity law") {
      forAll { (x: A, y: A, z: A) =>
        assert(
          MyMonoid[A].combine(MyMonoid[A].combine(x, y), z) ==
            MyMonoid[A].combine(x, MyMonoid[A].combine(y, z))
        )
      }
    }
  }

  checkMonoidInstance[Int]("intWithAddition")
  checkMonoidInstance[Double]("doubleWithAddition")
  checkMonoidInstance[Option[Sample]]("Option[Sample] with minimum")
  checkMonoidInstance[Summary]("summaryMonoid")
  checkMonoidInstance[Map[String, Summary]]("mapMonoid")

  test(s"foldMap idenitity is consistent with monoFoldLeft") {
    forAll { (pl: ParList[Int]) =>
      assert(pl.foldMap(identity) == pl.monoFoldLeft)
    }
  }

   implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  test(s"parFoldMap is consistent with foldMap") {
    forAll { (pl: ParList[Int]) =>
      assert(pl.parFoldMap(identity) == pl.foldMap(identity))
    }
  }

  test("summary is consistent between implementations") {
    forAll { (samples: ParList[Sample]) =>
      val samplesList = samples.partitions.flatten
      val reference   = summaryList(samples.partitions.flatten)
      List(
        summaryListOnePass(samplesList),
        summaryParList(samples),
        summaryParListOnePass(samples)
      ).foreach { other =>
        assert(reference.size == other.size)
        assert((reference.sum - other.sum).abs < 0.00001)
        assert(reference.min == other.min)
        assert(reference.max == other.max)
      }
    }
  }
}
