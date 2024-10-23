package exercises.dataprocessing

import cats._
import cats.implicits._
import scala.concurrent.ExecutionContext

object TemperatureExercises {
  // b. Implement `minSampleByTemperature` which finds the `Sample` with the coldest temperature.
  // `minSampleByTemperature` should work as follow:
  // Step 1: Find the local minimums (for each partition the `Sample` with the coldest temperature).
  // Step 2: Find the minimum value among the local minimums.
  // Note: We'll write test in the file `ParListTest.scala`
  def minSampleByTemperature(samples: ParList[Sample])(implicit ec: ExecutionContext): Option[Sample] =
    samples.parFoldMap(Option(_))
  // samples.partitions
  //   .flatMap(_.minByOption(_.temperatureCelsius))
  //   .minByOption(_.temperatureCelsius)

  // c. Implement `averageTemperature` which finds the average temperature across all `Samples`.
  // `averageTemperature` should work as follow:
  // Step 1: Compute the sum of all samples temperatures
  //   a) Compute the sum per partition
  //   b) Sum-up the sum of each partition
  // Step 2: Compute the size of the dataset
  //   a) Compute the size of each partition
  //   b) Sum-up the size of each partition
  // Step 3: Divide the total temperature by the size of dataset.
  // In case the input `ParList` is empty we return `None`.
  // Bonus: Can you calculate the size and sum in one go?
  def averageTemperature(samples: ParList[Sample])(implicit ec: ExecutionContext): Option[Double] = {
    // val (sum,size) = samples.partitions.foldLeft((0.0,0)) { case ((sum, size), p) =>
    //   (sum + p.map(_.temperatureFahrenheit).sum, size + p.size)
    // }
    // val size = samples.as(1).monoFoldLeft
    // val size = samples.size
    // val sum  = samples.map(s => s.temperatureFahrenheit).monoFoldLeft
    val (size, sum) = samples.parFoldMap(s => (1, s.temperatureFahrenheit))

    if (samples.partitions.isEmpty) None else Option(sum / size)
  }

  // {
  //   val sum = samples.partitions.map(_.map(_.temperatureFahrenheit).sum).sum
  //   val size = samples.partitions.map(_.size).sum
  //   if (samples.partitions.isEmpty) None else Option(sum / size)
  // }

  // d. Implement `foldLeft` and then move it inside the class `ParList`.
  // `foldLeft` should work as follow:
  // Step 1: Fold each partition into a single value.
  // Step 2: Fold the intermediate results of all partitions together.
  // For example,
  // Partition 1: List(a1, b1, c1, d1, e1, f1) ->    res1 (intermediate result of partition 1) \
  // Partition 2: List(a2, b2, c2, d2, e2, f2) ->    res2 (intermediate result of partition 2) - finalResult
  // Partition 3:                          Nil -> default (partition 3 is empty)               /
  // def foldLeft[A, B](parList: ParList[A], z: B)(f: (B, A) => B): B =
  //   parList.partitions.map(_.foldLeft(z)(f))

  // e. Implement `monoFoldLeft`, a version of `foldLeft` that does not change the element type.
  // Then move `monoFoldLeft` inside  the class `ParList`.
  // `monoFoldLeft` should work as follow:
  // Step 1: Fold each partition into a single value.
  // Step 2: Fold the results of all partitions together.
  // For example,
  // Partition 1: List(a1, b1, c1, d1, e1, f1) ->       x   (folded partition 1)  \
  // Partition 2: List(a2, b2, c2, d2, e2, f2) ->       y   (folded partition 2) - z (final result)
  // Partition 3:                          Nil -> default (partition 3 is empty)  /
  def monoFoldLeft[A](parList: ParList[A], z: A)(f: (A, A) => A): A =
    parList.partitions.map(_.foldLeft(z)(f)).foldLeft(z)(f)

  // `summaryList` iterate 4 times over `samples`, one for each field.
  def summaryList(samples: List[Sample]): Summary =
    Summary(
      min = samples.minByOption(_.temperatureFahrenheit),
      max = samples.maxByOption(_.temperatureFahrenheit),
      sum = samples.foldLeft(0.0)((state, sample) => state + sample.temperatureFahrenheit),
      size = samples.size
    )

  def summaryListOnePass(samples: List[Sample]): Summary =
    samples.foldLeft(
      Summary(
        min = None,
        max = None,
        sum = 0.0,
        size = 0
      )
    )((state, sample) =>
      Summary(
        min = state.min.fold(Some(sample))(current =>
          if (current.temperatureFahrenheit <= sample.temperatureFahrenheit)
            Some(current)
          else Some(sample)
        ),
        max = state.max.fold(Some(sample))(current =>
          if (current.temperatureFahrenheit >= sample.temperatureFahrenheit)
            Some(current)
          else Some(sample)
        ),
        sum = state.sum + sample.temperatureFahrenheit,
        size = state.size + 1
      )
    )

  // Implement `summaryParList` by calling `parFoldMap` once for each field of Summary.
  // Note: In `ParListTest.scala`, there is already a test checking that `summaryParList`
  // should return the same result as `summaryList`
  def summaryParList(samples: ParList[Sample]): Summary =
    Summary(
      min = samples.parFoldMap(Option(_)),
      max = samples.parFoldMap(Option(_))(MyMonoid.sampleMaxMonoid),
      sum = samples.parFoldMap(_.temperatureFahrenheit),
      size = samples.size
    )

  // Implement `summaryParListOnePass` using `parFoldMap` only ONCE.
  // Note: In `ParListTest.scala`, there is already a test checking that `summaryParListOnePass`
  // should return the same result as `summaryList`
  def summaryParListOnePass(samples: ParList[Sample]): Summary =
    samples.parFoldMap((s: Sample) =>
      Summary(
        min = Option(s),
        max = Option(s),
        sum = s.temperatureFahrenheit,
        size = 1
      )
    )

  def aggregateBy[A](samples: ParList[Sample])(f: Sample => A): Map[A, Summary] =
    samples.parFoldMap((s: Sample) =>
        Map(f(s) -> Summary(
        min = Option(s),
        max = Option(s),
        sum = s.temperatureFahrenheit,
        size = 1
      ))
    )
}
