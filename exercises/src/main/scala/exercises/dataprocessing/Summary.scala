package exercises.dataprocessing

import exercises.dataprocessing.MyMonoid._

case class Summary(
  min: Option[Sample], // Sample with lowest temperature
  max: Option[Sample], // Sample with highest temperature
  sum: Double, // sum of all temperatures in Fahrenheit
  size: Int // number of Samples
) {

  def average: Option[Double] =
    Option.unless(size == 0)(sum / size)

  override def toString: String =
    f"Summary(avg = ${average.getOrElse(0.0)}%.2f, " +
      s"size = $size,\n  " +
      s"min = $min,\n  " +
      s"max = $max\n)"
}

object Summary {
  implicit val summaryMonoid: MyMonoid[Summary] = new MyMonoid[Summary] {
    def combine(x: Summary, y: Summary): Summary = Summary(
      min = sampleMinMonoid.combine(x.min, y.min),
      max = sampleMaxMonoid.combine(x.max, y.max),
      sum = x.sum + y.sum,
      size = x.size + y.size
    )
    def empty: Summary = Summary(None, None, 0, 0)
  }
}
