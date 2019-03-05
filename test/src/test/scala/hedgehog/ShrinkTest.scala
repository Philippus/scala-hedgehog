package hedgehog

import hedgehog.runner._

object ShrinkTest extends Properties {

  def tests: List[Test] =
    List(
      property("testTowardsFloat is always in range", testTowardsFloat)
    , example("testTowardsFloat is always in range", testTowardsFloatExample(Double.MinPositiveValue, 0.1))
    )

  def testTowardsFloat: Property =
    for {
      d <- Gen.double(Range.linearFracFrom(0, Double.MinValue, Double.MaxValue)).log("d")
      x <- Gen.double(Range.linearFracFrom(0, Double.MinValue, Double.MaxValue)).log("x")
    } yield testTowardsFloatExample(d, x)

  def testTowardsFloatExample(d: Double, x: Double): Result = {
    val fs = core.Shrink.towardsFloat(d, x)
    Result.all(fs.map(f =>
      Result.assert(if (d < x) d <= f && f <= x else x <= f && f <= d).log(f.toString)
    ))
  }
}
