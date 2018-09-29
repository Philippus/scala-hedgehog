package hedgehog

import hedgehog.core._
import hedgehog.predef._

trait GenTOps[M[_]] {

  /**********************************************************************/
  // Combinators

  /**
   * Runs a `Option`` generator until it produces a `Some`.
   *
   * This is implemented using `filter` and has the same caveats.
   */
  def fromSome[A](gen: GenT[M, Option[A]])(implicit F: Monad[M]): GenT[M, A] =
    gen
      .filter(_.isDefined)
      .map(x => x.getOrElse(sys.error("fromSome: internal error, unexpected None")))

  def lift[A](m: M[A])(implicit F: Functor[M]): GenT[M, A] =
    GenT((_, s) => Tree.lift(F.map(m)(a => (s, Some(a)))))

  /**
   * Construct a generator that depends on the size parameter.
   */
  def generate[A](f: (Size, Seed) => (Seed, A))(implicit F: Monad[M]): GenT[M, A] =
    GenT((size, seed) => {
      val (s2, a) = f(size, seed)
      Tree.TreeApplicative.point((s2, some(a)))
    })

  /**********************************************************************/
  // Combinators - Size

  /**
   * Construct a generator that depends on the size parameter.
   */
  def sized[A](f: Size => GenT[M, A]): GenT[M, A] =
    GenT((size, seed) => f(size).run(size, seed))

  /**********************************************************************/
  // Combinators - Integral

  /**
   * Generates a random integral number in the given `[inclusive,inclusive]` range.
   *
   * When the generator tries to shrink, it will shrink towards the
   * [[Range.origin]] of the specified [[Range]].
   *
   * For example, the following generator will produce a number between `1970`
   * and `2100`, but will shrink towards `2000`:
   *
   * {{{
   * Gen.integral(Range.constantFrom(2000, 1970, 2100))
   * }}}
   *
   * Some sample outputs from this generator might look like:
   *
   * {{{
   * === Outcome ===
   * 1973
   * === Shrinks ===
   * 2000
   * 1987
   * 1980
   * 1976
   * 1974
   *
   * === Outcome ===
   * 2061
   * === Shrinks ===
   * 2000
   * 2031
   * 2046
   * 2054
   * 2058
   * 2060
   * }}}
   */
  def integral[A : Integral](range: Range[A])(implicit F: Monad[M]): GenT[M, A] =
    integral_[A](range).shrink(Shrink.towards(range.origin, _))

  /**
   * Generates a random integral number in the `[inclusive,inclusive]` range.
   *
   * ''This generator does not shrink.''
   */
  def integral_[A](range: Range[A])(implicit F: Monad[M], I: Integral[A]): GenT[M, A] =
    genT.generate((size, seed) => {
      val (x, y) = range.bounds(size)
      val (s2, a) = seed.chooseLong(I.toLong(x), I.toLong(y))
      (s2, I.fromInt(a.toInt))
    })

  def char(lo: Char, hi: Char)(implicit F: Monad[M]): GenT[M, Char] =
    integral[Long](Range.constant(lo.toLong, hi.toLong)).map(_.toChar)

  /**********************************************************************/
  // Combinators - Enumeration

  /**
   * Generates a random boolean.
   *
   * _This generator shrinks to 'False'._
   */
  def boolean(implicit F: Monad[M]): GenT[M, Boolean] =
    element1(false, true)

  /**********************************************************************/
  // Combinators - Fractional

  def double(range: Range[Double])(implicit F: Monad[M]): GenT[M, Double] =
    double_(range).shrink(Shrink.towardsFloat(range.origin, _))

  def double_(range: Range[Double])(implicit F: Monad[M]): GenT[M, Double] =
    genT.generate((size, seed) => {
      val (x, y) = range.bounds(size)
      seed.chooseDouble(x, y)
    })

  /**********************************************************************/
  // Combinators - Choice

  /**
   * Trivial generator that always produces the same element.
   */
  def constant[A](x: => A)(implicit F: Monad[M]): GenT[M, A] =
    GenT.GenApplicative.point(x)

  /**
   * Randomly selects one of the elements in the list.
   *
   * This generator shrinks towards the first element in the list.
   */
  def element1[A](x: A, xs: A*)(implicit F: Monad[M]): GenT[M, A] =
    element(x, xs.toList)

  /**
   * Randomly selects one of the elements in the list.
   *
   * This generator shrinks towards the first element in the list.
   */
  def element[A](x: A, xs: List[A])(implicit F: Monad[M]): GenT[M, A] =
    integral[Int](Range.constant(0, xs.length)).map(i => (x :: xs)(i))

  /**
   * Randomly selects one of the elements in the list.
   *
   * This generator shrinks towards the first element in the list.
   *
   * WARNING: This may throw an exception if the list is empty,
   * please use one of the other `element` variants if possible
   */
  def elementUnsafe[A](xs: List[A])(implicit F: Monad[M]): GenT[M, A] =
    xs match {
      case Nil =>
        sys.error("element: used with empty list")
      case h :: t =>
        element(h, t)
    }

  /**
   * Randomly selects one of the generators in the list.
   *
   * This generator shrinks towards the first generator in the list.
   */
  def choice1[A](x: GenT[M, A], xs: GenT[M, A]*)(implicit F: Monad[M]): GenT[M, A] =
    choice(x, xs.toList)

  /**
   * Randomly selects one of the generators in the list.
   *
   * This generator shrinks towards the first generator in the list.
   */
  def choice[A](x: GenT[M, A], xs: List[GenT[M, A]])(implicit F: Monad[M]): GenT[M, A] =
    integral[Int](Range.constant(0, xs.length)).flatMap(i => (x :: xs)(i))

  /**
   * Uses a weighted distribution to randomly select one of the generators in the list.
   *
   * This generator shrinks towards the first generator in the list.
   *
   * _The input list must be non-empty._
   */
   def frequency1[A](a: (Int, GenT[M, A]), l: (Int, GenT[M, A])*): GenT[M, A] =
     frequency(a, l.toList)

   /**
    * Uses a weighted distribution to randomly select one of the generators in the list.
    *
    * This generator shrinks towards the first generator in the list.
    *
    * _The input list must be non-empty._
    */
   def frequency[A](a: (Int, GenT[M, A]), l: List[(Int, GenT[M, A])]): GenT[M, A] = {
     val xs0 = a :: l.toList
     @annotation.tailrec
     def pick(n: Int, x: (Int, GenT[M, A]), xs: List[(Int, GenT[M, A])]): GenT[M, A] =
       if (n <= x._1)
         x._2
       else
         xs match {
           case Nil =>
             sys.error("Invariant: frequency hits an impossible code path")
           case h :: t =>
             pick(n - x._1, h, t)
         }
     val total = xs0.map(_._1).sum
     val n = Range.constant(1, total)
     pick(n.origin, a, xs0)
   }

  /**********************************************************************/
  // Combinators - Conditional

  /**
   * Discards the whole generator.
   */
  def discard[A](implicit F: Monad[M]): GenT[M, A] =
    GenT((_, seed) => Tree.TreeApplicative(F).point((seed, None)))
}
