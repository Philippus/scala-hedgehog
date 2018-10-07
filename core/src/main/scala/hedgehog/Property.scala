package hedgehog

import hedgehog.core._
import hedgehog.predef._

trait PropertyTOps[M[_]] extends PropertyTReporting[M] {

  def fromGen[A](gen: GenT[M, A])(implicit F: Monad[M]): PropertyT[M, A] =
    PropertyT(PropertyConfig.default, gen.map(x => (Nil, Some(x))))

  def hoist[A](a: (List[Log], A))(implicit F: Monad[M]): PropertyT[M, A] =
    PropertyT(PropertyConfig.default, GenT.GenApplicative(F).point(a.copy(_2 = Some(a._2))))

  def lift[A](m: M[A])(implicit F: Monad[M]): PropertyT[M, A] =
    fromGen(genT.lift(m))

  def writeLog(log: Log)(implicit F: Monad[M]): PropertyT[M, Unit] =
    hoist((List(log), ()))

  def info(log: String)(implicit F: Monad[M]): PropertyT[M, Unit] =
    writeLog(Info(log))

  def discard(implicit F: Monad[M]): PropertyT[M, Unit] =
    fromGen(genT.discard)

  def failure_(implicit F: Monad[M]): PropertyT[M, Unit] =
    failure[Unit]

  def failure[A](implicit F: Monad[M]): PropertyT[M, A] =
    PropertyT(PropertyConfig.default, GenT.GenApplicative(F).point((Nil, None)))

  def success(implicit F: Monad[M]): PropertyT[M, Unit] =
    hoist((Nil, ()))

  def assert(b: Boolean)(implicit F: Monad[M]): PropertyT[M, Unit] =
    if (b) success else failure

  def evalEither[A](ea: Either[String, A])(implicit F: Monad[M]): PropertyT[M, A] =
    ea match {
      case Left(e) =>
        failure
      case Right(a) =>
        success.map(_ => a)
    }
}
