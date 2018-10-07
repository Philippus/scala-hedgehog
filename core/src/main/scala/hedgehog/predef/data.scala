package hedgehog.predef

trait Functor[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {

  def point[A](a: => A): F[A]

  def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(fa)(point(f))
}

object Applicative {

  implicit def ApplicativeEither[L]: Applicative[Either[L, ?]] =
    new Applicative[Either[L, ?]] {

      override def point[A](a: => A): Either[L, A] =
        ???

      override def ap[A, B](fa: => Either[L, A])(f: => Either[L, A => B]): Either[L, B] =
        ???
   }
}

trait Monad[F[_]] extends Applicative[F] {

  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] =
    bind(f)(x => map(fa)(x))
}
