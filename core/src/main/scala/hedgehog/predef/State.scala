package hedgehog.predef

import hedgehog.Gen
import hedgehog.core._

case class StateT[M[_], S, A](run: S => M[(S, A)]) {

  def eval(s: S)(implicit F: Functor[M]): M[A] =
    F.map(run(s))(_._2)

  def exec(s: S)(implicit F: Functor[M]): M[S] =
    F.map(run(s))(_._1)

  def map[B](f: A => B)(implicit F: Functor[M]): StateT[M, S, B] =
    StateT(s => F.map(run(s))(x => x._1 -> f(x._2)))

  def flatMap[B](f: A => StateT[M, S, B])(implicit F: Monad[M]): StateT[M, S, B] =
    StateT(s => F.bind(run(s))(x => f(x._2).run(x._1)))
}


abstract class StateTImplicits1 {

  implicit def StateTFunctor[M[_], S](implicit F: Functor[M]): Functor[StateT[M, S, ?]] =
    new Functor[StateT[M, S, ?]] {
      override def map[A, B](fa: StateT[M, S, A])(f: A => B): StateT[M, S, B] =
        fa.map(f)
    }
}

abstract class StateTImplicits2 extends StateTImplicits1 {

  implicit def StateTApplicative[M[_], S](implicit F: Monad[M]): Applicative[StateT[M, S, ?]] =
    new Applicative[StateT[M, S, ?]] {
      def point[A](a: => A): StateT[M, S, A] =
        StateT(s => F.point((s, a)))
      def ap[A, B](fa: => StateT[M, S, A])(f: => StateT[M, S, A => B]): StateT[M, S, B] =
        StateT.StateTMonad[M, S].bind(f)(ab =>
        StateT.StateTMonad[M, S].bind(fa)(a =>
          point(ab(a))
        ))
    }
}

object StateT extends StateTImplicits2 {

  implicit def StateTMonad[M[_], S](implicit F: Monad[M]): Monad[StateT[M, S, ?]] =
    new Monad[StateT[M, S, ?]] {
      override def map[A, B](fa: StateT[M, S, A])(f: A => B): StateT[M, S, B] =
        fa.map(f)
      override def point[A](a: => A): StateT[M, S, A] =
        StateTApplicative(F).point(a)
      override def bind[A, B](fa: StateT[M, S, A])(f: A => StateT[M, S, B]): StateT[M, S, B] =
        fa.flatMap(f)
    }

  implicit def StateTMonadGen[M[_], S](implicit F: Functor[M], G: MonadGen[M]): MonadGen[StateT[M, S, ?]] =
    new MonadGen[StateT[M, S, ?]] {
      def lift[A](gen: Gen[A]): StateT[M, S, A] =
        StateT(s => F.map(G.lift(gen))(a => (s, a)))
      def shrink[A](gen: StateT[M, S, A], f: A => List[A]): StateT[M, S, A] =
        ???
    }
}

trait StateTOpt[M[_]] {

  def point[S, A](a: A)(implicit F: Applicative[M]): StateT[M, S, A] =
    StateT(s => F.point((s, a)))

  def get[S](implicit F: Applicative[M]): StateT[M, S, S] =
    StateT(s => F.point((s, s)))

  def put[S](s: S)(implicit F: Applicative[M]): StateT[M, S, Unit] =
    StateT(_ => F.point((s, ())))

  def modify[S](f: S => S)(implicit F: Applicative[M]): StateT[M, S, Unit] =
    StateT(s => F.point((f(s), ())))
}
