package hedgehog.state

import hedgehog.predef._

/**
 * Higher-order traversable functors.
 *
 * This is used internally to make symbolic variables concrete given an 'Environment'.
 */
trait HTraversable[T[_[_]]] {

  def htraverse[F[_], G[_], H[_]](t: T[G])(f: HF[F, G, H])(implicit A: Applicative[F]): F[T[H]]
}

trait HF[F[_], G[_], H[_]] {

  def apply[A](f: G[A]): F[H[A]]
}

