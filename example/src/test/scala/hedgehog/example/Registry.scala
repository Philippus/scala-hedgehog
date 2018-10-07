/**
 * Translated from:
 *
 * https://github.com/hedgehogqa/haskell-hedgehog/blob/master/hedgehog-example/src/Test/Example/Registry.hs
 */
package hedgehog.example

import hedgehog.predef.Applicative
import hedgehog.state._

case class Pid(value: Int)
case class Name(value: String)

case class State[V[_]](
    pids: Set[Var[Pid, V]]
  , regs: Map[Name, Var[Pid, V]]
  )

object State {

  def default[V[_]]: State[V] =
    State(Set(), Map())
}

case class Spawn[V[_]]()

object Spawn {

  implicit def HTraversableSpawn: HTraversable[Spawn] =
    new HTraversable[Spawn] {
      override def htraverse[F[_], G[_], H[_]](t: Spawn[G])(f: HF[F, G, H])(implicit A: Applicative[F]): F[Spawn[H]] =
        t match {
          case Spawn() =>
            A.point(Spawn())
        }
    }

}
