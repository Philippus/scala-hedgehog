/**
 * Translated from:
 *
 * https://github.com/hedgehogqa/haskell-hedgehog/blob/master/hedgehog-example/src/Test/Example/Registry.hs
 */
package hedgehog.example

import hedgehog._
import hedgehog.predef._
import hedgehog.runner._
import hedgehog.state._

import scala.reflect.ClassTag

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

  def command[M[_], N[_]](implicit N: Monad[N], M: Monad[M]): Command[N, M, State] =
    new Command[N, M, State] {

      override type Input[X[_]] = Spawn[X]
      override type Output = Pid

      private var pid: Pid = Pid(0)

      override def htraverse: HTraversable[Input] =
        HTraversableSpawn

      override def tag: ClassTag[Output] =
        implicitly[ClassTag[Pid]]

      override def gen(s: State[Symbolic]): Option[N[Input[Symbolic]]] =
        Some(N.point(Spawn()))

      override def execute(s: Input[Concrete]): M[Output] = {
        val oldPid = pid
        pid = Pid(oldPid.value + 1)
        M.point(oldPid)
      }

      override def requires: List[Require[Input, Output, State]] =
        Nil

      override def updates: List[Update[Input, Output, State]] =
        Nil

      override def ensures: List[Ensure[Input, Output, State]] =
        Nil
    }
}

object Registry extends Properties {

  override def tests: List[Prop] =
    List(
      Prop("sequential", testSequential)
    )

  def testSequential: Property[Unit] =
    for {
      actions <- Action.sequential(Range.linear(1, 100), State.default[Symbolic], List(Spawn.command[Identity, Identity])).forAll
      x <- Action.executeSequential(State.default[Concrete], actions)
    } yield x
}
