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

  def command[M[_]](pid: java.util.concurrent.atomic.AtomicReference[Pid])(implicit M: Monad[M]): Command[Identity, M, State] =
    new Command[Identity, M, State] {

      override type Input[X[_]] = Spawn[X]
      override type Output = Pid

      override def htraverse: HTraversable[Input] =
        HTraversableSpawn

      override def tag: ClassTag[Output] =
        implicitly[ClassTag[Pid]]

      override def gen(s: State[Symbolic]): Option[Gen[Input[Symbolic]]] =
        Some(Gen.constant(Spawn()))

      override def execute(s: Input[Concrete]): M[Output] = {
        val oldPid = pid.getAndUpdate(x => Pid(x.value + 1))
        M.point(oldPid)
      }

      override def requires: List[Require[Input, Output, State]] =
        Nil

      override def updates: List[Update[Input, Output, State]] =
        List(new Update[Input, Output, State] {
          def run[V[_]](s: State[V], i: Input[V], o: Var[Output, V]): State[V] =
            s.copy(pids = s.pids + o)
        })

      override def ensures: List[Ensure[Input, Output, State]] =
        Nil
    }
}

case class Register[V[_]](name: Name, value: Var[Pid, V])

object Register {

  implicit def HTraversableRegister: HTraversable[Register] =
    new HTraversable[Register] {
      override def htraverse[F[_], G[_], H[_]](t: Register[G])(f: HF[F, G, H])(implicit A: Applicative[F]): F[Register[H]] =
        t match {
          case Register(n, v) =>
            A.ap(Var.HTraversableVar.htraverse(v)(f))(A.point(Register(n, _)))
        }
    }

  def command[M[_]](procTable: collection.mutable.Map[Name, Pid])(implicit M: Monad[M]): Command[Identity, M, State] =
    new Command[Identity, M, State] {

      override type Input[X[_]] = Register[X]
      override type Output = Unit

      override def htraverse: HTraversable[Input] =
        HTraversableRegister

      override def tag: ClassTag[Output] =
        implicitly[ClassTag[Output]]

      override def gen(s: State[Symbolic]): Option[Gen[Input[Symbolic]]] =
        s.pids.toList match {
          case Nil =>
            None
          case xs =>
            Some(for {
              n <- Gen.element("a", List("b", "c", "d")).map(Name(_))
              v <- Gen.elementUnsafe(xs)
            } yield Register(n, v))
        }

      override def execute(s: Input[Concrete]): M[Output] = {
        if (procTable.contains(s.name))
          sys.error("already registered")
        procTable += s.name -> s.value.value.value
        M.point(())
      }

      override def requires: List[Require[Input, Output, State]] =
        List(
          newRequire((state, input) => !state.regs.contains(input.name))
        , newRequire((state, input) => state.regs.forall(x => x._2 != input.value))
        )

      override def updates: List[Update[Input, Output, State]] =
        List(new Update[Input, Output, State] {
          def run[V[_]](s: State[V], i: Input[V], o: Var[Output, V]): State[V] =
            s.copy(regs = s.regs + (i.name -> i.value))
        })

      override def ensures: List[Ensure[Input, Output, State]] =
        Nil
    }
}

object Registry extends Properties {

  override def tests: List[Prop] =
    List(
      Prop("sequential", testSequential)
    )

  def testSequential: Property[Unit] = {
    val procTable = collection.mutable.Map[Name, Pid]()
    val pid = new java.util.concurrent.atomic.AtomicReference[Pid](Pid(0))
    for {
      actions <- Action.sequential(Range.linear(1, 100), State.default[Symbolic], List(
        Spawn.command[Identity](pid)
      , Register.command[Identity](procTable)
      )).forAll
      _ = procTable.clear()
      _ = pid.set(Pid(0))
      x <- Action.executeSequential(State.default[Concrete], actions)
    } yield x
  }
}
