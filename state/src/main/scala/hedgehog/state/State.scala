package hedgehog.state

import hedgehog._
import hedgehog.core._
import hedgehog.predef._

import scala.reflect.ClassTag

/** Symbolic variable names. */
case class Name(value: Int)

/** Symbolic values. */
// TODO Is `ClassTag` a reasonable approximation of Typeable? I suspect it's not but I clearly haven't used them in anger before
case class Symbolic[A](name: Name, value: ClassTag[A])

object Symbolic {

  /** Insert a symbolic variable in to a map of variables to types. */
  def insert[A](m: Map[Name, ClassTag[_]], s: Symbolic[A]): Map[Name, ClassTag[_]] =
    m + (s.name -> s.value)
}

/** Concrete values. */
case class Concrete[A](value: A)

/**********************************************************************/

/**
 * Variables are the potential or actual result of executing an action. They
 * are parameterised by either `Symbolic` or `Concrete` depending on the
 * phase of the test.
 *
 * `Symbolic` variables are the potential results of actions. These are used
 * when generating the sequence of actions to execute. They allow actions
 * which occur later in the sequence to make use of the result of an action
 * which came earlier in the sequence.
 *
 * `Concrete` variables are the actual results of actions. These are used
 * during test execution. They provide access to the actual runtime value of
 * a variable.
 *
 * The state update `Callback` for a command needs to be polymorphic in the
 * type of variable because it is used in both the generation and the
 * execution phase.
 *
 */
case class Var[A, V[_]](value: V[A])

/**********************************************************************/
// Symbolic Environment

/** A mapping of symbolic values to concrete values. */
case class Environment(value: Map[Name, Dynamic])

sealed trait EnvironmentError
case class EnvironmentValueNotFound(name: Name) extends EnvironmentError
// TODO What is the equivalent of TypeRep in the JVM?
case class EnvironmentTypeError(a: Any, b: Any) extends EnvironmentError


/**********************************************************************/
// Callback

sealed trait Callback[Input[_[_]], Output, State[_[_]]] {

  def require1(s: State[Symbolic], i: Input[Symbolic]): Boolean =
    this match {
      case Require(f) =>
        f(s, i)
      case Update(_) =>
        true
      case Ensure(_) =>
        true
    }
}

case class Require[Input[_[_]], Output, State[_[_]]](
    run: (State[Symbolic], Input[Symbolic]) => Boolean
  ) extends Callback[Input, Output, State] {
}
case class Update[V[_], Input[_[_]], Output, State[_[_]]](
    run: (State[V], Input[V], Var[Output, V]) => State[V]
  ) extends Callback[Input, Output, State]
case class Ensure[Input[_[_]], Output, State[_[_]]](
    run: (State[Concrete], State[Concrete], Input[Concrete], Output) => Property[Unit]
  ) extends Callback[Input, Output, State]

object Callback {

}
/**********************************************************************/

/** The specification for the expected behaviour of an `Action`. */
trait Command[N[_], M[_], State[_[_]], Input[_[_]], Output] {

  def gen(s: State[Symbolic]): Option[N[Input[Symbolic]]]

  def execute(s: Input[Concrete]): M[Output]

  def callbacks: List[Callback[Input, Output, State]]

  def genOK(state: State[Symbolic]): Boolean =
    gen(state).isDefined

  def genRequire(s: State[Symbolic])(implicit F: Functor[N]): Boolean =
    gen(s) match {
      case None =>
        Left(())
      case Some(in) =>
        Right(F.map(in)(i => callbacks.forall(_.require1(s, i))))
    }

}

trait Action[M[_], State[_[_]], Input[_[_]], Output] {

//  type Input[_[_]]
//  type Output

  def input: Input[Symbolic]

  def output: Symbolic[Output]

  def execute(input: Input[Concrete]): M[Output]

  def require(state: State[Symbolic], input: Input[Symbolic]): Boolean

//  def update[V[_]](state: State[V], input: Input[V], v: Var[Output, V]): State[V]

//  def ensure(state: State[Concrete], state2: State[Concrete], input: Input[Concrete], output: Output): Property[Unit]
}

case class Context[State[_[_]]](state: State[Symbolic], vars: Map[Name, ClassTag[_]])

object Context {

  def newVar[State[_[_]], A](c: Context[State])(implicit T: ClassTag[A]): (Context[State], Symbolic[A]) = {
    // TODO Ordering of the map, does the haskell version care?!?!
    val v: Symbolic[A] = c.vars.toList.headOption match {
      case None =>
         Symbolic(Name(0), T)
      case Some((name, t)) =>
        Symbolic(Name(name.value + 1), T)
    }
    (c.copy(vars = Symbolic.insert[A](c.vars, v)), v)
  }
}

object Action {

  def action[N[_], M[_], State[_[_]], Input[_[_]], Output](
      commands: List[Command[N, M, State, Input, Output]]
    , context: Context[State]
    )(implicit F: Monad[N], T: ClassTag[Output]): GenT[N, (Context[State], Action[M, State, Input, Output])] =

    genT.fromSome(genT.elementUnsafe(commands.filter(_.genOK(context.state))).flatMap(cmd =>
      cmd.genRequire(context.state) match {
        case Left(_) =>
          sys.error("Command.gen: internal error, tried to use generator with invalid state.")
        case Right(nb) =>
          genT.lift(nb).flatMap(b =>
            if (!b) {
              GenT.GenApplicative.point(None)
            } else {
              val (context2, outputt) = Context.newVar[State, Output](context)
//               Context.update(cmd.callbacks, cmd, input, Var(output))
              GenT.GenApplicative.point(Some((context, new Action[M, State, Input, Output] {
                override def input = ???
                override def output = outputt
                override def execute(input: Input[Concrete]) = cmd.execute(input)
                override def require(state: State[Symbolic], input: Input[Symbolic]): Boolean = ???
//                override def update[V[_]](state: State[V], input: cmd.Input[V], v: Var[this.type, V]): State[V] = ???
//                override def ensure(state: State[Concrete], state2: State[Concrete], input: cmd.Input[Concrete], output: this.type): Property[Unit] = ???
              })))

            }
          )
      }
    ))
}
