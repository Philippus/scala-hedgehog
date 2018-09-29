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
// NOTE: This is different from the Haskell version that makes this a GADT
// which I think just makes you jump through more hoops

case class Require[Input[_[_]], Output, State[_[_]]](
    run: (State[Symbolic], Input[Symbolic]) => Boolean
  )

trait Update[Input[_[_]], Output, State[_[_]]] {

  def run[V[_]](s: State[V], i: Input[V], v: Var[Output, V]): State[V]
}

case class Ensure[Input[_[_]], Output, State[_[_]]](
    run: (State[Concrete], State[Concrete], Input[Concrete], Output) => Property[Unit]
  )

/**********************************************************************/

/**
 * The specification for the expected behaviour of an `Action`.
 *
 * NOTE: We unfortunately expose the input/output types at the trait level (unlike the Haskell versions existentials).
 * It's almost certainly possible to use type members, but I couldn't get the path-dependent types to place nice.
 */
trait Command[N[_], M[_], State[_[_]], Input[_[_]], Output] {

  def gen(s: State[Symbolic]): Option[N[Input[Symbolic]]]

  def execute(s: Input[Concrete]): M[Output]

  def requires: List[Require[Input, Output, State]]

  def updates: List[Update[Input, Output, State]]

  def ensures: List[Ensure[Input, Output, State]]

  def genOK(state: State[Symbolic]): Boolean =
    gen(state).isDefined

  // Helper functions

  def require(s: State[Symbolic], i: Input[Symbolic]): Boolean =
    requires.forall(_.run(s, i))

  def update[V[_]](s0: State[V], i: Input[V], o: Var[Output, V]): State[V] =
    updates.foldLeft(s0)((s, u) => u.run(s, i, o))

  def ensure(s0: State[Concrete], s: State[Concrete], i: Input[Concrete], o: Output): Property[Unit] =
    traverse_(ensures)(_.run(s0, s, i, o))
}

trait Action[M[_], State[_[_]], Input[_[_]], Output] {

  def input: Input[Symbolic]

  def output: Symbolic[Output]

  def execute(input: Input[Concrete]): M[Output]

  def require(state: State[Symbolic], input: Input[Symbolic]): Boolean

  def update[V[_]](state: State[V], input: Input[V], v: Var[Output, V]): State[V]

  def ensure(state: State[Concrete], state2: State[Concrete], input: Input[Concrete], output: Output): Property[Unit]
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
    )(implicit F: Monad[N], T: ClassTag[Output]
    ): GenT[N, (Context[State], Action[M, State, Input, Output])] =

    genT.fromSome(for {
      cmd <- genT.elementUnsafe(commands.filter(_.genOK(context.state)))
      inputt <- cmd.gen(context.state) match {
        case None =>
          sys.error("Command.gen: internal error, tried to use generator with invalid state.")
        case Some(nb) =>
          genT.lift(nb)
      }
      x <-
        if (!cmd.require(context.state, inputt)) {
          genT.constant(None)
        } else {
          val (context2, outputt) = Context.newVar[State, Output](context)
          val context3 = context2.copy(state = cmd.update(context.state, inputt, Var(outputt)))
          genT.constant(Some((context3, new Action[M, State, Input, Output] {
            override def input: Input[Symbolic] =
              inputt
            override def output: Symbolic[Output] =
              outputt
            override def execute(input: Input[Concrete]): M[Output] =
              cmd.execute(input)
            override def require(state: State[Symbolic], input: Input[Symbolic]): Boolean =
              cmd.require(state, input)
            override def update[V[_]](state: State[V], input: Input[V], v: Var[Output, V]): State[V] =
              cmd.update(state, input, v)
            override def ensure(state: State[Concrete], state2: State[Concrete], input: Input[Concrete], output: Output): Property[Unit] =
              cmd.ensure(state, state2, input, output)
          })))
        }
      } yield x)


  def genActions[N[_], M[_], State[_[_]], Input[_[_]], Output](
      range: Range[Int]
    , commands: List[Command[N, M, State, Input, Output]]
    , ctx: Context[State]
    )(implicit F: Monad[N], T: ClassTag[Output]
    ): GenT[N, (Context[State], List[Action[M, State, Input, Output]])] =
    // TODO We really need a StateT here, unfortunately all the GenT functions won't be compatible
    action(commands, ctx).list(range).map(_.map(_._2)).flatMap(xs =>
      genT.constant(dropInvalid(xs, ctx))
    )

  def dropInvalid[State[_[_]], Input[_[_]], Output, M[_]](
      actions: List[Action[M, State, Input, Output]]
    , ctx: Context[State]
    ): (Context[State], List[Action[M, State, Input, Output]]) =
    ???
}
