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

  def takeVariables[T[_[_]]](xs: T[Symbolic])(implicit HT: HTraversable[T]): TypeMap =
    HT.htraverse[State[TypeMap, ?], Symbolic, Symbolic](xs)(new HF[State[TypeMap, ?], Symbolic, Symbolic] {
      def apply[A](x: Symbolic[A]): State[TypeMap, Symbolic[A]] =
        State.modify[TypeMap](m => Symbolic.insert(m, x))
          .map(_ => x)
    }).run(Map.empty[Name, ClassTag[_]]).value._1

  def variablesOK[T[_[_]]](xs: T[Symbolic], allowed: TypeMap)(implicit HT: HTraversable[T]): Boolean =
    takeVariables(xs).forall(v => allowed.get(v._1).contains(v._2))

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

case class Require[Input[_[_]], Output, S[_[_]]](
    run: (S[Symbolic], Input[Symbolic]) => Boolean
  )

trait Update[Input[_[_]], Output, S[_[_]]] {

  def run[V[_]](s: S[V], i: Input[V], v: Var[Output, V]): S[V]
}

case class Ensure[Input[_[_]], Output, S[_[_]]](
    run: (S[Concrete], S[Concrete], Input[Concrete], Output) => Property[Unit]
  )

/**********************************************************************/

/**
 * The specification for the expected behaviour of an `Action`.
 *
 * NOTE: We unfortunately expose the input/output types at the trait level (unlike the Haskell versions existentials).
 * It's almost certainly possible to use type members, but I couldn't get the path-dependent types to place nice.
 */
trait Command[N[_], M[_], S[_[_]], Input[_[_]], Output] {

  def htraverse: HTraversable[Input]

  def gen(s: S[Symbolic]): Option[N[Input[Symbolic]]]

  def execute(s: Input[Concrete]): M[Output]

  def requires: List[Require[Input, Output, S]]

  def updates: List[Update[Input, Output, S]]

  def ensures: List[Ensure[Input, Output, S]]

  def genOK(state: S[Symbolic]): Boolean =
    gen(state).isDefined

  // Helper functions

  def require(s: S[Symbolic], i: Input[Symbolic]): Boolean =
    requires.forall(_.run(s, i))

  def update[V[_]](s0: S[V], i: Input[V], o: Var[Output, V]): S[V] =
    updates.foldLeft(s0)((s, u) => u.run(s, i, o))

  def ensure(s0: S[Concrete], s: S[Concrete], i: Input[Concrete], o: Output): Property[Unit] =
    traverse_(ensures)(_.run(s0, s, i, o))
}

trait Action[M[_], S[_[_]], Input[_[_]], Output] {

  def htraverse: HTraversable[Input]

  def input: Input[Symbolic]

  def output: Symbolic[Output]

  def execute(input: Input[Concrete]): M[Output]

  def require(state: S[Symbolic], input: Input[Symbolic]): Boolean

  def update[V[_]](state: S[V], input: Input[V], v: Var[Output, V]): S[V]

  def ensure(state: S[Concrete], state2: S[Concrete], input: Input[Concrete], output: Output): Property[Unit]
}

case class Context[S[_[_]]](state: S[Symbolic], vars: Map[Name, ClassTag[_]])

object Context {

  def newVar[S[_[_]], A](c: Context[S])(implicit T: ClassTag[A]): (Context[S], Symbolic[A]) = {
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

  def action[N[_], M[_], S[_[_]], Input[_[_]], Output](
      commands: List[Command[N, M, S, Input, Output]]
    , context: Context[S]
    )(implicit F: Monad[N], T: ClassTag[Output]
    ): GenT[N, (Context[S], Action[M, S, Input, Output])] =

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
          val (context2, outputt) = Context.newVar[S, Output](context)
          val context3 = context2.copy(state = cmd.update(context.state, inputt, Var(outputt)))
          genT.constant(Some((context3, new Action[M, S, Input, Output] {
            override def htraverse: HTraversable[Input] =
              cmd.htraverse
            override def input: Input[Symbolic] =
              inputt
            override def output: Symbolic[Output] =
              outputt
            override def execute(input: Input[Concrete]): M[Output] =
              cmd.execute(input)
            override def require(state: S[Symbolic], input: Input[Symbolic]): Boolean =
              cmd.require(state, input)
            override def update[V[_]](state: S[V], input: Input[V], v: Var[Output, V]): S[V] =
              cmd.update(state, input, v)
            override def ensure(state: S[Concrete], state2: S[Concrete], input: Input[Concrete], output: Output): Property[Unit] =
              cmd.ensure(state, state2, input, output)
          })))
        }
      } yield x)


  def genActions[N[_], M[_], S[_[_]], Input[_[_]], Output](
      range: Range[Int]
    , commands: List[Command[N, M, S, Input, Output]]
    , ctx: Context[S]
    )(implicit F: Monad[N], T: ClassTag[Output]
    ): GenT[N, (Context[S], List[Action[M, S, Input, Output]])] =
    // TODO We really need a ST here, unfortunately all the GenT functions won't be compatible
    action(commands, ctx).list(range).map(_.map(_._2)).flatMap(xs =>
      genT.constant(dropInvalid(xs).run(ctx).value)
    )

  /** Drops invalid actions from the sequence. */
  def dropInvalid[S[_[_]], Input[_[_]], Output, M[_]](
      actions: List[Action[M, S, Input, Output]]
    ): State[Context[S], List[Action[M, S, Input, Output]]] = {

    def loop(step: Action[M, S, Input, Output]): State[Context[S], Option[Action[M, S, Input, Output]]] =
      for {
        c <- State.get[Context[S]]
        x <-
          if (step.require(c.state, step.input) && Symbolic.variablesOK(step.input, c.vars)(step.htraverse)) {
            for {
              _ <- State.put[Context[S]](Context(
                step.update(c.state, step.input, Var(step.output))
              , Symbolic.insert(c.vars, step.output)
              ))
            } yield some(step)
          } else {
            State.point[Context[S], Option[Action[M, S, Input, Output]]](Option.empty[Action[M, S, Input, Output]])
          }
      } yield x
    sequence[State[Context[S], ?], Option[Action[M, S, Input, Output]]](
      actions.map(loop)).map(_.flatMap(_.toList)
    )
  }
}
