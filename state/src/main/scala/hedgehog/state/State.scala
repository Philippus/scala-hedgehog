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
    }).exec(Map.empty[Name, ClassTag[_]]).value

  def variablesOK[T[_[_]]](xs: T[Symbolic], allowed: TypeMap)(implicit HT: HTraversable[T]): Boolean =
    takeVariables(xs).forall(v => allowed.get(v._1).contains(v._2))

}

trait Dynamic {

  type A
  val value: A
  val tag: ClassTag[A]
}

/** Concrete values. */
case class Concrete[B](value: B) { self =>

  def toDyn(implicit T: ClassTag[B]): Dynamic =
    new Dynamic { type A = B; val value: A = self.value; val tag: ClassTag[A] = T }
}

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
case class Environment(value: Map[Name, Dynamic]) {

  def reify[A](n: Symbolic[A]): Either[EnvironmentError, Concrete[A]] =
    value.get(n.name)
      .toRight(EnvironmentValueNotFound(n.name))
      .flatMap(dyn =>
        if (n.value == dyn.tag) {
          Right(Concrete(dyn.value.asInstanceOf[A]))
        } else {
          Left(EnvironmentTypeError(dyn))
        }
      )
}

sealed trait EnvironmentError
case class EnvironmentValueNotFound(name: Name) extends EnvironmentError
// TODO What is the equivalent of TypeRep in the JVM?
case class EnvironmentTypeError(e: Dynamic) extends EnvironmentError


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
trait Command[N[_], M[_], S[_[_]]] { self =>

  type Input[_[_]]
  type Output

  def htraverse: HTraversable[Input]

  def tag: ClassTag[Output]

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

  // NOTE: I found the compiler didn't play nicely with the type variables as a separate function,
  // and it was easier to include inline
  final def action(context: Context[S])(implicit F: Monad[N]): StateT[GenT[N, ?], Context[S], Option[Action[M, S]]] =
    for {
      inputt <- gen(context.state) match {
        case None =>
          sys.error("Command.gen: internal error, tried to use generator with invalid state.")
        case Some(nb) =>
          stateT.lift[Context[S], Input[Symbolic]](genT.lift(nb))
      }
      x <-
        if (!require(context.state, inputt)) {
          stateT[GenT[N, ?]].point[Context[S], Option[Action[M, S]]](None)
        } else {
          val (context2, outputt) = Context.newVar[S, Output](context)(tag)
          val context3 = context2.copy(state = update(context.state, inputt, Var(outputt)))
          for {
            _ <- stateT[GenT[N, ?]].put(context3)
            y <- stateT[GenT[N, ?]].point[Context[S], Option[Action[M, S]]](Some(new Action[M, S] {
              override type Input[X[_]] =
                self.Input[X]
              override type Output =
                self.Output
              override def htraverse: HTraversable[Input] =
                self.htraverse
              override def tag: ClassTag[Output] =
                self.tag
              override def input: Input[Symbolic] =
                inputt
              override def output: Symbolic[Output] =
                outputt
              override def execute(input: Input[Concrete]): M[Output] =
                self.execute(input)
              override def require(state: S[Symbolic], input: Input[Symbolic]): Boolean =
                self.require(state, input)
              override def update[V[_]](state: S[V], input: Input[V], v: Var[Output, V]): S[V] =
                self.update(state, input, v)
              override def ensure(state: S[Concrete], state2: S[Concrete], input: Input[Concrete], output: Output): Property[Unit] =
                self.ensure(state, state2, input, output)
            }))
          } yield y
        }
    } yield x
}

trait Action[M[_], S[_[_]]] {

  type Input[_[_]]
  type Output

  def htraverse: HTraversable[Input]

  def tag: ClassTag[Output]

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

  def create[S[_[_]]](s: S[Symbolic]): Context[S] =
    Context(s, Map())
}

object Action {

  def action[N[_], M[_], S[_[_]]](
      commands: List[Command[N, M, S]]
    )(implicit F: Monad[N]
    ): StateT[GenT[N, ?], Context[S], Action[M, S]] =
    MonadGen.fromSome(for {
      context <- stateT[GenT[N, ?]].get[Context[S]]
      cmd <- MonadGen[StateT[GenT[N, ?], Context[S], ?]].elementUnsafe(commands.filter(_.genOK(context.state)))
      a <- cmd.action(context)
      } yield a)

  def genActions[N[_], M[_], S[_[_]]](
      range: Range[Int]
    , commands: List[Command[N, M, S]]
    , ctx: Context[S]
    )(implicit F: Monad[N]
    ): GenT[N, (Context[S], List[Action[M, S]])] = {
      for {
        xs <- MonadGen.list(action(commands), range).eval(ctx)
      } yield dropInvalid(xs).run(ctx).value
    }

  /** Drops invalid actions from the sequence. */
  def dropInvalid[S[_[_]], M[_]](
      actions: List[Action[M, S]]
    ): State[Context[S], List[Action[M, S]]] = {

    def loop(step: Action[M, S]): State[Context[S], Option[Action[M, S]]] =
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
            State.point[Context[S], Option[Action[M, S]]](Option.empty[Action[M, S]])
          }
      } yield x
    sequence[State[Context[S], ?], Option[Action[M, S]]](
      actions.map(loop)).map(_.flatMap(_.toList)
    )
  }

  def executeUpdateEnsure[S[_[_]], M[_]](
      state0: S[Concrete]
    , env0: Environment
    , action: Action[M, S]
    )(implicit F: Monad[M]): PropertyT[M, (S[Concrete], Environment)] =
    for {
      input <- propertyT.evalEither(action.htraverse.htraverse(action.input)(new HF[Either[EnvironmentError, ?], Symbolic, Concrete] {
        def apply[A](n: Symbolic[A]): Either[EnvironmentError, Concrete[A]] =
          env0.reify(n)
      }).left.map(_.toString))
      output <- propertyT.lift(action.execute(input))
      coutput = Concrete(output)
      state = action.update(state0, input, Var(coutput))
      _ <- propertyT.withM(action.ensure(state0, state, input, output))
      env = Environment(env0.value + (action.output.name -> coutput.toDyn(action.tag)))
    } yield (state, env)


  def sequential[N[_], M[_], S[_[_]]](
      range: Range[Int]
    , initial: S[Symbolic]
    , commands: List[Command[N, M, S]]
    )(implicit F: Monad[N]
    ): GenT[N, List[Action[M, S]]] =
      genActions(range, commands, Context.create(initial)).map(_._2)

  def executeSequential[M[_], S[_[_]]](
      initial: S[Concrete]
    , actions: List[Action[M, S]]
    )(implicit F: Monad[M]): PropertyT[M, Unit] =
    foldM[PropertyT[M, ?], Action[M, S], (S[Concrete], Environment)](
        actions
      , (initial, Environment(Map()))
      )((s, a) => executeUpdateEnsure(s._1, s._2, a))
      .map(_ => ())
}
