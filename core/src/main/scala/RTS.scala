package matterhorn

import scala.language.higherKinds

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

import rts.Interruptor
import rts.Interpreter._

case class RTS(ec: ExecutionContext, intr: Interruptor) {
  import RTS._

  def unsafePerformIO[A](io: IO[A]): Future[A] =
    Val.reifyF[Future, A](unsafePerformEval(this)(io.thunk.reverse))

  def unsafePerformIO_[A](io: IO[A]): A =
    Await.result(unsafePerformIO(io), Duration.Inf)
}

object RTS {

  val defaultRTS = RTS(ExecutionContext.Implicits.global, Interruptor.global)

  case class  ThreadId private[matterhorn](future: Future[Val], intr: Interruptor)
  case object ThreadKilled extends Error

  type Val = Any with ({ type Tag = Any })

  private[matterhorn] object Val {
    val unit: Val = cast(Unit)
    def cast(x: Any): Val = x.asInstanceOf[Val]
    def castF[F[_]](x: F[Any]): F[Val] = x.asInstanceOf[F[Val]]
    def reify[A](x: Val): A = x.asInstanceOf[A]
    def reifyF[F[_], A](x: Future[Val]): F[A] = x.asInstanceOf[F[A]]
  }

  type Thunk = List[Exp]

  object Thunk {
    import Exp._
    import Val._

    def point[A](f: Unit => A): Thunk =
      Point(_ => cast(f(()))) :: Nil

    def bind[A, B](xs: Thunk)(f: A => IO[B]): Thunk =
      Bind(x => f(reify[A](x)).thunk) :: xs

    def map[A, B](xs: Thunk)(f: A => B): Thunk =
      Map(x => cast(f(reify[A](x)))) :: xs

    def apply[A, B, C](f: (A, B) => C)(ioa: IO[A], iob: IO[B]): Thunk =
      Apply((a, b) => cast(reify[A](a) -> reify[B](b)), ioa.thunk, iob.thunk) :: Nil

    def fork[A](io: IO[A]): Thunk =
      Fork(_ => io.thunk) :: Nil

    def catching[A, E <: Throwable](io: IO[A])(f: E => IO[A])(implicit E: scala.reflect.ClassTag[E]): Thunk =
      Catch({
        case e: E => Some(f(e).thunk)
        case _ => None
      }, io.thunk) :: Nil

    def fromFuture[A](future: Future[A]): Thunk =
      Wait(ThreadId(castF[Future](future), Interruptor.unintr)) :: Nil
  }

  sealed trait Exp

  object Exp {
    import Val._

    case class Point(f: Unit => Val) extends Exp
    case class Map(f: Val => Val) extends Exp
    case class Bind(f: Val => Thunk) extends Exp
    case class Apply(f: (Val, Val) => Val, left: Thunk, right: Thunk) extends Exp

    case class Fork(f: Unit => Thunk) extends Exp
    case class Wait(t: ThreadId) extends Exp

    case class Catch(f: Throwable => Option[Thunk], on: Thunk) extends Exp
  }
}
