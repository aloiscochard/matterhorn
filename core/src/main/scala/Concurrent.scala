package matterhorn

import scala.concurrent.Future

import IO._
import RTS._

import rts.Interruptor

case class ParIO[A] private[matterhorn](thunk: Thunk) extends AnyVal {
  def *>[B](par: ParIO[B]): ParIO[B] =
    Concurrent.concurrently(IO[A](thunk), IO[B](par.thunk)).map(_._2).par

  def io: IO[A] = IO(thunk)
}

object Concurrent {
  type  ThreadId = RTS.ThreadId
  val   ThreadKilled = RTS.ThreadKilled
  type  ThreadKilled = RTS.ThreadKilled.type

  def forkIO[A](io: IO[A]): IO[ThreadId] =
    IO[ThreadId](Thunk.fork(io))

  def killThread(thread: ThreadId): IO[Unit] =
    captureIO(thread.intr.kill)

  def concurrently[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] =
    IO[(A, B)](Thunk.apply[A, B, (A, B)]((a, b) => (a, b))(ioa, iob))

  // TODO Bring in scalaz and implement with Traverse (+ Ops)
  def mapConcurrently[A, B](ta: Seq[A])(f: A => IO[B])/*(implicit T: Traverse[T])*/: IO[Seq[B]] =  ta match {
    case Nil => constIO(Nil)
    case xs  => xs.map(x => f(x).map(Seq(_))).reduceLeft((a, b) => concurrently(a, b).map { case (a, b) => a ++ b })
  }

  def waitFuture[A](future: Future[A]): IO[A] = IO[A](Thunk.fromFuture(future))
}
