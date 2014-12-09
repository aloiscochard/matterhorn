import scalaz._

import matterhorn._
import IO._

package object matterhorn {
  implicit val IOMonad: Monad[IO] = new Monad[IO] {
    def point[A](a: => A): IO[A] = captureIO(a)
    def bind[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
    override def map[A, B](fa: IO[A])(f: A => B): IO[B] = fa.map(f)
  }

  implicit val CatchableMonad: Catchable[IO] = new Catchable[IO] {
    def attempt[A](f: IO[A]): IO[Throwable \/ A] =
      catching[Throwable, Throwable \/ A](f.map(\/-(_)))(e => constIO(-\/(e)))
    def fail[A](err: Throwable): IO[A] = captureIO(throw err)
  }

  implicit val ConcurrentlyApplicative: Applicative[Concurrently] = new Applicative[Concurrently] {
    def point[A](a: => A): Concurrently[A] = captureIO(a).concurrently
    def ap[A, B](fa: => Concurrently[A])(f: => Concurrently[A => B]): Concurrently[B] =
      Concurrent.concurrently(fa.io, f.io).map { case (a, f) => f(a) }.concurrently
    override def map[A, B](fa: Concurrently[A])(f: A => B): Concurrently[B] = fa.map(f)
  }

  implicit val STMMonad: Monad[STM] = new Monad[STM] {
    def point[A](a: => A): STM[A] = STM(_ => a)
    def bind[A, B](fa: STM[A])(f: A => STM[B]): STM[B] = fa.flatMap(f)
  }
}
