package matterhorn

import RTS._
import Exp._

case class IO[A] private[matterhorn](thunk: Thunk) extends AnyVal {
  def *>[B](io: => IO[B]): IO[B] = flatMap(_ => io)
  def map[B](f: A => B): IO[B] = IO[B](Thunk.map(thunk)(f))
  def flatMap[B](f: A => IO[B]): IO[B] = IO[B](Thunk.bind(thunk)(f))

  def concurrently: Concurrently[A] = Concurrently(thunk)
  def void: IO[Unit] = map(_ => ())
}

object IO {
  val unitIO: IO[Unit] = constIO(())

  trait App {
    def mainIO: IO[Unit]
    def rts: RTS = defaultRTS

    // TODO Feed arguments with the STM in IO
    def main(args: Array[String]): Unit = rts.unsafePerformIO_(mainIO)
  }

  def constIO[A](value: A): IO[A] = IO[A](Thunk.point(_ => value))
  def captureIO[A](fx: => A): IO[A] = IO[A](Thunk.point(_ => fx))
  def liftIO[A, B](f: A => B): A => IO[B] = x => captureIO(f(x))

  // TODO Can this combinators remove by bringing scalaz in?
  //def duringIO[A](p: IO[Boolean])(io: IO[A]): IO[Seq[A]] = p.flatMap(x => if (x) io.void else unit)
  def duringIO_[A](p: IO[Boolean])(io: IO[A]): IO[Unit] = p.flatMap(x => if (x) io *> duringIO_(p)(io) else unitIO)
  def whenIO[A](p: IO[Boolean])(io: IO[A]): IO[Unit] = p.flatMap(x => if (x) io.void else unitIO)

  def catching[E <: Throwable, A](io: IO[A])(f: E => IO[A])(implicit E: scala.reflect.ClassTag[E]): IO[A] =
    IO[A](Thunk.catching(io)(f))
}
