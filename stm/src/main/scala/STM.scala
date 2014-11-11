package matterhorn

import scala.concurrent.stm.{atomic, InTxn, Ref, Txn}

import IO._

// TODO Use Ref.View for readIO,.. methods
// TODO scalaz: Reader, Alternative instance using orElse

case class STM[A] private[matterhorn](run: InTxn => A) extends AnyVal {
  def *>[B](io: => STM[B]): STM[B] = flatMap(_ => io)
  def map[B](f: A => B): STM[B] = STM[B](tx => f(run(tx)))
  def flatMap[B](f: A => STM[B]): STM[B] = STM(tx => f(run(tx)).run(tx))

  //def orElse[B](that: => STM[A]): STM[A] = STM { implicit tx => atomic(run).orAtomic(that.run) }
}

object STM {
  def atomically[A](stm: STM[A]): IO[A] = captureIO(atomic(stm.run))
  def retry[A]: STM[A] = STM(Txn.retry(_))

  case class TVar[A](ref: Ref[A]) extends AnyVal {
    def read: STM[A] = TVar.readTVar(this)
    def write(a: A): STM[Unit] = TVar.writeTVar(this)(a)
    def modify(f: A => A): STM[Unit] = TVar.modifyTVar(this)(f)
  }

  object TVar {
    def newTVar[A](a: A)(implicit R: Reference[A]): STM[TVar[A]] = STM(_ => TVar(R(a)))

    def readTVar[A](tv: TVar[A]): STM[A] = STM(tv.ref.get(_))
    def writeTVar[A](tv: TVar[A])(a: A): STM[Unit] = STM(tv.ref.set(a)(_))
    def modifyTVar[A](tv: TVar[A])(f: A => A): STM[Unit] = readTVar(tv).flatMap(x => writeTVar(tv)(f(x)))

    trait Reference[A] extends (A => Ref[A])
    object Reference {
      def apply[A](f: A => Ref[A]): Reference[A] = new Reference[A] { def apply(x: A) = f(x) }
      implicit val unit: Reference[Unit] = apply(Ref.apply _)
      implicit val double: Reference[Double] = apply(Ref.apply _)
      implicit val float: Reference[Float] = apply(Ref.apply _)
      implicit val long: Reference[Long] = apply(Ref.apply _)
      implicit val int: Reference[Int] = apply(Ref.apply _)
      implicit val char: Reference[Char] = apply(Ref.apply _)
      implicit val short: Reference[Short] = apply(Ref.apply _)
      implicit val byte: Reference[Byte] = apply(Ref.apply _)
      implicit val boolean: Reference[Boolean] = apply(Ref.apply _)

      implicit def generic[A](implicit O: OptManifest[A]): Reference[A] = apply[A](Ref.apply[A] _)
    }
  }
}

