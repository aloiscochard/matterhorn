package matterhorn

import scala.language.existentials
import scala.language.higherKinds

trait Identity[A] {
  def run: A
}

trait Monad[M[_]] {
  def point[A](a: A): M[A]
  def map[A, B](ma: M[A])(f: A => B): M[B]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
}

sealed trait Step[K[_], O, R] {
  def map[B](f: R => B): Step[K, O, B]
}

object Step {
  case class Stop[K[_], O, R]() extends Step[K, O, R] {
    def map[B](f: R => B): Step[K, O, B] = Stop[K, O, B]
  }

  case class Yield[K[_], O, R](o: O, r: R) extends Step[K, O, R] {
    def map[B](f: R => B): Step[K, O, B] = Yield[K, O, B](o, f(r))
  }

  abstract class Await[K[_], O, R] extends Step[K, O, R] { self =>
    def g: T => R
    def kg: K[T]
    def r: R

    type T

    def map[B](f: R => B) = new Await[K, O, B] {
      type T = self.T
      def g = x => f(self.g(x))
      def kg = self.kg
      def r = f(self.r)
    }
  }
}

case class MachineT[M[_], K[_], O](run: M[Step[K, O, MachineT[M, K, O]]])

object Machine {
  import Step._
  type Machine[K[_], O] = MachineT[M forSome { type M[_] }, K, O]

  def runMachine[K[_], B](mt: MachineT[Identity, K, B])(implicit M: Monad[Identity]): Seq[B] =
    runMachineT[Identity, K, B](mt).run

  def runMachineT[M[_], K[_], B](mt: MachineT[M, K, B])(implicit M: Monad[M]): M[Seq[B]] =
    M.flatMap(mt.run)(_ match {
      case Stop()                               => M.point(Seq.empty)
      case Yield(o, k)                          => M.map(runMachineT(k))(o+:_)
      case await: Await[K, B, MachineT[M,K,B]]  => runMachineT(await.r)
    })

    /*
repeatedly :: Monad m => PlanT k o m a -> MachineT m k o
repeatedly m = r where
  r = MachineT $ runPlanT m
    (const (runMachineT r))
    (\o k -> return (Yield o (MachineT k)))
    (\f k g -> return (Await (MachineT . f) k (MachineT g)))
    (return Stop)
    */
}
