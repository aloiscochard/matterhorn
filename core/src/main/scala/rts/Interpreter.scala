package matterhorn
package rts

import scala.concurrent.{ExecutionContext, Future}

import RTS._

object Interpreter {
  def unsafePerformEval(rts: RTS)(thunk: Thunk): Future[Val] =
    unsafePerformEval_(thunk, Val.unit, Nil, rts.intr)(rts.ec)

  def unsafePerformEval_(
    thunk0: List[Exp],
    value0: Val,
    stack0: List[Either[List[Exp], Future[Val => Val]]],
    intr: Interruptor)
  (implicit ec: ExecutionContext): Future[Val] = {

    import Exp._
    import Val._

    type Continuation = Future[Val => Val]
    type Frame = Either[Thunk, Continuation]

    var thunk: List[Exp] = thunk0
    var value: Val = value0
    var stack: List[Frame] = stack0
    var next: Option[Continuation] = None

    var done: Boolean = false

    def die = throw ThreadKilled

    while(!done) {
      def thunkEmpty = thunk.tail.isEmpty

      def setVal(x: Val): Unit = {
        done = thunkEmpty
        value = x
        if (!thunkEmpty) thunk = thunk.tail
      }

      def pushC(x: Continuation): Unit = {
        stack = Right(x) :: stack
      }

      def pushT: Unit =
        if (!thunkEmpty) stack = Left(thunk.tail) :: stack

      val tail = if (thunk.tail.nonEmpty) Some(thunk.tail) else None

      thunk.head match {
        case Point(f) => setVal(f(()))
        case Map(f)   => setVal(f(value))

        case Fork(f)  => setVal {
          val intrChild = intr.newChild
          def run = unsafePerformEval_(f(()).reverse, unit, Nil, intrChild)
          cast(ThreadId(Future(run).flatMap(identity), intrChild))
        }

        case Bind(f) =>
          pushT
          thunk = f(value).reverse

        case Apply(f, left, right) =>
          pushT
          thunk = left.reverse
          pushC(
            Future(unsafePerformEval_(right.reverse, unit, Nil, intr))
              .flatMap(_.map(r => ((l: Val) => f(l, r))))
          )

        case Wait(ThreadId(future, tintr)) =>
          done = true
          if (tintr.interrupted) {
            stack = Nil
            thunk = Nil
            next = Some(Future(die))
          } else {
            pushT
            thunk = Nil
            pushC(future.map(x => (_: Val) => x))
          }
      }

      // TODO Performance analysis of the volatile in the intr
      if ((!done || (done && stack.nonEmpty)) && intr.interrupted) die

      if (done && stack.nonEmpty) {
        stack.head match {
          case Left(head) =>
            done = false
            thunk =  head
          case Right(future) =>
            next = Some(future)
        }
        stack = stack.tail
      }
    }

    next.fold(Future.successful(value)) { future =>
      stack match {
        case Left(nextThunk)::tail => future.flatMap(f => unsafePerformEval_(nextThunk, f(value), tail, intr))
        case _                => future.map(f => f(value))
      }
    }
  }
}
