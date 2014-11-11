package matterhorn

import scala.concurrent.Future

import org.scalatest.FunSpec
import org.scalatest.concurrent.ScalaFutures
import org.scalactic.TypeCheckedTripleEquals

import IO._
import RTS.defaultRTS.unsafePerformIO_

class CoreSpec extends FunSpec with TypeCheckedTripleEquals with ScalaFutures {
  import TestUtils._

  describe("IO") {
    import Concurrent._

    it("must be stack overflow safe") {
      def foo(n: Int): Int = foo(n - 1) + 2
      def task(n: Int): IO[Unit] = if (n == 0) unitIO else unitIO *> task(n - 1)

      val size = 65535
      intercept[StackOverflowError](foo(size))

      val main = task(size)

      unsafePerformIO_(main)
    }

    it("must support thread interruption") {
      def task: IO[Unit] = unitIO *> task

      val main = for {
        thread  <- forkIO(task)
        _       <- killThread(thread)
        _       <- waitFuture(thread.future)
      } yield ()

      try {
        unsafePerformIO_(main)
      } catch {
        case e: java.util.concurrent.ExecutionException => assert(e.getCause === ThreadKilled)
      }
    }

    it("must support legacy future") {
      import Concurrent._

      val legacyFuture = Future(42)(scala.concurrent.ExecutionContext.Implicits.global)
      val main = concurrently(constIO(24), waitFuture(legacyFuture))

      assert(unsafePerformIO_(main) === (24, 42))
    }
  }

  describe("Async") {
    import Async._

    it("support wait") {
      val main = for {
        asyncA <- async(constIO(42))
        asyncB <- async(constIO(24))
        a <- await(asyncA)
        b <- await(asyncB)
      } yield (a, b)

      assert(unsafePerformIO_(main) === (42, 24))
    }

    it("support cancel") {
      var x = false

      val main = for {
        a <- async(threadDelay(1000) *> captureIO(x = true))
        _ <- Async.cancel(a)
      } yield ()

      unsafePerformIO_(main)

      assert(x === false)
    }
  }
}
