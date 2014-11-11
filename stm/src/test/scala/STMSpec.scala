package matterhorn

import scala.concurrent.Future

import org.scalatest.FunSpec
import org.scalactic.TypeCheckedTripleEquals

import IO._
import RTS.defaultRTS.{unsafePerformIO_ => runIO}

class STMSpec extends FunSpec with TypeCheckedTripleEquals {
  import TestUtils._

  describe("STM") {
    import Concurrent._
    import STM._
    import STM.TVar._

    it("must support simple transaction") {
      val main =
        atomically(newTVar(42)).flatMap { tv =>
          duringIO_(atomically(readTVar(tv).map(_ != 0)))(atomically(modifyTVar(tv)(_ - 1))) *>
          atomically(readTVar(tv))
        }

      assert(runIO(main) === 0)
    }

    it("must support concurrent update") {
      def task(tv: TVar[Int])(p: Int => Boolean): IO[Unit] =
        duringIO_(atomically(readTVar(tv).map(_ != 0)))(
          atomically(modifyTVar(tv)(x => if (p(x)) x - 1 else x)) *> threadDelay(10)
        )

      val main =
        atomically(newTVar(42)).flatMap { tv =>
          concurrently(
            task(tv)(x => (x % 2) == 0),
            task(tv)(x => (x % 2) == 1)
          ) *>
          atomically(readTVar(tv))
        }

      assert(runIO(main) === 0)
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

      assert(runIO(main) === (42, 24))
    }

    it("support cancel") {
      var x = false

      val main = for {
        a <- async(threadDelay(1000) *> captureIO(x = true))
        _ <- Async.cancel(a)
      } yield ()

      runIO(main)

      assert(x === false)
    }
  }
}
