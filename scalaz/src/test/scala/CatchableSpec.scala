package matterhorn

import org.scalatest.FunSpec

import scalaz.{ Catchable, -\/, \/- }
import IO._
import RTS.defaultRTS.unsafePerformIO_

class CatchableSpec extends FunSpec {
  import TestUtils._

  describe("Catchable[IO]") {
    import Concurrent._

    val C = Catchable[IO]
    val err = new Error("oh noes")
    val good = constIO(1)
    val bad = C.fail[Int](err)

    it("must throw exceptions captured via fail()") {
      try {
        unsafePerformIO_(bad)
        fail("should have thrown")
      } catch {
        case t: Throwable => assert(t == err)
      }
    }

    it("must catch exceptions captured via fail()") {
      val r = unsafePerformIO_(C.attempt(bad))
      assert(r == -\/(err))
    }

    it("must catch ambient exceptions in map") {
      val a = good.map[Int](_ => throw err)
      val r = unsafePerformIO_(C.attempt(a))
      assert(r == -\/(err))
    }

    it("must catch ambient exceptions in flatMap") {
      val a = good.flatMap[Int](_ => throw err)
      val r = unsafePerformIO_(C.attempt(a))
      assert(r == -\/(err))
    }

    it("must properly handle success") {
      val r = unsafePerformIO_(C.attempt(good))
      assert(r == \/-(1))
    }
  }
}
