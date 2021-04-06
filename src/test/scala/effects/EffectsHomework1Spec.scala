package effects

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import EffectsHomework1._
import cats.Monad

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class EffectsHomework1Spec extends AnyFreeSpec {
  import EffectsHomework1Spec._

  "Effect should be delayed unless unsafeRunSync is called" - {
    "IO.apply" in {
      val f = MockEffect()
      val iof = IO(f("foo"))

      assert(f.notCalled)
      iof.unsafeRunSync()
      assert(f.calledOnce)
    }

    "IO.delay" in {
      val f = MockEffect()
      val iof = IO.delay(f("foo"))

      assert(f.notCalled)
      iof.unsafeRunSync()
      assert(f.calledOnce)
    }

    "IO.suspend" in {
      val f = MockEffect()
      val iof = IO.delay(f("foo"))
      val iog = IO.suspend(iof)

      assert(f.notCalled)
      iog.unsafeRunSync()
      assert(f.calledOnce)
    }
  }

  "IO.pure injects value" in {
    val ioa = IO.pure("foo")
    ioa.unsafeRunSync() should equal("foo")
  }

  "IO.unit injects unit" in {
    IO.unit.unsafeRunSync() should equal(())
  }

  "IO::flatMap composes IO" in {
    val f = MockEffect()
    val iof = IO(f("foo")).flatMap(s => IO(f(s + "bar")))

    assert(f.notCalled)
    iof.unsafeRunSync()
    assert(f.calledExact(Seq("foo", "foobar")))
  }

  "IO::map applies function to IO value" in {
    val f = MockEffect()
    val iof = IO(f("foo")).map(_ + "bar")
    assert(f.notCalled)
    iof.unsafeRunSync() should equal("foobar")
    assert(f.calledExact(Seq("foo")))
  }

  "Error handling" - {
    "IO.raiseError throws" in {
      val f = MockEffect()
      val iof = IO.raiseError(new RuntimeException("bang!")) *> IO(f("not called"))
      assertThrows[RuntimeException] {
        iof.unsafeRunSync()
      }
      assert(f.notCalled)
    }

    "IO::handleErrorWith maps errors" - {
      "error" in {
        val f = MockEffect()
        val iof = IO.raiseError(new RuntimeException("bang!")) *> IO(f("not called"))
        iof.handleErrorWith(_ => IO.pure("handled")).unsafeRunSync() should equal("handled")
        assert(f.notCalled)
      }

      "success" in {
        val f = MockEffect()
        val iof = IO(f("success"))
        iof.handleErrorWith(_ => IO.pure("handled")).unsafeRunSync() should equal("success")
        assert(f.calledOnceWith("success"))
      }
    }

    "IO::attempt turns errors to Either" - {
      "error" in {
        val f = MockEffect()
        val iof = IO.raiseError(new RuntimeException("bang!")) *> IO(f("not called"))
        iof.attempt.unsafeRunSync() should be(Symbol("left"))
        assert(f.notCalled)
      }

      "success" in {
        val f = MockEffect()
        val iof = IO(f("success"))
        iof.attempt.unsafeRunSync() should equal(Right("success"))
        assert(f.calledOnceWith("success"))
      }
    }

    "IO::option turns errors to None" - {
      "error" in {
        val f = MockEffect()
        val iof = IO.raiseError(new RuntimeException("bang!")) *> IO(f("not called"))
        iof.option.unsafeRunSync() should be(Symbol("empty"))
        assert(f.notCalled)
      }

      "success" in {
        val f = MockEffect()
        val iof = IO(f("success"))
        iof.option.unsafeRunSync() should equal(Some("success"))
        assert(f.calledOnceWith("success"))
      }
    }

    "IO::redeem maps errors" - {
      "error" in {
        val f = MockEffect()
        val iof = IO.raiseError(new RuntimeException("bang!")) *> IO(f("not called"))
        iof.redeem(_ => "foo", identity).unsafeRunSync() should equal("foo")
        assert(f.notCalled)
      }

      "success" in {
        val f = MockEffect()
        val iof = IO(f("success"))
        iof.redeem(_ => "foo", identity).unsafeRunSync() should equal("success")
        assert(f.calledOnceWith("success"))
      }
    }

    "IO::redeemWith maps errors" - {
      "error" in {
        val f = MockEffect()
        val iof = IO.raiseError(new RuntimeException("bang!")) *> IO(f("not called"))
        iof.redeemWith(_ => IO.pure("foo"), IO.pure).unsafeRunSync() should equal("foo")
        assert(f.notCalled)
      }

      "success" in {
        val f = MockEffect()
        val iof = IO(f("success"))
        iof.redeemWith(_ => IO.pure("foo"), IO.pure).unsafeRunSync() should equal("success")
        assert(f.calledOnceWith("success"))
      }
    }
  }

  "IO is a" - {
    "Monad" in {
      val m = implicitly[Monad[IO]]
    }
  }
}

object EffectsHomework1Spec {
  class MockEffect {
    var history: ListBuffer[String] = mutable.ListBuffer[String]()

    def apply(param: String): String = {
      history += param
      param
    }

    def calledOnce = history.length == 1
    def called = history.nonEmpty
    def notCalled = !called
    def calledOnceWith(param: String) = calledOnce && history.head == param
    def calledWith(param: String) = history.count(_ == param) > 0
    def calledExact(params: Seq[String]) =
      params.length == history.length && params.zip(history).forall {
        case (left, right) => left == right
      }
  }

  object MockEffect {
    def apply() = new MockEffect()
  }
}
