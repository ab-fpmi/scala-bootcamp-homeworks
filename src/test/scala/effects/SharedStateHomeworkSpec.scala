package effects

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import cats.effect.{ContextShift, IO, Resource, Timer}

import scala.concurrent.duration._
import SharedStateHomework._
import org.scalatest.concurrent.Eventually
import org.scalatest.concurrent.ScalaFutures.convertScalaFuture

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class SharedStateHomeworkSpec extends AnyFlatSpec with Eventually {
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global
  implicit val cs: ContextShift[IO] = IO.contextShift(ec)
  implicit val timer: Timer[IO] = IO.timer(ec)

  def ioCache: Resource[IO, CacheManager[IO, Int, String]] = CacheManager.of[IO, Int, String](100.millis, 40.millis)

  "Cache" should "return stored value" in {
    val t = ioCache.use {
      case CacheManager(cache, _) =>
        for {
        _ <- cache.put(0, "foo")
        _ <- cache.put(1, "bar")
        a <- cache.get(0)
        b <- cache.get(1)
      } yield (a, b)
    }

    t.unsafeToFuture().futureValue should equal((Some("foo"), Some("bar")))
  }

  "Cache" should "not return expired values" in {
    val t = ioCache.use {
      case CacheManager(cache, _) =>
        for {
          _ <- cache.put(0, "foo")
          _ <- IO.sleep(120.millis)
          a <- cache.get(0)
        } yield a
    }

    t.unsafeToFuture().futureValue shouldBe None
  }

  "Expired values" should "be removed from cache" in {
      val t = ioCache.use {
        case CacheManager(cache, _) =>
          for {
            _ <- cache.put(0, "foo")
            _ <- IO.sleep(50.millis)
            _ <- cache.put(1, "bar")
            _ <- IO.sleep(70.millis)
            s <- cache.size
          } yield s
      }

      t.unsafeToFuture().futureValue shouldBe 1
  }
}
