package effects

import cats.syntax.all._
import cats.effect.syntax.all._
import cats.effect.concurrent.Ref
import cats.effect.{Bracket, BracketThrow, Clock, Concurrent, ExitCode, Fiber, IO, IOApp, Resource, Sync, Timer}

import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 */
object SharedStateHomework extends IOApp {

  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]
    def put(key: K, value: V): F[Unit]
    def size: F[Int]
    def cleanup: F[Unit]
  }

  private[effects] class RefCache[F[_] : Sync : Clock, K, V](
    state: Ref[F, Map[K, (Long, V)]],
    expiresIn: FiniteDuration
  ) extends Cache[F, K, V] {
    def get(key: K): F[Option[V]] = for {
      now <- Clock[F].realTime(MILLISECONDS)
      m <- state.get
      a = m.get(key).flatMap {
        case (exp, value) =>
          if (exp >= now)
            Some(value)
          else
            None
      }
    } yield a

    def put(key: K, value: V): F[Unit] = for {
      expires <- Clock[F].realTime(MILLISECONDS).map(_ + expiresIn.toMillis)
      _ <- state.update(_ + (key -> (expires, value)))
    } yield ()

    def size: F[Int] = state.get.map(_.size)

    def cleanup: F[Unit] =
      Clock[F].realTime(MILLISECONDS).flatMap { now =>
        Sync[F].delay(println(s"cleanup: $now")) *>
        state.update(_.filter {
          case (key, (exp, value)) => exp >= now
        })
      }
  }

  object Cache {
    def of[F[_]: Sync : Clock, K, V](
      expiresIn: FiniteDuration
    ): F[Cache[F, K, V]] = for {
      cache <- Ref.of[F, Map[K, (Long, V)]](Map.empty).map(new RefCache[F,K,V](_, expiresIn))
    } yield cache
  }

  object CacheManager {
    def of[F[_]: Timer : Concurrent, K, V](
      expiresIn: FiniteDuration,
      checkOnExpirationsEvery: FiniteDuration
    ): Resource[F, Cache[F, K, V]] = {
      val acquire = for {
        cache <- Cache.of[F, K, V](expiresIn)
        fib <- runCleanup(checkOnExpirationsEvery, cache).start
      } yield (cache, fib)

      val release: ((Any, Fiber[F,Unit])) => F[Unit] = {
        case (_: Any, fib: Fiber[F, Unit]) => fib.cancel
      }

      Resource.make(acquire)(release).map {
        case (cache, _) => cache
      }
    }

    private def runCleanup[F[_]: Sync : Timer, K, V](period: FiniteDuration, cache: Cache[F,K,V]): F[Unit] =
      Sync[F].foreverM(Timer[F].sleep(period) *> cache.cleanup)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    CacheManager.of[IO, Int, String](10.seconds, 4.seconds).use { cache =>
      for {
        _ <- cache.put(1, "Hello")
        _ <- cache.put(2, "World")
        _ <- cache.get(1).flatMap(s => IO {
          println(s"first key $s")
        })
        _ <- cache.get(2).flatMap(s => IO {
          println(s"second key $s")
        })
        _ <- IO.sleep(12.seconds)
        _ <- cache.size.flatMap(s => IO {
          println(s">>> $s")
        })
        _ <- cache.get(1).flatMap(s => IO {
          println(s"first key $s")
        })
        _ <- cache.get(2).flatMap(s => IO {
          println(s"second key $s")
        })
        _ <- IO.sleep(12.seconds)
        _ <- cache.get(1).flatMap(s => IO {
          println(s"first key $s")
        })
        _ <- cache.get(2).flatMap(s => IO {
          println(s"second key $s")
        })
      } yield ()
    } as ExitCode.Success
  }
}
