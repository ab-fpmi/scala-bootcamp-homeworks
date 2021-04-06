package effects

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import cats.Monad

object EffectsHomework1 {
  final class IO[A](private val run: () => A) {
    def map[B](f: A => B): IO[B] = IO(f(run()))
    def flatMap[B](f: A => IO[B]): IO[B] = IO.suspend(f(run()))
    def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)
    def as[B](newValue: => B): IO[B] = map(_ => newValue)
    def void: IO[Unit] = as(())
    def attempt: IO[Either[Throwable, A]] = IO(Try(run()).toEither)
    def option: IO[Option[A]] = attempt.map(_.toOption)
    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] =
      attempt.flatMap(_.fold(f, IO.pure))
    def redeem[B](recover: Throwable => B, map: A => B): IO[B] =
      attempt.map(_.fold(recover, map))
    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] =
      attempt.flatMap(_.fold(recover, bind))
    def unsafeRunSync(): A = run()
    def unsafeToFuture()(implicit ec: ExecutionContext): Future[A] = Future(unsafeRunSync())
  }

  object IO {
    def apply[A](body: => A): IO[A] = new IO(() => body)
    def suspend[A](thunk: => IO[A]): IO[A] = IO(thunk.run())
    def delay[A](body: => A): IO[A] = IO(body)
    def pure[A](a: A): IO[A] = IO(a)
    def fromEither[A](e: Either[Throwable, A]): IO[A] =
      e.fold(IO.raiseError, IO.pure)
    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] =
      option.fold(IO.raiseError[A](orElse))(a => IO.pure(a))
    def fromTry[A](t: Try[A]): IO[A] = t.fold(IO.raiseError, IO.pure)
    def none[A]: IO[Option[A]] = IO.pure(None)
    def raiseError[A](e: Throwable): IO[A] = IO(throw e)
    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) unit else raiseError(e)
    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) raiseError(e) else unit
    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) unit else action
    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) action else unit
    val unit: IO[Unit] = IO(())
  }

  implicit val monadIO: Monad[IO] =
    new Monad[IO] {
      override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
      override def tailRecM[A, B](a: A)(f: A => IO[Either[A, B]]): IO[B] = {
        IO.suspend {
          f(a).flatMap(_.fold(tailRecM(_)(f), pure))
        }
      }
      override def pure[A](x: A): IO[A] = IO.pure(x)
    }
}
