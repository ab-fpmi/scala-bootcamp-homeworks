package effects

import cats.effect.concurrent.Ref
import cats.{Monad, Parallel}
import cats.syntax.all._
import cats.effect.{Blocker, Bracket, ExitCode, IO, IOApp, Sync}
import scala.io.{Source, StdIn}
import scala.util.Try

/*
  Additional assignment:
  1. Read from the console the file path.
    1.1 Use Blocking Thread Pool
    1.2 Check the transmitted data(Error Handling + Validation).
  2. Read from the console the seed.
    2.1 Use Blocking Thread Pool
    2.2 Check the transmitted data(Error Handling + Validation).
  3. Read the data from the file.
  4. Calculate the signature (in parallel if possible).
    4.1 Use Separate Thread Pool(ContextShift)
    4.2 Split text into words
    4.3 Calculate hash for each word
    4.4 Take the minimal hash
    4.5* Repeat the process for n different hash functions.
  5. Save the signature in memory(think about storage).
  6. Terminate the application.
 */

object MinHash extends IOApp {
  import Implementations._

  val console: Console[IO] = Console[IO]()
  val fileReader: FileReader[IO] = FileReader[IO]()
  val hasher: Hasher[IO] = Hasher[IO]()

  override def run(args: List[String]): IO[ExitCode] =
    Blocker[IO].use(runWithBlocker) as ExitCode.Success

  def runWithBlocker(blocker: Blocker): IO[Unit] = for {
    storage <- Storage.of[IO, String, Int]
    name <- blocker.blockOn(askFilename)
    seed <- blocker.blockOn(askSeed)
    words <- readWords(name)
    hash <- hasher.minHash(words, seed)
    _ <- storage.put(name, hash)
    check <- storage.get(name).map(_.getOrElse(0))
    _ <- console.write(f">>> $check")
  } yield ()

  def askFilename: IO[String] = for {
    _ <- console.write("Enter file name:")
    unvalidated <- console.read()
    name <- Validator.validateFileName(unvalidated).fold(e => reportError(e) *> askFilename, IO.pure)
  } yield name

  def askSeed: IO[Int] = for {
    _ <- console.write("Enter seed:")
    unvalidated <- console.read()
    seed <- Validator.validateSeed(unvalidated).fold(e => reportError(e) *> askSeed, IO.pure)
  } yield seed

  def readWords(filename: String): IO[List[String]] =
    fileReader.readLines(filename).map(_.flatMap(_.split(raw"\s+")))

  def reportError(error: ValidationError): IO[Unit] = error match {
    case ValidationError.InvalidFileName => console.write("Invalid file name")
    case ValidationError.InvalidSeed => console.write("Invalid seed")
  }
}

object Implementations {
  trait Console[F[_]] {
    def read(): F[String]
    def write(s: String): F[Unit]
  }

  object Console {
    def apply[F[_] : Sync](): Console[F] = new Console[F] {
      override def read(): F[String] = Sync[F].delay(StdIn.readLine())
      override def write(s: String): F[Unit] = Sync[F].delay(println(s))
    }
  }

  trait FileReader[F[_]] {
    def readLines(filename: String): F[List[String]]
  }

  object FileReader extends {
    def apply[F[_]: Sync : Bracket[*[_], Throwable]](): FileReader[F] = (filename: String) =>
      Bracket[F, Throwable].bracket(Sync[F].delay(Source.fromFile(filename))) { file =>
        Sync[F].delay(file.getLines().toList)
      } { file =>
        Sync[F].delay(file.close())
      }
  }

  sealed trait ValidationError
  object ValidationError {
    case object InvalidFileName extends ValidationError
    case object InvalidSeed extends ValidationError
  }

  object Validator {
    import ValidationError._

    def validateFileName(s: String): Either[ValidationError, String] = Either.cond(s.nonEmpty, s, InvalidFileName)

    def validateSeed(s: String): Either[ValidationError, Int] =
      Try(s.toInt).fold(_ => Left(ValidationError.InvalidSeed), Right(_))
  }

  trait Hasher[F[_]] {
    def minHashWord(word: String, seed: Int): F[Int]
    def minHash(words: List[String], seed: Int): F[Int]
  }

  object Hasher {
    def apply[F[_]: Sync : Monad : Parallel](): Hasher[F] = new Hasher[F] {
      override def minHashWord(word: String, seed: Int): F[Int] =
        Sync[F].delay(Math.min(javaHash(word, seed), knuthHash(word, seed)))

      override def minHash(words: List[String], seed: Int): F[Int] =
        words.map(minHashWord(_, seed)).parSequence.map(_.min)
    }

    def javaHash(word: String, seed: Int = 0): Int = {
      var hash = 0
      for (ch <- word.toCharArray)
        hash = 31 * hash + ch.toInt
      hash = hash ^ (hash >> 20) ^ (hash >> 12)
      hash ^ (hash >> 7) ^ (hash >> 4)
    }

    def knuthHash(word: String, constant: Int): Int = {
      var hash = 0
      for (ch <- word.toCharArray)
        hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
      hash % constant
    }
  }

  trait Storage[F[_], K, V] {
    def put(k: K, v: V): F[Unit]
    def get(k: K): F[Option[V]]
  }

  object Storage {
    class MapStorage[F[_]: Monad, K, V](ref: Ref[F, Map[K, V]]) extends Storage[F, K, V] {
      override def put(k: K, v: V): F[Unit] = ref.update(_ + (k -> v))
      override def get(k: K): F[Option[V]] = ref.get.map(_.get(k))
    }

    def of[F[_]: Sync, K, V]: F[MapStorage[F, K, V]] =
      Ref.of[F, Map[K, V]](Map.empty).map(new MapStorage[F,K,V](_))
  }
}
