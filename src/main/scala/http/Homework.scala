package http

import cats.data.Kleisli.ask
import cats.data.{OptionT, ReaderT}
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.syntax.all._
import effects.SharedStateHomework.{Cache, CacheManager}
import http.Game.GameState
import io.circe.generic.JsonCodec
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.http4s.circe.CirceEntityCodec._

import java.util.UUID
import scala.concurrent.duration._
import scala.util.Random

object Protocol {
  object GameRequest {
    @JsonCodec
    final case class NewGame(from: Int, to: Int)
    @JsonCodec
    final case class Guess(gameId: UUID, number: Int)
  }

  object GameResponse {
    @JsonCodec
    case class GameStarted(gameId: UUID, attempts: Int)

    sealed trait GuessResponse

    object GuessResponse {
      final case class Lower(attempts: Int) extends GuessResponse
      final case class Greater(attempts: Int) extends GuessResponse
      final case class RightOnTheMoney(attempts: Int) extends GuessResponse
      final case class YouLose(answer: Int) extends GuessResponse
    }
  }

  implicit val guessResponseEncoder: Encoder[GameResponse.GuessResponse] = Encoder.instance {
    case GameResponse.GuessResponse.Lower(attempts) =>
      Json.obj("result" -> "Lower".asJson, "attempts" -> attempts.asJson)
    case  GameResponse.GuessResponse.Greater(attempts) =>
      Json.obj("result" -> "Greater".asJson, "attempts" -> attempts.asJson)
    case GameResponse.GuessResponse.RightOnTheMoney(attempts) =>
      Json.obj("result" -> "RightOnTheMoney".asJson, "attempts" -> attempts.asJson)
    case GameResponse.GuessResponse.YouLose(answer) =>
      Json.obj("result" -> "YouLose".asJson, "answer" -> answer.asJson)
  }

  implicit val guessResponseDecoder: Decoder[GameResponse.GuessResponse] = Decoder.instance { c =>
    c.downField("result").as[String].flatMap {
      case "Lower" => for {
        attempts <- c.downField("attempts").as[Int]
      } yield GameResponse.GuessResponse.Lower(attempts)

      case "Greater" => for {
        attempts <- c.downField("attempts").as[Int]
      } yield GameResponse.GuessResponse.Greater(attempts)

      case "RightOnTheMoney" => for {
        attempts <- c.downField("attempts").as[Int]
      } yield GameResponse.GuessResponse.RightOnTheMoney(attempts)

      case "YouLose" => for {
        answer <- c.downField("answer").as[Int]
      } yield GameResponse.GuessResponse.YouLose(answer)
    }
  }
}

object Game {
  import Protocol.GameResponse._

  case class GameState(from: Int, to: Int, number: Int, attempts: Int) {
    def guess(a: Int): (GameState, Protocol.GameResponse.GuessResponse) = {
      val result = if (a == number) {
        GuessResponse.RightOnTheMoney(attempts - 1)
      } else if (attempts <= 1) {
        GuessResponse.YouLose(number)
      } else if (number < a) {
        GuessResponse.Lower(attempts - 1)
      } else {
        GuessResponse.Greater(attempts - 1)
      }

      (copy(attempts = attempts - 1), result)
    }
  }

  object GameState {
    def create[F[_]: Sync](from: Int, to: Int, attempts: Int): OptionT[F, GameState] =
      if (from >= to || attempts <= 0)
        OptionT.fromOption(None)
      else
        OptionT(Sync[F].delay(Random.between(from, to)).map { number =>
          Some(GameState(from, to, number, attempts))
        })
  }
}

object GameStorage {
  import Game._

  trait GameStorage[F[_]] {
    def create(from: Int, to: Int, attempts: Int): OptionT[F, UUID]
    def get(id: UUID): OptionT[F, GameState]
    def modify[A](id: UUID)(fn: GameState => (GameState, A)): OptionT[F, A]
  }

  private class RefGameStorage[F[_]: Sync](ref: Ref[F, Map[UUID, GameState]]) extends GameStorage[F] {
    override def create(from: Int, to: Int, attempts: Int): OptionT[F, UUID] = for {
      state <- GameState.create(from, to, attempts)
      id <- OptionT.liftF(Sync[F].delay(UUID.randomUUID()))
      _ <- OptionT.liftF(putState(id, state))
    } yield id

    override def get(id: UUID): OptionT[F, GameState] = OptionT(ref.get.map(_.get(id)))

    private def putState(id: UUID, state: GameState): F[Unit] = ref.update(_ + (id -> state))

    override def modify[A](id: UUID)(fn: GameState => (GameState, A)): OptionT[F, A] =
      OptionT(ref.modify { state =>
        state.get(id).map(fn).map {
          case (upd, result) => (state.updated(id, upd), Some(result))
        }.getOrElse((state, None))
      })
  }

  private class CacheGameStorage[F[_]: Sync](cache: Cache[F, UUID, GameState]) extends GameStorage[F] {
    override def create(from: Int, to: Int, attempts: Int): OptionT[F, UUID] = for {
      state <- GameState.create(from, to, attempts)
      id <- OptionT.liftF(Sync[F].delay(UUID.randomUUID()))
      _ <- OptionT.liftF(cache.put(id, state))
    } yield id

    override def get(id: UUID): OptionT[F, GameState] = OptionT(cache.get(id))

    override def modify[A](id: UUID)(fn: GameState => (GameState, A)): OptionT[F, A] = OptionT(cache.modify(id)(fn))
  }

  object GameStorage {
    def of[F[_]: Sync]: F[GameStorage[F]] = Ref.of(Map.empty[UUID, Game.GameState]).map(new RefGameStorage[F](_))

    def fromCache[F[_]: Sync](cache: Cache[F, UUID, GameState]): GameStorage[F] = new CacheGameStorage(cache)
  }
}

object GuessServer extends IOApp {
  import GameStorage._
  import Protocol._
  import org.http4s.HttpRoutes
  import org.http4s.dsl.io._
  import org.http4s.server.blaze.BlazeServerBuilder
  import org.http4s.syntax.all._

  val initialAttempts = 5

  private def routes(storage: GameStorage[IO]) = HttpRoutes.of[IO] {
    // curl -XPOST "localhost:9000/new" -d '{"from": 0, "to": 100}' -H "Content-Type: application/json"
    case req @ POST -> Root / "new" => (for {
      params <- OptionT.liftF(req.as[GameRequest.NewGame])
      uuid <- storage.create(params.from, params.to, initialAttempts)
    } yield uuid).foldF(BadRequest("Invalid params"))(uuid => Ok(GameResponse.GameStarted(uuid, initialAttempts)))

    // curl -XPOST "localhost:9000/guess" -d '{"gameId": "5dc6bdc9-8193-404f-8ae7-91b92b6c9abb", "number": 50}' -H "Content-Type: application/json"
    case req @ POST -> Root / "guess" => (for {
      params <- OptionT.liftF(req.as[GameRequest.Guess])
      response <- storage.modify(params.gameId)(_.guess(params.number))
    } yield response).foldF(BadRequest("Invalid params"))(Ok(_))
  }

  def runWithStorage(host: String, port: Int)(storage: GameStorage[IO]): IO[ExitCode] =
    BlazeServerBuilder[IO](executionContext)
      .bindHttp(port, host)
      .withHttpApp(routes(storage).orNotFound)
      .serve
      .compile
      .drain as ExitCode.Success

//  def run(host: String, port: Int): IO[ExitCode] =
//    http.GameStorage.GameStorage.of[IO] >>= runWithStorage(host, port)

  def run(host: String, port: Int): IO[ExitCode] =
    CacheManager.of[IO, UUID, GameState](10.minutes, 1.minute)
      .map(http.GameStorage.GameStorage.fromCache[IO])
      .use(runWithStorage(host, port))

  override def run(args: List[String]): IO[ExitCode] = run("localhost", 9000)
}

object GuessClient extends IOApp {
  import Protocol._
  import org.http4s.Method._
  import org.http4s.client._
  import org.http4s.client.dsl.io._
  import org.http4s.syntax.all._
  import org.http4s.client.blaze.BlazeClientBuilder

  private val uri = uri"http://localhost:9000"

  private def printLine[F[_]: Sync](string: String = ""): F[Unit] = Sync[F].delay(println(string))
  private def printJson[F[_]: Sync](j: Json) = printLine(j.toString)

  private type ClientConfig = Client[IO]

  private def newGame: ReaderT[IO, ClientConfig, GameResponse.GameStarted] = {
    val req = POST(GameRequest.NewGame(0, 100).asJson, uri / "new")
    for {
      client <- ask[IO, ClientConfig]
      response <- ReaderT.liftF(client.expect[GameResponse.GameStarted](req))
    } yield response
  }

   private def guess(gameId: UUID, number: Int): ReaderT[IO, ClientConfig, GameResponse.GuessResponse] = {
     val req = POST(GameRequest.Guess(gameId, number).asJson, uri / "guess")
     for {
       client <- ask[IO, ClientConfig]
       response <- ReaderT.liftF(client.expect[GameResponse.GuessResponse](req))
     } yield response
   }

  private def guessLoop(gameId: UUID, min: Int, max: Int, attempts: Int): ReaderT[IO, ClientConfig, GameResponse.GuessResponse] = {
    for {
      v <- if (attempts <= 1)
        ReaderT.liftF[IO, ClientConfig, Int](IO(Random.between(min, max)))
      else
        ReaderT.pure[IO, ClientConfig, Int]((min + max) / 2)

      _ <- printLine[ReaderT[IO, ClientConfig, *]](s"Maybe $v?")

      response <- guess(gameId, v)

      response <- response match {
        case response: GameResponse.GuessResponse.Lower =>
          printJson[ReaderT[IO, ClientConfig, *]](response.asInstanceOf[GameResponse.GuessResponse].asJson) >>
            guessLoop(gameId, min, v-1, response.attempts)

        case response: GameResponse.GuessResponse.Greater =>
          printJson[ReaderT[IO, ClientConfig, *]](response.asInstanceOf[GameResponse.GuessResponse].asJson) >>
            guessLoop(gameId, v+1, max, response.attempts)

        case response => ReaderT.pure[IO, ClientConfig, GameResponse.GuessResponse](response)
      }
    } yield response
  }

  private def playGame: ReaderT[IO, ClientConfig, Unit] = for {
    game <- newGame
    _ <- printJson[ReaderT[IO, Any, *]](game.asJson)
    guessResponse <- guessLoop(game.gameId, 0, 96, game.attempts)
    _ <- printJson[ReaderT[IO, Any, *]](guessResponse.asJson)
  } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](executionContext).resource.use { client => for {
      _ <- playGame.run(client)
    } yield ()
  } as ExitCode.Success
}
