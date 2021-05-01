package http

import cats.data.Kleisli.ask
import cats.data.{OptionT, ReaderT}
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp, Resource, Sync}
import cats.syntax.all._
import effects.SharedStateHomework.{Cache, CacheManager}
import http.Game.GameState
import io.circe.generic.JsonCodec
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import io.circe.parser
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

    @JsonCodec
    case class  ProtocolError(error: String)
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

object WSGuessServer extends IOApp {
  import Protocol._
  import Game.GameState
  import org.http4s.dsl.io._
  import org.http4s.HttpRoutes
  import fs2.Pipe
  import fs2.concurrent.Queue
  import org.http4s.server.websocket.WebSocketBuilder
  import org.http4s.websocket.WebSocketFrame
  import org.http4s.server.blaze.BlazeServerBuilder
  import org.http4s.syntax.all._

  private val routes = HttpRoutes.of[IO] {
    case req @ GET -> Root / "ws" =>
      def gamePipe: Pipe[IO, Json, Json] =
        _.evalMapAccumulate(None: Option[GameState]) {
          case (None, j) =>
            (for {
              req <- OptionT.fromOption[IO](j.as[GameRequest.NewGame].toOption)
              game <- GameState.create[IO](req.from, req.to, 5)
              id <- OptionT.liftF(IO(UUID.randomUUID()))
            } yield (Some(game), GameResponse.GameStarted(id, game.attempts).asJson))
              .getOrElse((None, GameResponse.ProtocolError("Wrong request").asJson))

          case (Some(game), j) =>
            (for {
              req <- OptionT.fromOption[IO](j.as[GameRequest.Guess].toOption)
              (updated, response) = game.guess(req.number)
              newState = Option.when(updated.attempts > 0 && updated.number != req.number)(updated)
            } yield (newState, response.asJson))
              .getOrElse((Some(game), GameResponse.ProtocolError("Wrong request").asJson))
        }.map(_._2)

      val toJson: Pipe[IO, WebSocketFrame, Option[Json]] =
        _.collect {
          case WebSocketFrame.Text(message, _) => parser.parse(message).toOption
        }

      val fromJson: Pipe[IO, Json, WebSocketFrame] =
        _.collect(j => WebSocketFrame.Text(j.toString))

      def getOrElse[A, B](b: => B, p: Pipe[IO, A, B]): Pipe[IO, Option[A], B] = { s =>
        val s1: Pipe[IO, Option[A], B] = _.filter(_.isEmpty).map(_ => b)
        val s2: Pipe[IO, Option[A], B] = _.filter(_.isDefined).map(_.get).through(p)
        s.broadcastThrough(s1, s2)
      }

      for {
        queue <- Queue.bounded[IO, WebSocketFrame](10)
        response <- WebSocketBuilder[IO].build(
          receive = queue.enqueue,
          send = queue
            .dequeue
            .through(toJson)
            .through(getOrElse(GameResponse.ProtocolError("Wrong request").asJson, gamePipe))
            .through(fromJson)
        )
      } yield response
  }

  def run(host: String, port: Int): IO[ExitCode] =
    BlazeServerBuilder[IO](executionContext)
      .bindHttp(port, host)
      .withHttpApp(routes.orNotFound)
      .serve
      .compile
      .drain as ExitCode.Success

  override def run(args: List[String]): IO[ExitCode] = run("localhost", 9000)
}

trait AbstractGuesClient {
  import Protocol._
  import org.http4s._

  protected type ClientConfig

  protected def uri: Uri

  protected def printLine[F[_]: Sync](string: String = ""): F[Unit] = Sync[F].delay(println(string))
  protected def printJson[F[_]: Sync](j: Json) = printLine(j.toString)

  protected def request[A: Encoder, B: Decoder](cmd: String, m: A): ReaderT[IO, ClientConfig, B]

  protected def newGame(min: Int, max: Int): ReaderT[IO, ClientConfig, GameResponse.GameStarted] =
    request[GameRequest.NewGame, GameResponse.GameStarted]("new", GameRequest.NewGame(min, max))

  protected def guess(gameId: UUID, number: Int): ReaderT[IO, ClientConfig, GameResponse.GuessResponse] =
    request[GameRequest.Guess, GameResponse.GuessResponse]("guess", GameRequest.Guess(gameId, number))

  protected def guessLoop(gameId: UUID, min: Int, max: Int, attempts: Int): ReaderT[IO, ClientConfig, GameResponse.GuessResponse] = {
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

  protected def playGame: ReaderT[IO, ClientConfig, Unit] = for {
    game <- newGame(0, 96)
    _ <- printJson[ReaderT[IO, Any, *]](game.asJson)
    guessResponse <- guessLoop(game.gameId, 0, 96, game.attempts)
    _ <- printJson[ReaderT[IO, Any, *]](guessResponse.asJson)
  } yield ()
}

object GuessClient extends IOApp with AbstractGuesClient {
  import org.http4s.Method._
  import org.http4s.client._
  import org.http4s.client.dsl.io._
  import org.http4s.syntax.all._
  import org.http4s.client.blaze.BlazeClientBuilder

  protected val uri = uri"http://localhost:9000"

  protected type ClientConfig = Client[IO]

  protected def request[A: Encoder, B: Decoder](cmd: String, m: A): ReaderT[IO, ClientConfig, B] = {
    val req = POST(m.asJson, uri / cmd)
    for {
      client <- ask[IO, ClientConfig]
      response <- ReaderT.liftF(client.expect[B](req))
    } yield response
  }

  override def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](executionContext).resource.use { client =>
      playGame.run(client)
  } as ExitCode.Success
}

object WSGuessClient extends IOApp with AbstractGuesClient {
  import org.http4s.client.jdkhttpclient.{JdkWSClient, WSConnectionHighLevel, WSFrame, WSRequest}
  import java.net.http.HttpClient
  import org.http4s.syntax.all._

  protected val uri = uri"ws://localhost:9000/ws"

  protected type ClientConfig = WSConnectionHighLevel[IO]

  protected def send[A: Encoder](m: A): ReaderT[IO, ClientConfig, Unit] = {
    val req = WSFrame.Text(m.asJson.noSpaces)
    for {
      client <- ask[IO, ClientConfig]
      _ <- ReaderT.liftF(client.send(req))
    } yield ()
  }

  protected def receive[A: Decoder]: ReaderT[IO, ClientConfig, A] = for {
    client <- ask[IO, ClientConfig]
    response <- ReaderT.liftF(client.receiveStream.collectFirst {
      case WSFrame.Text(msg, _) => msg
    }.compile.string)
  } yield parser.parse(response).toOption.get.as[A].toOption.get

  protected def request[A: Encoder, B: Decoder](cmd: String, m: A) = send(m) >> receive

  override def run(args: List[String]): IO[ExitCode] =
    Resource.eval(IO(HttpClient.newHttpClient()))
      .flatMap(JdkWSClient[IO](_).connectHighLevel(WSRequest(uri)))
      .use { client =>
        playGame.run(client)
      } as ExitCode.Success
}
