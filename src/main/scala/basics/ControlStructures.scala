package basics

import scala.io.Source

object ControlStructures {
  import Command._

  sealed trait Command

  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command

    object Divide {
      def parseArgs(args: List[String]): Either[ErrorMessage, Divide] = args match {
        case s1 :: s2 :: Nil => for {
          a <- safeStringToDouble(s1)
          b <- safeStringToDouble(s2)
        } yield Divide(a, b)

        case _ => Left(ErrorMessage("Arguments count mismatch"))
      }
    }

    object Sum {
      def parseArgs(args: List[String]): Either[ErrorMessage, Sum] = safeListToDouble(args).map(Sum(_))
    }

    object Average {
      def parseArgs(args: List[String]): Either[ErrorMessage, Average] = safeListToDouble(args).map(Average(_))
    }

    object Min {
      def parseArgs(args: List[String]): Either[ErrorMessage, Min] = safeListToDouble(args).map(Min(_))
    }

    object Max {
      def parseArgs(args: List[String]): Either[ErrorMessage, Max] = safeListToDouble(args).map(Max(_))
    }
  }

  final case class ErrorMessage(value: String)

  case class Result(value: Double)

  def safeStringToDouble(s: String): Either[ErrorMessage, Double] = {
    try {
      Right(s.toDouble)
    } catch {
      case e: RuntimeException => Left(ErrorMessage(e.getMessage))
    }
  }

  def safeListToDouble(l: List[String]): Either[ErrorMessage, List[Double]] =
    l.foldLeft(Right(List()): Either[ErrorMessage, List[Double]]) {
      case (acc, el) => for {
        args <- acc
        a <- safeStringToDouble(el)
      } yield a :: args
    }.map(_.reverse)

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    x.split(raw"\s+").toList match {
      case "divide" :: args => Divide.parseArgs(args)
      case "sum" :: args => Sum.parseArgs(args)
      case "average" :: args => Average.parseArgs(args)
      case "min" :: args => Min.parseArgs(args)
      case "max" :: args => Max.parseArgs(args)
      case _ => Left(ErrorMessage(f"Can't parse expression: $x"))
    }
  }

  def calculate(x: Command): Either[ErrorMessage, Result] = x match {
    case x: Divide => if (x.divisor == 0.0)
      Left(ErrorMessage("Division by zero"))
    else
      Right(Result(x.dividend / x.divisor))

    case x: Sum => Right(Result(x.numbers.sum))

    case x: Average => if (x.numbers.isEmpty)
      Left(ErrorMessage("Empty arguments list"))
    else
      Right(Result(x.numbers.sum / x.numbers.length))

    case x: Min => if (x.numbers.isEmpty)
      Left(ErrorMessage("Empty arguments list"))
    else
      Right(Result(x.numbers.min))

    case x: Max => if (x.numbers.isEmpty)
      Left(ErrorMessage("Empty arguments list"))
    else
      Right(Result(x.numbers.max))
  }

  def renderResult(x: Result): String = x.value.toString

  def process(x: String): String = {
    val r = for {
      cmd <- parseCommand(x)
      result <- calculate(cmd)
    } yield renderResult(result)

    r.fold(l => f"Error: ${l.value}", r => r)
  }

  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
