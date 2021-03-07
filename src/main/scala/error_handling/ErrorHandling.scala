package error_handling

import cats._
import cats.data.{NonEmptyChain, ValidatedNec}
import cats.implicits._

import java.time.format.DateTimeFormatter
import java.time.YearMonth
import scala.util.Try

object ErrorHandling {
  object Types {
    final case class CardholderName(value: String) extends AnyVal
    final case class CardNumber(value: String) extends AnyVal
    final case class ExpirationDate(value: YearMonth) extends AnyVal
    final case class SecurityCode(value: String) extends AnyVal
  }

  case class PaymentCard(
    name: Types.CardholderName,
    number: Types.CardNumber,
    expirationDate: Types.ExpirationDate,
    securityCode: Types.SecurityCode
  )

  sealed trait ValidationError
  object ValidationError {
    case object InvalidCardNumber extends ValidationError
    case object WrongCardNumberChecksum extends ValidationError
    case object InvalidCardholderName extends ValidationError
    case object InvalidExpirationDate extends ValidationError
    case object InvalidSecurityCode extends ValidationError
  }

  trait Validator[F[_], E, T] {
    implicit def ae: ApplicativeError[F, E]
    def error(e: T): E
    def raiseError[V](e: T): F[V] = ae.raiseError(error(e))
    def pure[V](v: V): F[V] = v.pure[F]
  }

  trait ValidatedNecValidator[T] {
    implicit def ae =
      implicitly[ApplicativeError[ValidatedNec[T, *], NonEmptyChain[T]]]

    def error(e: T): NonEmptyChain[T] = NonEmptyChain(e)
  }

  trait EitherValidator[T] {
    implicit def ae = implicitly[ApplicativeError[Either[T, *], T]]
    def error(e: T): T = e
  }

  trait TryValidator[T] {
    implicit def ae = implicitly[ApplicativeError[Try, Throwable]]
    def error(e: T): Throwable = new Throwable(e.toString)
  }

  trait OptionValidator[T] {
    implicit def ae = implicitly[ApplicativeError[Option, Unit]]
    def error(e: T): Unit = ()
  }

  trait PaymentCardValidator[F[_], E] extends Validator[F, E, ValidationError] {
    import Types._
    import ValidationError._

    def validateCardholderName(name: String): F[CardholderName] =
      if (name.matches(raw"^[A-Za-z ]{3,48}"))
        pure(CardholderName(name))
      else
        raiseError(InvalidCardholderName)

    def validateCardNumber(number: String): F[CardNumber] =
      if (number.matches(raw"\d{16}"))
        pure(CardNumber(number))
      else
        raiseError(InvalidCardNumber)

    // Let's pretend we're validating checksum somehow
    // Say, any number started with 1 is invalid, others are all valid
    def validateCardNumberChecksum(number: String): F[CardNumber] =
      if (number.startsWith("1"))
        raiseError(WrongCardNumberChecksum)
      else
        pure(CardNumber(number))

    def validateExpirationDate(date: String): F[ExpirationDate] =
      Try(YearMonth.parse(date, DateTimeFormatter.ofPattern("MM/yy"))).fold(
        _ => raiseError(InvalidExpirationDate),
        d => pure(ExpirationDate(d))
      )

    def validateSecurityCode(code: String): F[SecurityCode] =
      if (code.matches(raw"\d{3}"))
        pure(SecurityCode(code))
      else
        raiseError(InvalidSecurityCode)

    def validate(
      name: String,
      number: String,
      expirationDate: String,
      securityCode: String
    ): F[PaymentCard] =
      (PaymentCard.apply _).curried.pure[F] <*>
        validateCardholderName(name) <*>
        (validateCardNumber(number) *> validateCardNumberChecksum(number)) <*>
        validateExpirationDate(expirationDate) <*>
        validateSecurityCode(securityCode)
  }

  /**
   * That's the point: now it's possible to make validators using different validation models
   * without changing validation logic
   */

  object PaymentCardValidatorValidated
    extends PaymentCardValidator[ValidatedNec[ValidationError, *], NonEmptyChain[ValidationError]]
    with ValidatedNecValidator[ValidationError]

  object PaymentCardValidatorEither
    extends PaymentCardValidator[Either[ValidationError, *], ValidationError]
    with EitherValidator[ValidationError]

  object PaymentCardValidatorTry
    extends PaymentCardValidator[Try, Throwable]
    with TryValidator[ValidationError]

  object PaymentCardValidatorOption
    extends PaymentCardValidator[Option, Unit]
    with OptionValidator[ValidationError]
}
