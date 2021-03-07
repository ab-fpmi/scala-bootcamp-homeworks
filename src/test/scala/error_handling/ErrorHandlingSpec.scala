package error_handling

import cats.data.Validated.Invalid
import error_handling.ErrorHandling._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ErrorHandlingSpec extends AnyFreeSpec {
  import ValidationError._

  "PaymentCardValidator should accept valid data" - {
    val data = (
      "Cardholder Name",
      "2111111111111111",
      "01/22",
      "111"
    )

    "PaymentCardValidatorOption" in {
      (PaymentCardValidatorOption.validate _).tupled(data).isDefined should be(true)
    }

    "PaymentCardValidatorEither" in {
      (PaymentCardValidatorEither.validate _).tupled(data).isRight should be(true)
    }

    "PaymentCardValidatorTry" in {
      (PaymentCardValidatorTry.validate _).tupled(data).isSuccess should be(true)
    }

    "PaymentCardValidatorValidated" in {
      (PaymentCardValidatorValidated.validate _).tupled(data).isValid should be(true)
    }
  }

  "PaymentCardValidator should reject invalid data" - {
    val data = (
      "21lkamsd*&",
      "222",
      "99/99",
      "123123"
    )

    "PaymentCardValidatorOption" in {
      (PaymentCardValidatorOption.validate _).tupled(data).isDefined should be(false)
    }

    "PaymentCardValidatorEither" in {
      val result = (PaymentCardValidatorEither.validate _).tupled(data)
      result should be(Left(InvalidCardholderName))
    }

    "PaymentCardValidatorTry" in {
      (PaymentCardValidatorTry.validate _).tupled(data).isSuccess should be(false)
    }

    "PaymentCardValidatorValidated" in {
      val result = (PaymentCardValidatorValidated.validate _).tupled(data)
      result.isValid should be(false)
      val expected = Invalid(List(
        InvalidCardholderName,
        InvalidCardNumber,
        InvalidExpirationDate,
        InvalidSecurityCode)
      )
      result.leftMap(_.toNonEmptyList.toList) should be(expected)
    }
  }

  "PaymentCardValidator should reject card with wrong number checksum" - {
    val data = (
      "Cardholder Name",
      "1111111111111111",
      "01/22",
      "111"
    )

    "PaymentCardValidatorOption" in {
      (PaymentCardValidatorOption.validate _).tupled(data).isDefined should be(false)
    }

    "PaymentCardValidatorEither" in {
      (PaymentCardValidatorEither.validate _).tupled(data).isRight should be(false)
    }

    "PaymentCardValidatorTry" in {
      (PaymentCardValidatorTry.validate _).tupled(data).isSuccess should be(false)
    }

    "PaymentCardValidatorValidated" in {
      val result = (PaymentCardValidatorValidated.validate _).tupled(data)
      result.isValid should be(false)
      val expected = Invalid(List(WrongCardNumberChecksum))
      result.leftMap(_.toNonEmptyList.toList) should be(expected)
    }
  }
}
