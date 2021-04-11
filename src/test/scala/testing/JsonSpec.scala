package testing

import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalacheck.Test.Parameters

class JsonSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  import Json._
  import JsonSpec._

  implicit val params: Parameters = Parameters.default.withMinSuccessfulTests(1000)

  "parse" should "invert print" in {
    forAll(jsonGen) { json =>
      parse(print(json)) should equal(Some(json))
    }
  }
}

object JsonSpec {
  import Json._
  import Shrink._

  def lazyFrequency[T](gs: (Int, () => Gen[T])*): Gen[T] = {
    val applied = gs.filter(_._1 > 0).map { case (f, g) => (f, g()) }
    Gen.frequency(applied.toArray:_*)
  }

  def JNullGen: Gen[Json.JNull.type] = Gen.const(JNull)
  def JBooleanGen: Gen[JBoolean] = Gen.oneOf(JBoolean(false), JBoolean(true))
  def JNumberGen: Gen[JNumber] = Gen.resultOf(JNumber)
  def JStringGen: Gen[JString] = Gen.resultOf(JString)

  def JsonValueGen(depth: Int): Gen[Json] = lazyFrequency(
    (depth, () => JNumberGen),
    (depth, () => JBooleanGen),
    (depth, () => JNumberGen),
    (depth, () => JStringGen),
    (Math.min(depth, 5 - depth), () => JArrayGen(depth + 1)),
    (Math.min(depth, 5 - depth), () => JObjectGen(depth + 1))
  )

  def JArrayGen(depth: Int): Gen[JArray] = Gen.nonEmptyListOf(JsonValueGen(depth + 1)).map(l => JArray(l.toVector))

  def JObjectEntryGen(depth: Int): Gen[(String, Json)] = for {
    j <- JsonValueGen(depth + 1)
    n <- Arbitrary.arbitrary[String].suchThat(_.nonEmpty)
  } yield (n, j)

  def JObjectGen(depth: Int): Gen[JObject] = Gen.nonEmptyListOf(JObjectEntryGen(depth)).map(_.toMap).map(JObject)

  def jsonGen: Gen[Json] = JsonValueGen(1)

  implicit def shrinkJson: Shrink[Json] = Shrink {
    case JNumber(a) => shrink(a).map(JNumber)
    case JString(a) => shrink(a).map(JString)
    case JArray(a) => shrink(a).filter(_.nonEmpty).map(JArray)
    case JObject(a) => shrink(a).filter(_.nonEmpty).map(JObject)
  }
}
