package testing

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._

class CalculatorSpec extends AnyFlatSpec {
  import CalculatorSpec._

  "Calculator" should "enter digit" in {
    forAll(genCalculator, Gen.choose(0, 9)) { (c: Calculator, d: Int) =>
      c.enter(d) match {
        case Left(_) => assert(false)
        case Right(Calculator(mem, screen, op)) =>
          mem should be(c.memory)
          screen should be(c.screen * 10 + d)
          op should be(c.operation)
      }
    }
  }

  "Calculator" should "refuse to enter anything but single digit" in {
    forAll { (c: Calculator, d: Int) =>
      whenever(d > 9 || d < 0) {
        c.enter(d) should be a Symbol("isLeft")
      }
    }
  }

  "Calculator" should "add numbers" in {
    forAll(genCalculatorWithoutOperation) { c: Calculator =>
      val c2 = c.copy(operation = Some(Calculator.Operation.Plus)).calculate
      c2.screen should be(c.memory + c.screen)
      c2.memory should be(0)
      c2.operation should be(None)
    }
  }

  "Calculator" should "subtract numbers" in {
    forAll(genCalculatorWithoutOperation) { c: Calculator =>
      val c2 = c.copy(operation = Some(Calculator.Operation.Minus)).calculate
      c2.screen should be(c.memory - c.screen)
      c2.memory should be(0)
      c2.operation should be(None)
    }
  }

  "Calculator.calculate" should "do nothing if there is no operation" in {
    forAll(genCalculatorWithoutOperation) { c: Calculator =>
      c.calculate should be(c)
    }
  }

  "Calculator w/o operation" should "move screen value to memory when hit plus or minus" in {
    forAll(genCalculatorWithoutOperation) { c: Calculator =>
      c.plus.memory should be(c.screen)
      c.plus.screen should be(0)
      c.minus.memory should be(c.screen)
      c.minus.screen should be(0)
    }
  }

  "Calculator w/ operation" should "calculate value and then move result to memory when hit plus or minus" in {
    forAll(genCalculatorWithOperation) { c: Calculator =>
      c.plus.memory should be(c.calculate.screen)
      c.plus.screen should be(0)
      c.minus.memory should be(c.calculate.screen)
      c.minus.screen should be(0)
    }
  }
}

object CalculatorSpec {
  import Gen._
  import Arbitrary._
  import Calculator._

  val genOperation: Gen[Operation] = oneOf(Operation.Plus, Operation.Minus)
  implicit val arbitraryOperation: Arbitrary[Operation] = Arbitrary(genOperation)

  val genCalculator: Gen[Calculator] = for {
    mem <- arbitrary[Int]
    screen <- arbitrary[Int]
    op <- arbitrary[Option[Operation]]
  } yield Calculator(mem, screen, op)

  implicit val arbitraryCalculator: Arbitrary[Calculator] = Arbitrary(genCalculator)

  val genCalculatorWithOperation: Gen[Calculator] = for {
    mem <- arbitrary[Int]
    screen <- arbitrary[Int]
    op <- arbitrary[Operation]
  } yield Calculator(mem, screen, Some(op))

  val genCalculatorWithoutOperation: Gen[Calculator] = for {
    mem <- arbitrary[Int]
    screen <- arbitrary[Int]
  } yield Calculator(mem, screen, None)
}
