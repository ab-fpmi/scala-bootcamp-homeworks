package testing

/** Simple calculator with buttons.
 *
 * @param memory whatever is stored in the memory.
 * @param screen whatever you see on the screen.
 */
case class Calculator(memory: Int = 0, screen: Int = 0, operation: Option[Calculator.Operation] = None) {
  import Calculator._

  def enter(digit: Int): Either[String, Calculator] =
    if (digit >= 0 && digit <= 9) {
      Right(copy(screen = screen * 10 + digit))
    } else {
      Left("digit out of range")
    }

  def plus: Calculator = calculate match {
    case Calculator(mem, scr, op) => Calculator(scr, 0, Some(Operation.Plus))
  }

  def minus: Calculator = calculate match {
    case Calculator(mem, scr, op) => Calculator(scr, 0, Some(Operation.Minus))
  }

  def calculate: Calculator = operation.fold(this) {
    case Operation.Plus => Calculator(screen = screen + memory)
    case Operation.Minus => Calculator(screen =  memory - screen)
  }
}
object Calculator {
  sealed trait Operation
  object Operation {
    object Plus extends Operation
    object Minus extends Operation
  }
}
