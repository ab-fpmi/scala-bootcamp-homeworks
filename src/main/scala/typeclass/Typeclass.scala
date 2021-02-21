package typeclass

import scala.List

object TypeclassTask extends App {
  trait HashCode[T] {
    def hash(t: T): Int
  }

  object HashCode {
    def apply[T](implicit instance: HashCode[T]): HashCode[T] = instance
  }

  implicit class HashCodeSyntax[A](x: A) {
    def hash(implicit hashCode: HashCode[A]): Int = hashCode.hash(x)
  }

  // TODO: make an instance for String
  // TODO: write "abc".hash to check everything

  implicit val HashCodeString: HashCode[String] = _.hashCode

  println("abc".hash)
}

object Task1 {
  final case class Money(amount: BigDecimal)
  implicit val moneyOrdering: Ordering[Money] = (a, b) => a.amount.compareTo(b.amount)
}

object Task2 extends App{
  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  // TODO: create Show instance for User
  // TODO: create syntax for Show so i can do User("1", "Oleg").show

  implicit class ShowSyntax[T](x: T) {
    def show(implicit showT: Show[T]): String = showT.show(x)
  }

  implicit val ShowUser: Show[User] = u => f"User ${u.id}: ${u.name}"

  println(User("1", "Alice").show)
}

object Task3 extends App {
  type Error = String
  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  final case class User(id: String, name: String)

  // TODO: create Parse instance for User
  // TODO: create syntax for Parse so i can do "lalala".parse[User] (and get an error because it is obviously not a User)

  implicit class ParseSyntax(x: String) {
    def parse[T](implicit parser: Parse[T]): Either[Error, T] = parser.parse(x)
  }

  implicit val parseUser: Parse[User] = (s: String) => s.split(raw"\s*:\s*", 2).toList match {
    case id :: name :: Nil => Right(User(id, name))
    case _ => Left("Invalid user")
  }

  println("1: Alice".parse[User])
  println("lalala".parse[User])
}

object Task4 extends App {
  // TODO: design a typesafe equals so i can do a === b, but it won't compile if a and b are of different types
  // define the typeclass (think of a method signature)
  // remember `a method b` is `a.method(b)`

  trait Eq[T] {
    def safeEq(a: T, b: T): Boolean
  }

  implicit class EqSyntax[T](a: T) {
    def safeEq(b: T)(implicit eqT: Eq[T]): Boolean = eqT.safeEq(a, b)
    def ===(b: T)(implicit eqT: Eq[T]): Boolean = safeEq(b)
  }

  final case class User(id: String, name: String)

  implicit val EqString: Eq[String] = (a, b) => a == b
  implicit val EqUser: Eq[User] = (a, b) => a.id === b.id && a.name === b.name

  println(User("1", "Alice") === User("1", "Alice"))
  println(User("1", "Alice") === User("1", "Bob"))

  // User("1", "Alice") === "1: Alice" // will not compile
}

object AdvancedHomework extends App {
  // TODO: create a typeclass for flatMap method

  trait MyFlatMap[F[_]] {
    def myFlatMap[A, B](f: A => F[B])(a: F[A]): F[B]
  }

  implicit class MyFlatMapSyntax[A, F[_]](a: F[A]) {
    def myFlatMap[B](f: A => F[B])(implicit myFlatMapF: MyFlatMap[F]): F[B] = myFlatMapF.myFlatMap(f)(a)
  }

  implicit val MyFlatMapList: MyFlatMap[List] = new MyFlatMap[List] {
    override def myFlatMap[A, B](f: A => List[B])(a: List[A]): List[B] = a.flatMap(f)
  }

  val mapper = (a: Int) => List(a, a*a)
  println(MyFlatMapList.myFlatMap(mapper)(List(1, 2, 3)))
  println(List(1, 2, 3).myFlatMap(mapper))
}
