package basics

import scala.annotation.tailrec

object Basics {
  def lcm(a: Int, b: Int): Option[Int] = if (a == 0 && b == 0) None else Some(a*b/gcd(a, b))
  @tailrec def gcd(a: Int, b: Int): Int = if (a == 0) b else gcd(b % a, a)
}
