package basics

import scala.annotation.tailrec

object Basics {
  def lcm(a: Int, b: Int): Int = a*b/gcd(a, b)
  @tailrec def gcd(a: Int, b: Int): Int = if (a == 0) b else gcd(b % a, a)
}
