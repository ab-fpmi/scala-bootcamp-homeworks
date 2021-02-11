package basics

object Collections {
  def runningSum(nums: List[Int]): List[Int] = nums match {
    case Nil => Nil
    case head :: tail => tail.foldLeft(List(head)) {
      case (sum, el) => (sum.head + el) :: sum
    }.reverse
  }

  def shuffle(nums: List[Int]): List[Int] = {
    val first = nums
    val second = nums.drop(nums.length / 2)
    first.zip(second).flatMap {
      case (a, b) => List(a, b)
    }
  }

  def maximumWealth(accounts: List[List[Int]]): Int = {
    accounts.map(_.sum).max
  }

  def kidsWithCandies(candies: List[Int], extraCandies: Int): List[Boolean] = {
    val max = candies.max
    candies.map(_ + extraCandies >= max)
  }

  def maxWidthOfVerticalArea(points: List[(Int, Int)]): Int = {
    val xl = points.map(_._1).sorted
    xl.zip(xl.tail).map(a => a._2 - a._1).max
  }
}
