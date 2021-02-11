package basics

import org.scalatest.flatspec.AnyFlatSpec
import Collections._
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class CollectionsSpec extends AnyFlatSpec {
  "runningSum" should "work" in {
    runningSum(List(1, 2, 3, 4)) shouldEqual List(1, 3, 6, 10)
    runningSum(Nil) shouldEqual Nil
  }

  "shuffle" should "work" in {
    shuffle(List(2, 5, 1, 3, 4, 7)) shouldEqual List(2, 3, 5, 4, 1, 7)
    shuffle(Nil) shouldEqual Nil
  }

  "maximumWealth" should "work" in {
    maximumWealth(List(List(1, 2, 3), List(3, 2, 1))) shouldEqual 6
    maximumWealth(List(List(1, 5), List(7, 3), List(3, 5))) shouldEqual 10
  }

  "kidsWithCandies" should "work" in {
    kidsWithCandies(List(2, 3, 5, 1, 3), 3) shouldEqual List(true, true, true, false, true)
    kidsWithCandies(List(4, 2, 1, 1, 2), 1) shouldEqual List(true, false, false, false, false)
  }

  "maxWidthOfVerticalArea" should "work" in {
    maxWidthOfVerticalArea(List((8, 7), (9, 9), (7, 4), (9, 7))) shouldEqual 1
    maxWidthOfVerticalArea(List((3, 1), (9, 0), (1, 0), (1, 4), (5, 3), (8, 8))) shouldEqual 3
  }
}
