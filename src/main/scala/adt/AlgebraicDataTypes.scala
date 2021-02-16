package adt

// Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
// task you completed to join the bootcamp. Use your best judgement about particular data types to include
// in the solution, you can model concepts like:
//
// 1. Suit
// 2. Rank
// 3. Card
// 4. Hand (Texas or Omaha)
// 5. Board
// 6. Poker Combination (High Card, Pair, etc.)
// 7. Test Case (Board & Hands to rank)
// 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
//
// Make sure the defined model protects against invalid data. Use value classes and smart constructors as
// appropriate. Place the solution under `adt` package in your homework repository.

object AlgebraicDataTypes {
  sealed trait Suit
  object Suit {
    final case object Spades extends Suit
    final case object Clubs extends Suit
    final case object Diamonds extends Suit
    final case object Hearts extends Suit
  }

  final case class Rank private (rank: Int) extends AnyVal
  object Rank {
    def create(value: Int): Option[Rank] = if (value >= 2 && value <= 14) Some(Rank(value)) else None
  }

  final case class Card(suit: Suit, rank: Rank)

  sealed trait Hand {
    def cards: Seq[Card]
  }
  object Hand {
    final case class TexasHand private (cards: Seq[Card]) extends Hand
    object TexasHand {
      def create(cards: Seq[Card]): Option[TexasHand] = if (cards.length == 2) Some(TexasHand(cards)) else None
    }

    final case class OmahaHand private (cards: Seq[Card]) extends Hand
    object OmahaHand {
      def create(cards: Seq[Card]): Option[OmahaHand] = if (cards.length == 4) Some(OmahaHand(cards)) else None
    }
  }

  sealed trait Board {
    def cards: Seq[Card]
  }

  object Board {
    final case class TexaxBoard private (cards: Seq[Card]) extends Board
    object TexaxBoard {
      def create(cards: Seq[Card]): Option[TexaxBoard] = if (cards.length == 5) Some(TexaxBoard(cards)) else None
    }

    final case class OmahaBoard private (cards: Seq[Card]) extends Board
    object OmahaBoard {
      def create(cards: Seq[Card]): Option[OmahaBoard] = if (cards.length == 4) Some(OmahaBoard(cards)) else None
    }
  }

  sealed trait Combination {
    def cards: Seq[Card]
  }
  object Combination {
    final case class RoyalFlush private (cards: Seq[Card]) extends Combination
    object RoyalFlush {
      def create(cards: Seq[Card]): Option[RoyalFlush] = ??? // should validate combination
    }

    final case class StraightFlush private (cards: Seq[Card]) extends Combination
    object StraightFlush {
      def create(cards: Seq[Card]): Option[StraightFlush] = ??? // should validate combination
    }

    final case class FullHouse private (cards: Seq[Card]) extends Combination
    object FullHouse {
      def create(cards: Seq[Card]): Option[FullHouse] = ??? // should validate combination
    }

    final case class Flush private (cards: Seq[Card]) extends Combination
    object Flush {
      def create(cards: Seq[Card]): Option[Flush] = ??? // should validate combination
    }

    final case class Straight private (cards: Seq[Card]) extends Combination
    object Straight {
      def create(cards: Seq[Card]): Option[Straight] = ??? // should validate combination
    }

    final case class ThreeOfAKind private (cards: Seq[Card]) extends Combination
    object ThreeOfAKind {
      def create(cards: Seq[Card]): Option[ThreeOfAKind] = ??? // should validate combination
    }

    final case class TwoPair private (cards: Seq[Card]) extends Combination
    object TwoPair {
      def create(cards: Seq[Card]): Option[TwoPair] = ??? // should validate combination
    }

    final case class Pair private (cards: Seq[Card]) extends Combination
    object Pair {
      def create(cards: Seq[Card]): Option[Pair] = ??? // should validate combination
    }

    final case class HighCard private (cards: Seq[Card]) extends Combination
    object HighCard {
      def create(cards: Seq[Card]): Option[HighCard] = ??? // should validate combination
    }
  }

  final case class TestCase private (board: Board, hands: Seq[Hand])
  object TestCase {
    def create(board: Board, hands: Seq[Hand]): Option[TestCase] = board match {
      case board: Board.TexaxBoard => if (hands.forall(_.isInstanceOf[Hand.TexasHand]))
        Some(TestCase(board, hands))
      else
        None

      case board: Board.OmahaBoard => if (hands.forall(_.isInstanceOf[Hand.OmahaHand]))
        Some(TestCase(board, hands))
      else
        None
    }
  }

  // Result has the same structure as a TestCase but hands are sorted
  final case class TestResult(value: TestCase) extends AnyVal
}
