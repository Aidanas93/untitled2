import data._

import scala.util.Random

object Main extends App {
  val suites = List(Spade, Heart, Club, Diamond)
  val ranks = List(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)

  val shuffledDeck = ShuffledDeck() match {
    case Some(shuffledDeck) => shuffledDeck
    case None => throw new IllegalArgumentException("Illegal deck")
  }
  val game = Game(shuffledDeck)
  val players = game.play()
  println(players._1.scorePile.size)
  println(players._2.scorePile.size)

  case class Card(rank: Rank, suite: Suite) {
    def getCardValue(trumpCard: Suite): Int = {
      val trumpDefaultValue: Int = 100
      val defaultCurrentCardValue = ranks.indexOf(rank) + 2

      if (trumpCard == suite) defaultCurrentCardValue + trumpDefaultValue else defaultCurrentCardValue
    }
  }

  case class ShuffledDeck(deck: List[Card]) {
    val shuffledDeck = Random.shuffle(deck)

    def split(): (List[Card], List[Card]) = shuffledDeck.splitAt(shuffledDeck.size / 2)
  }

  object ShuffledDeck {
    def apply(): Option[ShuffledDeck] = {
      val deck = for (r <- ranks; s <- suites) yield Card(r, s)
      if (deck.size <= 52 && deck.distinct.size == deck.size) Some(new ShuffledDeck(deck))
      else None
    }
  }

  case class Player(hand: List[Card], scorePile: List[Card]) {
    def addToTop(cardsToAdd: List[Card]): Player = this.copy(scorePile = cardsToAdd ::: scorePile)
  }

  case class Game(shuffledDeck: ShuffledDeck) {
    val trumpCard = suites(new Random().nextInt(suites.length))
    val (playerOneCards, playerTwoCards) = shuffledDeck.split()
    val playerOne = Player(playerOneCards, List.empty)
    val playerTwo = Player(playerTwoCards, List.empty)

    def play(): (Player, Player) = {
      val battles: List[(Card, Card)] = playerOne.hand zip playerTwo.hand

      val playersScorePiles = battles.foldLeft((playerOne, playerTwo))((acc, x) =>
        if (x._1.getCardValue(trumpCard) > x._2.getCardValue(trumpCard)) {
          (acc._1.addToTop(List(x._1, x._2)), acc._2)
        }
        else if (x._1.getCardValue(trumpCard) < x._2.getCardValue(trumpCard)) {
          (acc._1, acc._2.addToTop(List(x._1, x._2)))
        }
        else {
          (acc._1.addToTop(List(x._1)), acc._2.addToTop(List(x._2)))
        }
      )

      playersScorePiles
    }
  }

}