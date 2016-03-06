package org.leanpoker.player

import com.google.gson.{JsonObject, JsonElement}
import scala.collection.JavaConversions._
import scala.util.{Random, Try}

case class Card(rank: Char, suit: String)  {
  def getRankInt: Int = {
    val rankToNumMap = Map(
      'A' -> 13,
      'K' -> 12,
      'Q' -> 11,
      'J' -> 10,
      '1' -> 9,
      '9' -> 8,
      '8' -> 7,
      '7' -> 6,
      '6' -> 5,
      '5' -> 4,
      '4' -> 3,
      '3' -> 2,
      '2' -> 1
    )
    rankToNumMap(rank)
  }
}

trait TargetCombination {
  def combRank: Int
  def cardsOnHand: Int
}
case class Pair(rank: Char) { def combRank = 1 }
case class TwoPairs(rank1: Char, rank2: Char) { def combRank = 2 }
case class Three(rank: Char) { def combRank = 3 }
case class Street(rank: Char) { def combRank = 4 }
case class Color(suit: String) { def combRank = 5 }


case class BetParams(myCards: List[Card],
                     communityCards: List[Card],
                     betIdx: Int,
                     buyIn: Int,
                     stack: Int,
                     bet: Int,
                     smallBlind: Int)



object Player {

  val VERSION = "Fixxxx"

  val MyName = "Pony"


  def parseCard(cardJson: JsonObject): Card = {
    Card(
      cardJson.get("rank").getAsString.head,
      cardJson.get("suit").getAsString
    )
  }

  def getMyCards(request: JsonElement): List[Card] = {
    val players = request.getAsJsonObject.get("players").getAsJsonArray.map(_.getAsJsonObject)
    val myPlayerOpt = players.find(_.get("name").getAsString == MyName)
    myPlayerOpt match {
      case Some(myPlayer) =>
        val myCards = myPlayer.get("hole_cards").getAsJsonArray.map(_.getAsJsonObject).toList
        myCards.map(parseCard)
      case None =>
        List.empty
    }
  }

  def getMyStack(request: JsonElement): Int = {
    val players = request.getAsJsonObject.get("players").getAsJsonArray.map(_.getAsJsonObject)
    val myPlayerOpt = players.find(_.get("name").getAsString == MyName)
    myPlayerOpt match {
      case Some(myPlayer) =>
        myPlayer.get("stack").getAsInt
      case None =>
        0
    }
  }

  def getMyBet(request: JsonElement): Int = {
    val players = request.getAsJsonObject.get("players").getAsJsonArray.map(_.getAsJsonObject)
    val myPlayerOpt = players.find(_.get("name").getAsString == MyName)
    myPlayerOpt match {
      case Some(myPlayer) =>
        myPlayer.get("bet").getAsInt
      case None =>
        0
    }
  }


  def getCommunityCards(request: JsonElement): List[Card] = {
    request.getAsJsonObject.get("community_cards").getAsJsonArray.toList.map(j => parseCard(j.getAsJsonObject))
  }

  def getMinBuyIn(request: JsonElement): Int = {
    request.getAsJsonObject.get("current_buy_in").getAsInt
  }

  def getBetIndex(request: JsonElement): Int = {
    request.getAsJsonObject.get("bet_index").getAsInt
  }
  def getSmallBlind(request: JsonElement): Int = {
    request.getAsJsonObject.get("small_blind").getAsInt
  }

  val random = new Random()


  def detectCobminations(params: BetParams): List[TargetCombination] = {
    List.empty
  }


  def maxGroupOf(cards: List[Card]): Int = {
    if (cards.isEmpty) 0
    else
      cards
        .groupBy(_.rank)
        .values
        .map(_.size)
        .max
  }

  def colorGroupOf(cards: List[Card]): Int = {
    if (cards.isEmpty) 0
    else
      cards
        .groupBy(_.suit)
        .values
        .map(_.size)
        .max
  }


  def isStrit(myCards: List[Card], community: List[Card]): Boolean = {
    (myCards ++ community).
      sortBy(_.rank).
      map(_.getRankInt).
      distinct.
      sliding(5).
      map(l => l.last - l.head).
      contains(4)
  }

  def isFull(myCards: List[Card], community: List[Card]): Boolean = {
    val allCards = myCards ++ community
    val cardsGrouped = allCards.groupBy(_.rank)
    cardsGrouped.values.exists(_.size == 3) && cardsGrouped.values.exists(_.size == 2)
  }

  def decideBet(params: BetParams): Int = {

    val allCards = params.myCards ++ params.communityCards

    val maxGroupAll = maxGroupOf(allCards)
    val maxGroupMine = maxGroupOf(params.myCards)
    val maxGroupTable = maxGroupOf(params.communityCards)

    val maxColorsAll = colorGroupOf(allCards)
    val maxColorsTable = colorGroupOf(params.communityCards)

    if (isFull(params.myCards, params.communityCards) && !isFull(Nil, params.communityCards)) {
      params.stack
    } else if (isStrit(params.myCards, params.communityCards)) {
      params.buyIn + (if(params.bet <= params.smallBlind * 2) 50 else 0)
    } else if (maxGroupAll >= 2 && (maxGroupMine >= 2 || maxGroupAll != maxGroupTable)) {
      params.buyIn + (if(params.bet <= params.smallBlind * 2) maxGroupAll * 30 else 0)
    } else if (maxColorsAll == 5) {
      params.stack
    } else {
      if (random.nextDouble() < getStartHandPower(params.myCards))
        params.buyIn
      else
        0
    }
  }


  def betRequest(request: JsonElement) = Try {
    val myCards = getMyCards(request)
    val communityCards = getCommunityCards(request)
    val betIndex = getBetIndex(request)
    val minRaise = getMinBuyIn(request)
    val myStack = getMyStack(request)
    val myBet = getMyBet(request)
    val smallBlind = getSmallBlind(request)
    val betParams = BetParams(myCards, communityCards, betIndex, minRaise, myStack, myBet, smallBlind)
    decideBet(betParams)
  }.getOrElse(0)

  def showdown(game: JsonElement) {

  }


  case class RankHand(siuted: Boolean, card1: Int, card2: Int)

  val statistics = Map[RankHand, Double](
    RankHand(false, 13, 13) -> 31.0,
    RankHand(false, 12, 12) -> 26.0,
    RankHand(false, 11, 11) -> 22.0,
    RankHand(true, 13, 12) -> 20.2,
    RankHand(false, 10, 10) -> 19.1,
    RankHand(true, 13, 11) -> 18.7,
    RankHand(true, 12, 11) -> 18.1,
    RankHand(true, 13, 10) -> 17.5,
    RankHand(true, 12, 10) -> 17.1,
    RankHand(false, 9, 9) -> 16.8,
    RankHand(false, 13, 12) -> 16.7,
    RankHand(true, 13, 9) -> 16.6,
    RankHand(true, 11, 10) -> 16.6,
    RankHand(true, 12, 9) -> 16.1,
    RankHand(true, 11, 9) -> 15.8,
    RankHand(true, 10, 9) -> 15.8,
    RankHand(false, 8, 8) -> 15.3,
    RankHand(false, 13, 11) -> 14.9,
    RankHand(true, 13, 8) -> 14.6,
    RankHand(false, 12, 11) -> 14.4,
    RankHand(false, 7, 7) -> 14.2,
    RankHand(true, 12, 8) -> 14.2,
    RankHand(true, 13, 8) -> 14.1,
    RankHand(true, 13, 7) -> 13.9,
    RankHand(true, 11, 8) -> 13.8,
    RankHand(true, 10, 8) -> 13.8,
    RankHand(false, 13, 10) -> 13.5,
    RankHand(true, 13, 4) -> 13.4,
    RankHand(false, 6, 6) -> 13.4,
    RankHand(true, 13, 6) -> 13.4,
    RankHand(false, 12, 10) -> 13.2,
    RankHand(true, 13, 3) -> 13.2,
    RankHand(true, 13, 2) -> 13.1,
    RankHand(true, 13, 5) -> 13.0
  )

  def getStartHandPower(myCards: List[Card]): Double = {
    statistics.getOrElse(getRankHand(myCards), 0.6) * 3 / 100.0
  }

  def getRankHand(myCards: List[Card]): RankHand = {
    val first = myCards.head
    val second = myCards.last
    val areSuited = first.suit == second.suit

    if (first.getRankInt > second.getRankInt) {
      RankHand(areSuited, first.getRankInt, second.getRankInt)
    } else {
      RankHand(areSuited, second.getRankInt, first.getRankInt)
    }
  }

}
