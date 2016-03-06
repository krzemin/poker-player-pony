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
                     minimumRaise: Int,
                     stack: Int)



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


  def getCommunityCards(request: JsonElement): List[Card] = {
    request.getAsJsonObject.get("community_cards").getAsJsonArray.toList.map(j => parseCard(j.getAsJsonObject))
  }

  def getMinimumRaise(request: JsonElement): Int = {
    request.getAsJsonObject.get("minimum_raise").getAsInt
  }

  def getBetIndex(request: JsonElement): Int = {
    request.getAsJsonObject.get("bet_index").getAsInt
  }

  val random = new Random()


  def detectCobminations(params: BetParams): List[TargetCombination] = {
    List.empty
  }


  def maxGroupOf(cards: List[Card]): Int = {
    if(cards.isEmpty) 0
    else
      cards
        .groupBy(_.rank)
        .values
        .map(_.size)
        .max
  }

  def colorGroupOf(cards: List[Card]): Int = {
    if(cards.isEmpty) 0
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
      sliding(5).
      map(l => l.last - l.head).
      contains(4)
  }

  def decideBet(params: BetParams): Int = {

    val allCards = params.myCards ++ params.communityCards

    val maxGroupAll = maxGroupOf(allCards)
    val maxGroupTable = maxGroupOf(params.communityCards)

    val maxColorsAll = colorGroupOf(allCards)
    val maxColorsTable = colorGroupOf(params.communityCards)

    if(isStrit(params.myCards, params.communityCards)) {
      params.minimumRaise + 50
    } else if(maxGroupAll >= 2 && maxGroupAll != maxGroupTable) {
      params.minimumRaise + maxGroupAll * 20
    } else if(maxColorsAll == 5) {
      params.stack
    } else {
      val hasStrongCards = params.myCards.map(_.getRankInt).sum > 18

      if((hasStrongCards && random.nextDouble() < 0.65) || random.nextDouble() < 0.1)
        params.minimumRaise
      else
        0
    }
  }


  def betRequest(request: JsonElement) = Try {
    val myCards = getMyCards(request)
    val communityCards = getCommunityCards(request)
    val betIndex = getBetIndex(request)
    val minRaise = getMinimumRaise(request)
    val myStack = getMyStack(request)
    val betParams = BetParams(myCards, communityCards, betIndex, minRaise, myStack)
    decideBet(betParams)
  }.getOrElse(random.nextInt(100))

  def showdown(game: JsonElement) {

  }
}
