package org.leanpoker.player

import com.google.gson.{JsonObject, JsonElement}
import scala.collection.JavaConversions._
import scala.util.{Random, Try}

case class Card(rank: Char, suits: String)  {
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

case class BetParams(myCards: List[Card],
                     communityCards: List[Card],
                     betIdx: Int,
                     minimumRaise: Int)



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

  def decideBet(params: BetParams): Int = {

    val allCards = params.myCards ++ params.communityCards

    val maxGroup = allCards
      .groupBy(_.rank)
      .values
      .map(_.size)
      .max

    if(maxGroup >= 2) {
      params.minimumRaise + maxGroup * 100
    } else {

      val hasStrongCards = allCards.map(_.getRankInt).size > 18

      if(params.betIdx <= 1 || (hasStrongCards && random.nextDouble() < 0.8) || random.nextDouble() < 0.3)
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
    val betParams = BetParams(myCards, communityCards, betIndex, minRaise)
    decideBet(betParams)
  }.getOrElse(random.nextInt(100))

  def showdown(game: JsonElement) {

  }
}
