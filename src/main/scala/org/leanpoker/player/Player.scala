package org.leanpoker.player

import com.google.gson.JsonElement
import scala.collection.JavaConversions._
import scala.util.{Random, Try}

case class Card(rank: Char, suits: String)  {
  def getRankInt: Int = {
    val rankToNumMap = Map{
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
    }
    rankToNumMap(rank)
  }
}





object Player {

  val VERSION = "Fixxxx"

  val MyName = "Pony"


  def getMyCards(request: JsonElement): List[Card] = {
    val players = request.getAsJsonObject.get("players").getAsJsonArray.map(_.getAsJsonObject)
    val myPlayerOpt = players.find(_.get("name").getAsString == MyName)
    println(myPlayerOpt)
    myPlayerOpt match {
      case Some(myPlayer) =>
        val myCards = myPlayer.get("hole_cards").getAsJsonArray.map(_.getAsJsonObject).toList
        myCards.map { cardJson =>
          println(cardJson)
          Card(
            cardJson.get("rank").getAsString.head,
            cardJson.get("suit").getAsString
          )
        }
      case None =>
        List.empty
    }
  }

  val random = new Random()

  def betRequest(request: JsonElement) = Try {

    val myCards = getMyCards(request)

    if(myCards.groupBy(_.rank).values.exists(_.size == 2)) {
      100
    } else {
      0
    }
  }.getOrElse(random.nextInt(100))

  def showdown(game: JsonElement) {

  }
}
