package org.leanpoker.player

import com.google.gson.JsonElement
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

case class Card(rank: Char, suits: String)




object Player {

  val VERSION = "Default Scala folding player"

  val MyName = "Pony"


  def getMyCards(request: JsonElement): List[Card] = {
    val players = request.getAsJsonObject.get("players").getAsJsonArray
    val myPlayer = players.toList.find(_.getAsJsonObject.get("name") == MyName).get
    val myCards = myPlayer.getAsJsonObject.get("hole_cards").getAsJsonArray.map(_.getAsJsonObject).toList
    myCards.map { cardJson =>
      Card(cardJson.get("rank").getAsCharacter, cardJson.get("suits").getAsString)
    }
  }


  def betRequest(request: JsonElement) = {

    val myCards = getMyCards(request)

    if(myCards.groupBy(_.rank).values.exists(_.size == 2)) {
      100
    } else {
      0
    }
  }

  def showdown(game: JsonElement) {

  }
}
