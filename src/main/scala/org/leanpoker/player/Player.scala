package org.leanpoker.player

import com.google.gson.JsonElement

object Player {

  val VERSION = "Default Scala folding player"

  def betRequest(request: JsonElement) = {

    //request.getAsJsonObject.get("pot").getAsInt()
    120
  }

  def showdown(game: JsonElement) {

  }
}
