package org.leanpoker.player

import java.io.InputStreamReader

import com.google.gson.JsonParser
import com.google.gson.stream.JsonReader
import org.scalatest.{FunSpec, MustMatchers}

class PlayerTest extends FunSpec with MustMatchers {

  val gameStateJsonStream = getClass.getResourceAsStream("/game_state.json")
  val gameStateJson = new JsonParser().parse(new InputStreamReader(gameStateJsonStream))


  it("parse cards") {
    Player.getMyCards(gameStateJson) must have size 2


  }

}