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

  it("parse community cards") {
    Player.getCommunityCards(gameStateJson) must have size 3
  }

  it("parse minimum raise") {
    Player.getMinimumRaise(gameStateJson) mustBe 240
  }


  it("should bet minimum raise when have pair") {
    Player.decideBet(List(Card('4', ""), Card('4', "")), 100) mustBe 100
  }

}