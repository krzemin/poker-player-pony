package org.leanpoker.player

import java.io.InputStreamReader

import com.google.gson.JsonParser
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

  val pair = List(Card('4', ""), Card('4', ""))

  it("should bet minimum raise when have pair") {
    Player.decideBet(pair, Nil, 100) mustBe 100
  }

  it("should bet minimum raise when have pair with community") {
    Player.decideBet(List(Card('A', "")), List(Card('A', "")), 200) mustBe 200
  }


}