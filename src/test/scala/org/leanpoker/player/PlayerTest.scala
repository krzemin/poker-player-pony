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

  it("parse bet index") {
    Player.getBetIndex(gameStateJson) mustBe 0
  }

  it("parse Stack") {
    Player.getMyStack(gameStateJson) mustBe 1590
  }


  val pair = List(Card('4', ""), Card('4', ""))

  it("should bet minimum raise + 40 when have pair") {
    Player.decideBet(BetParams(pair, Nil, 0, 100, 0)) mustBe 140
  }

  it("should bet minimum raise + 40 when have pair with community") {
    Player.decideBet(BetParams(List(Card('A', "")), List(Card('A', "")), 0, 200, 0)) mustBe 240
  }


}