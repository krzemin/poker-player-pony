package org.leanpoker.player

import org.scalatest.{MustMatchers, FunSpec}

/**
  * Created by robert on 06.03.16.
  */
class UtilTest extends FunSpec with MustMatchers {

	it("should find strit") {
		val hand = List(Card('2', "t"), Card('5', "t"))
		val community = List(Card('4', "t"), Card('7', "t"), Card('6', "t"), Card('3', "t"))

		Util.isStrit(hand, community) mustBe true
	}

	it("should not fount false strit") {
		val hand = List(Card('2', "t"), Card('5', "t"))
		val community = List(Card('9', "t"), Card('7', "t"), Card('6', "t"), Card('3', "t"))

		Util.isStrit(hand, community) mustBe false
	}
}
