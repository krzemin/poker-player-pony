package org.leanpoker.player

/**
  * Created by robert on 06.03.16.
  */
object Util {

	case class RankHand(siuted: Boolean, card1: Int, card2: Int)

	val statistics = Map[RankHand, Double](
		RankHand(false, 13, 13) -> 31.0,
		RankHand(false, 12, 12) -> 26.0,
		RankHand(false, 11, 11) -> 22.0,
		RankHand(true, 13, 12) -> 20.2,
		RankHand(false, 10, 10) -> 19.1,
		RankHand(true, 13, 11) -> 18.7,
		RankHand(true, 12, 11) -> 18.1,
		RankHand(true, 13, 10) -> 17.5,
		RankHand(true, 12, 10) -> 17.1,
		RankHand(false, 9, 9) -> 16.8,
		RankHand(false, 13, 12) -> 16.7,
		RankHand(true, 13, 9) -> 16.6,
		RankHand(true, 11, 10) -> 16.6,
		RankHand(true, 12, 9) -> 16.1,
		RankHand(true, 11, 9) -> 15.8,
		RankHand(true, 10, 9) -> 15.8,
		RankHand(false, 8, 8) -> 15.3,
		RankHand(false, 13, 11) -> 14.9,
		RankHand(true, 13, 8) -> 14.6,
		RankHand(false, 12, 11) -> 14.4,
		RankHand(false, 7, 7) -> 14.2,
		RankHand(true, 12, 8) -> 14.2,
		RankHand(true, 13, 8) -> 14.1,
		RankHand(true, 13, 7) -> 13.9,
		RankHand(true, 11, 8) -> 13.8,
		RankHand(true, 10, 8) -> 13.8,
		RankHand(false, 13, 10) -> 13.5,
		RankHand(true, 13, 4) -> 13.4,
		RankHand(false, 6, 6) -> 13.4,
		RankHand(true, 13, 6) -> 13.4,
		RankHand(false, 12, 10) -> 13.2,
		RankHand(true, 13, 3) -> 13.2,
		RankHand(true, 13, 2) -> 13.1,
		RankHand(true, 13, 5) -> 13.0
	)

	def getStartHandPower(myCards: List[Card]): Double = {
		statistics.get(getRankHand(myCards)).getOrElse(5.0)
	}

	def getRankHand(myCards: List[Card]): RankHand ={
		val first = myCards.head
		val second = myCards.last
		val areSuited = first.suits.equals(second.suits)
		if(first.getRankInt > second.getRankInt){
			RankHand(areSuited, first.getRankInt, second.getRankInt)
		} else {
			RankHand(areSuited, second.getRankInt, first.getRankInt)
		}
	}

	def isStrit(myCards: List[Card], community: List[Card]): Boolean = {
		(myCards ++ community).
			sortBy(_.rank).
			map(_.getRankInt).
			sliding(5).
			map(l => l.last - l.head).
			exists(_ == 4)
	}



}
