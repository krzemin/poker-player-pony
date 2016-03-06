package org.leanpoker.player

/**
  * Created by robert on 06.03.16.
  */
object Util {

	def isStrit(myCards: List[Card], community: List[Card]): Boolean = {
		(myCards ++ community).
			sortBy(_.rank).
			map(_.getRankInt).
			sliding(5).
			map(l => l.last - l.head).
			exists(_ == 4)

	}

}
