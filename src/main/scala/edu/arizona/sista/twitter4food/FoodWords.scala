package edu.arizona.sista.twitter4food
import scala.io.Source

/**
 * Created by dfried on 5/21/14.
 */
object FoodWords {
  lazy val words: Set[String] = {
    def source = Source.fromURL(getClass.getResource("food_words.txt"))
    source.getLines.map(_.trim.toLowerCase).toSet
  }
}

