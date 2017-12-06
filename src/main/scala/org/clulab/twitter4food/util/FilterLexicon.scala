package org.clulab.twitter4food.util

import org.clulab.struct.Lexicon
import com.typesafe.config.ConfigFactory

object FilterLexicon extends App {

  val config = ConfigFactory.load
  val lastDictLoc = config.getString(s"classifiers.features.lexicons.food_words")
  val newDictLoc = config.getString(s"classifiers.features.lexicons.food_words_less")
  val dictionary = Lexicon.loadFrom[String](lastDictLoc)
  var edited = Seq(dictionary)

  val removables = Seq("add", "can", "pin", "combine", "mix", "sink", "cottage", "dash", "dot", "dog", "dent", "dry",
    "edge", "empty", "fast", "fire", "fold", "gray", "handful", "hang", "high", "junk", "layer", "leaf", "lid", "main",
    "pulses", "reduce", "remove", "rest", "return", "rinse", "side", "skin", "soak", "stuff", "system", "tie", "trim",
    "wash", "pour", "toaster", "diet", "coat", "chew", "tender", "tart", "cheesecake", "heat", "ate", "brown", "dress",
    "blood", "toss", "fries", "drink", "stock")

  removables.zipWithIndex.foreach { case (removable, j) =>
    val remove = edited(j).get(removable)
    val ixMap = if (remove.nonEmpty) {
      (0 until remove.get).map(i => (i, i)) ++ ((remove.get + 1) until dictionary.size).map(i => (i, i - 1))
    } else edited(j).indices.map(i => (i, i))
    edited = edited :+ edited(j).mapIndicesTo(ixMap.toMap)
  }

  edited.last.saveTo(newDictLoc)
}