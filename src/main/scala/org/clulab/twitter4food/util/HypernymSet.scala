package org.clulab.twitter4food.util

import edu.smu.tspell.wordnet._

class HypernymSet {
  System.setProperty("wordnet.database.dir", "lib/WordNet-3.0/dict/")
  val db = WordNetDatabase.getFileInstance
  
  def getHypernyms(word: String): Set[String] = {
    val synsets = db.getSynsets(word, SynsetType.NOUN)
    val hypernyms = synsets.foldLeft(Set[String]())(
      (l, s) => {
        val nouns = s.asInstanceOf[NounSynset]
        l ++ nouns.getHypernyms.reverse.foldLeft(Set[String]())(
          (L, h) => L ++ h.getWordForms)
      })
    hypernyms
  }

  def subHypernym(set: Set[String]) = {
    set.foldLeft(Set[String]())((l, b) => l ++ getHypernyms(b))
  }
}