package org.clulab.twitter4food.featureclassifier

import edu.arizona.sista.learning._
import edu.arizona.sista.struct.Counter
import org.clulab.twitter4food.util._
import org.clulab.twitter4food.struct.TwitterAccount

/**
  * Created by adikou on 1/22/16.
  */

class HumanClassifier(
  useUnigrams: Boolean = true, useBigrams: Boolean = false,
  useTopics: Boolean = false,  useDictionaries: Boolean = false,
  useEmbeddings: Boolean = false) extends ClassifierImpl(useUnigrams,
    useBigrams, useTopics, useDictionaries, useEmbeddings) {

  override def addDatum(account: TwitterAccount, label: String) = {
    dataset += featureExtractor.mkDatum(account, label, customFeatures(account))
  }

  def customFeatures(account: TwitterAccount): Counter[String] = {
    val SINGULAR_PRONOUNS = Set("I", "me", "you", "she", "her", "he",
                              "him", "it", "myself", "yourself", "itself",
                              "himself", "herself", "self", "oneself")
    val PLURAL_PRONOUNS = Set("we", "us", "they", "them", "ourselves",
                            "yourselves", "themselves")

    def isSingularPronoun(word: String) = SINGULAR_PRONOUNS.contains(word)
    def isPluralPronoun(word: String) = PLURAL_PRONOUNS.contains(word)

    val PERSON_CLASS = Set("person", "individual", "mortal", "self", "someone",
                           "somebody", "soul")
    val ORG_CLASS = Set("organisation", "organization", "establishment",
                      "governance", "governing body", "administration",
                      "arrangement", "constitution", "formation",
                      "institution", "building", "edifice", "structure")

    def intersection(A: Set[String], B: Set[String]) = A.intersect(B)
    def isPersonClass(set: Set[String]) = !intersection(set, PERSON_CLASS).isEmpty
    def isOrgClass(set: Set[String]) = !intersection(set, ORG_CLASS).isEmpty

    def getSubFeatureType(word: String): String = {
      val hyp = new HypernymSet()
      var level = 0
      var hSet = Set(word)
      var hSubset = hyp.subHypernym(hSet)

      while(level < 3 && !(isPersonClass(hSubset) || isOrgClass(hSubset))) {
        hSet = hSubset
        hSubset = hyp.subHypernym(hSet)
        level += 1
      }

      val A = isPersonClass(hSubset)
      val B = isOrgClass(hSubset)

      if(A || B) {
        if(A && B) {
          val s1 = intersection(hSubset, PERSON_CLASS)
          val s2 = intersection(hSubset, ORG_CLASS)

          if(s1.size > s2.size) "humans"
          else if(s2.size > s1.size) "org"
          else if(scala.util.Random.nextInt(2) == 0) "human" else "org"
        }
        else if(A) "human"
        else "org"
      }
      else ""
    }

    val nTags = Tokenizer.annotate(account.description)
      .filter(tt => "NO".contains(tt.tag))
    val (hCounts, oCounts) = nTags.foldLeft((0,0))(
      (count, tagTok) => {
        if(tagTok.tag.equals("O")) {
          if(isSingularPronoun(tagTok.token)) (count._1 + 1, count._2)
          else if(isPluralPronoun(tagTok.token)) (count._1, count._2 + 1)
          else count
        }
        else {
          getSubFeatureType(tagTok.token) match {
            case "human" => (count._1 + 1, count._2)
            case "org" => (count._1, count._2 + 1)
            case _ => count
          }
        }
      })

    val counter = new Counter[String]()
    counter.incrementCount("wn_human", hCounts)
    counter.incrementCount("wn_org", oCounts)

    counter
  }
}

object HumanClassifier {
  def main(args: Array[String]) = {
    val params = TestUtils.parseArgs(args)
    val (api, config) = TestUtils.init(0, true)
    val hc = new HumanClassifier(params.useUnigrams, params.useBigrams,
      params.useTopics, params.useDictionaries, params.useEmbeddings)
    hc.runTest(args, "human")
  }
}