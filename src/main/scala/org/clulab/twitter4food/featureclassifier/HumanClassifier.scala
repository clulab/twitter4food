package org.clulab.twitter4food.featureclassifier

import org.clulab.struct.Counter
import org.clulab.twitter4food.util._
import org.clulab.twitter4food.struct.TwitterAccount

/** Classifier to predict if a given twitter account represents an organization
  * or an individual. Implements a customFeatures method to parse the account
  * description and count #(words) that fall in person/organization Synset
  * @author adikou 
  * @date 01-22-16.
  */

class HumanClassifier(
    useUnigrams: Boolean = true,
    useBigrams: Boolean = false,
    useTopics: Boolean = false,
    useDictionaries: Boolean = false,
    useEmbeddings: Boolean = false,
    useCosineSim: Boolean = false,
    useFollowers: Boolean = false,
    useFollowees: Boolean = false,
    useGender: Boolean = false,
    useRace: Boolean = false,
    datumScaling: Boolean = false,
    featureScaling: Boolean = false)
  extends ClassifierImpl(
    useUnigrams=useUnigrams,
    useBigrams=useBigrams,
    useTopics=useTopics,
    useDictionaries=useDictionaries,
    useEmbeddings=useEmbeddings,
    useCosineSim=useCosineSim,
    useFollowers=useFollowers,
    useFollowees=useFollowees,
    useGender=useGender,
    useRace=useRace,
    datumScaling=datumScaling,
    featureScaling=featureScaling,
    variable = "human",
    customFeatures = HumanClassifier.customFeatures
  )

object HumanClassifier {
  /** Add a custom feature counter for the account based on description
    * @param account Twitter account
    * @return counter custom Counter[String] that keeps a count of "wn_human"
    *         and "wn_org" based on #(words) in person/organization Synset
    */
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
    def isPersonClass(set: Set[String]) = intersection(set, PERSON_CLASS).nonEmpty
    def isOrgClass(set: Set[String]) = intersection(set, ORG_CLASS).nonEmpty

    /** Recurse each synset until synset becomes too generic, or one or more
      * synset reaches person/org synset.
      * @param word Description word
      * @return label "human", "org" or ""
      */
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

    val counter = new Counter[String]()

    // Description features
    val dTags = Tokenizer.annotate(account.description)
      .filter(tt => "NO".contains(tt.tag))

    dTags.foreach{
      case singular if singular.tag == "O" && isSingularPronoun(singular.token) =>
        counter.incrementCount("__hcDescriptionSingular__")
      case plural if plural.tag == "O" && isPluralPronoun(plural.token) =>
        counter.incrementCount("__hcDescriptionPlural__")
      case humanWord if humanWord.tag == "N" && getSubFeatureType(humanWord.token) == "human" =>
        counter.incrementCount("__hcDescriptionHuman__")
      case orgWord if orgWord.tag == "N" && getSubFeatureType(orgWord.token) == "org" =>
        counter.incrementCount("__hcDescriptionOrg__")
      case _ => ()
    }

    // Tweet features
    val tTags = account.tweets.flatMap(tweet => Tokenizer.annotate(tweet.text))
      .filter(tt => "NO".contains(tt.tag))

    tTags.foreach{
      case singular if singular.tag == "O" && isSingularPronoun(singular.token) =>
        counter.incrementCount("__hcTweetSingular__")
      case plural if plural.tag == "O" && isPluralPronoun(plural.token) =>
        counter.incrementCount("__hcTweetPlural__")
      case humanWord if humanWord.tag == "N" && getSubFeatureType(humanWord.token) == "human" =>
        counter.incrementCount("__hcTweetHuman__")
      case orgWord if orgWord.tag == "N" && getSubFeatureType(orgWord.token) == "org" =>
        counter.incrementCount("__hcTweetOrg__")
      case _ => ()
    }

    println(counter.toString)
    counter
  }

  def main(args: Array[String]) = {
    val params = TestUtils.parseArgs(args)
    val (api, config) = TestUtils.init(0)
    val hc = new HumanClassifier(
      useUnigrams = params.useUnigrams,
      useBigrams = params.useBigrams,
      useTopics = params.useTopics,
      useDictionaries = params.useDictionaries,
      useEmbeddings = params.useEmbeddings,
      useCosineSim = params.useCosineSim,
      useFollowers = params.useFollowers,
      useFollowees = params.useFollowees,
      useGender = params.useGender,
      useRace = params.useRace,
      datumScaling = params.datumScaling,
      featureScaling = params.featureScaling)
    hc.runTest(args, "human")
    // hc.learn(args, "human", 0.001, 50)
    // val predictedLabels = hc.predict(config.getString("classifiers.overweight.allTestData"))
  }
}