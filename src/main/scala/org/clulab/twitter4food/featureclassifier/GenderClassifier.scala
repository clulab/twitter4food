package org.clulab.twitter4food.featureclassifier

import org.clulab.learning._
import org.clulab.struct.Counter
import org.clulab.twitter4food.struct._
import org.clulab.twitter4food.util._
import java.io._

/** Gender classifier that predicts if a given twitter account is M (male)
  * or F (female)
  * @author adikou
  * @date 03-13-2016
  */
class GenderClassifier(
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
    variable = "gender"
  )

object GenderClassifier {
  def main(args: Array[String]): Unit = {
    val params = TestUtils.parseArgs(args)
    TestUtils.init(0)
    val gc = new GenderClassifier(
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
    gc.runTest(args, "gender")
  }
}