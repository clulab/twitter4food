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
  useAvgEmbeddings: Boolean = false,
  useMinEmbeddings: Boolean = false,
  useMaxEmbeddings: Boolean = false,
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
    useAvgEmbeddings=useAvgEmbeddings,
    useMinEmbeddings=useMinEmbeddings,
    useMaxEmbeddings=useMaxEmbeddings,
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
    val params = Utils.parseArgs(args)
    Utils.init(0)
    val gc = new GenderClassifier(
      useUnigrams = params.useUnigrams,
      useBigrams = params.useBigrams,
      useTopics = params.useTopics,
      useDictionaries = params.useDictionaries,
      useAvgEmbeddings = params.useAvgEmbeddings,
      useMinEmbeddings = params.useMinEmbeddings,
      useMaxEmbeddings = params.useMaxEmbeddings,
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