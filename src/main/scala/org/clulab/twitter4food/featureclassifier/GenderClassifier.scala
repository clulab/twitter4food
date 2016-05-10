package org.clulab.twitter4food.featureclassifier

import edu.arizona.sista.learning._
import edu.arizona.sista.struct.Counter
import org.clulab.twitter4food.struct._
import org.clulab.twitter4food.util._
import java.io._

/** Gender classifier that predicts if a given twitter account is M (male)
  * or F (female)
  * @author adikou
  * @date 03-13-2016
  */
class GenderClassifier(
  useUnigrams: Boolean = true, useBigrams: Boolean = false,
  useTopics: Boolean = false,  useDictionaries: Boolean = false,
  useEmbeddings: Boolean = false, useFollowers: Boolean = false) 
  extends ClassifierImpl(useUnigrams, useBigrams, useTopics, 
    useDictionaries, useEmbeddings, useFollowers)

object GenderClassifier {
  def main(args: Array[String]): Unit = {
    val params = TestUtils.parseArgs(args)
    val (api, config) = TestUtils.init(0)
    val gc = new GenderClassifier(params.useUnigrams, params.useBigrams,
      params.useTopics, params.useDictionaries, params.useEmbeddings)
    gc.runTest(args, "gender")
  }
}