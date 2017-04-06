package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}

import org.slf4j.LoggerFactory
import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.featureclassifier.ClassifierImpl
import org.clulab.twitter4food.util.{Eval, FileUtils, Utils}

import scala.util.Random
import org.clulab.struct.Counter
import org.clulab.twitter4food.struct.TwitterAccount

/**
  * A classifier for classifying a TwitterAccount as "Overweight" or "Not overweight".
  *
  * @author terron
  * @author Dane Bell
  */
class InformedBaselineOverweightClassifier
  extends ClassifierImpl(
    useUnigrams=false,
    useBigrams=false,
    useName=false,
    useTopics=false,
    useDictionaries=false,
    useAvgEmbeddings=false,
    useMinEmbeddings=false,
    useMaxEmbeddings=false,
    useCosineSim=false,
    useTimeDate=false,
    useFoodPerc=false,
    useCaptions=false,
    useFollowers=false,
    useFollowees=false,
    useRT=false,
    useGender=false,
    useAge=false,
    useRace=false,
    useHuman=false,
    dictOnly=false,
    denoise=false,
    datumScaling=false,
    featureScaling=false,
    variable = "overweight") {
  val labels = Set("Overweight", "Not overweight")
}

object InformedBaselineOverweightClassifier {
  import ClassifierImpl._

  val logger = LoggerFactory.getLogger(this.getClass)

  def computeInformedBaselineLabel(ac: TwitterAccount, OWindicatingWords: Seq[String], NOindicatingWords: Seq[String]) : String = {

    val OWcounter = new Counter[String]

    OWcounter.setCount("Overweight", 0.0)
    OWcounter.setCount("Not overweight", 0.0)
    OWcounter.setCount("token", 0.0)

    for(tweet <- ac.tweets) {
      val tokens = tweet.text.split(" +")
      for(tok <- tokens){
        OWcounter.incrementCount("token", 1)
        if(OWindicatingWords.contains(tok))
          OWcounter.incrementCount("Overweight", 1)
        else if(NOindicatingWords.contains(tok))
          OWcounter.incrementCount("Not overweight", 1)
      }
    }

    val label = if(OWcounter.getCount("Overweight") > OWcounter.getCount("Not overweight"))
      "Overweight"
    else
      "Not overweight"

    label
  }

  def main(args: Array[String]) {
    // Parse args using standard Config
    val params = Utils.parseArgs(args)
    val config = ConfigFactory.load

    val fileExt = "heuristic"

    val outputDir = config.getString("classifier") + "/overweight/results/" + fileExt
    if (!Files.exists(Paths.get(outputDir))) {
      if (new File(outputDir).mkdir()) logger.info(s"Created output directory $outputDir")
      else logger.info(s"ERROR: failed to create output directory $outputDir")
    }

    val modelFile = s"${config.getString("overweight")}/model/$fileExt.dat"
    // Instantiate classifier after prompts in case followers are being used (file takes a long time to load)

    logger.info("Loading Twitter accounts")
    val labeledAccts = FileUtils.load(config.getString("classifiers.overweight.data"))
      .toSeq
      .filter(_._1.tweets.nonEmpty)

    logger.info("Running the informed baseline classifier...")

    val OWindicatingWords = Array[String]("fuck", "retweet", "chance", "shit", "ass", "fat", "hate", "damn", "rt", "sad", "wow")
    val NOindicatingWords = Array[String]("#cook", "#healthy", "#food", "#recipe", "#health", "#fitness", "#breakfast", "#vegan", "workout", "#dinner", "#lunch", "#love", "salad")

    val predictions = for((ac,lbl) <- labeledAccts) yield {
      val pred = computeInformedBaselineLabel(ac, OWindicatingWords, NOindicatingWords)
      (lbl, pred)
    }

    // Print results
    val (evalMeasures, microAvg, macroAvg) = Eval.evaluate(predictions)

    val evalMetric = if (evalMeasures.keySet contains "Overweight") {
      evalMeasures("Overweight")
    } else {
      logger.debug(s"Labels are {${evalMeasures.keys.mkString(", ")}}. Evaluating on ${evalMeasures.head._1}")
      evalMeasures.head._2
    }
    val precision = evalMetric.P
    val recall = evalMetric.R

    // Save results
    val writer = new BufferedWriter(new FileWriter(outputDir + "/analysisMetrics.txt", false))
    writer.write(s"Precision: $precision\n")
    writer.write(s"Recall: $recall\n")
    writer.write(s"F-measure (harmonic mean): ${fMeasure(precision, recall, 1)}\n")
    writer.write(s"F-measure (recall 5x): ${fMeasure(precision, recall, .2)}\n")
    writer.write(s"Macro average: $macroAvg\n")
    writer.write(s"Micro average: $microAvg\n")
    writer.close()

    // Save individual predictions for bootstrap significance
    val predWriter = new BufferedWriter(new FileWriter(outputDir + "/predicted.txt", false))
    predWriter.write(s"gold\tpred\n")
    predictions.foreach(acct => predWriter.write(s"${acct._1}\t${acct._2}\n"))
    predWriter.close()

    val evals = Seq((predictions.length, precision, recall, macroAvg, microAvg))

    println(s"\n$fileExt\n%train\t#accts\tp\tr\tf1\tf1(r*5)\tmacro\tmicro")
    evals.foreach { case (numAccounts, precision, recall, macroAvg, microAvg) =>
      println(s"$numAccounts\t$precision\t$recall\t${fMeasure(precision, recall, 1)}\t${fMeasure(precision, recall, .2)}" +
        s"\t$macroAvg\t$microAvg")
    }
  }
}