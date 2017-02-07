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
class InformedBaselineOverweightClassifier(
  useUnigrams: Boolean = false,
  useBigrams: Boolean = false,
  useName: Boolean = false,
  useTopics: Boolean = false,
  useDictionaries: Boolean = false,
  useAvgEmbeddings: Boolean = false,
  useMinEmbeddings: Boolean = false,
  useMaxEmbeddings: Boolean = false,
  useCosineSim: Boolean = false,
  useTimeDate: Boolean = false,
  useFollowers: Boolean = false,
  useFollowees: Boolean = false,
  useRT: Boolean = false,
  useGender: Boolean = false,
  useRace: Boolean = false,
  useHuman: Boolean = false,
  datumScaling: Boolean = false,
  featureScaling: Boolean = false)
  extends ClassifierImpl(
    useUnigrams=useUnigrams,
    useBigrams=useBigrams,
    useName=useName,
    useTopics=useTopics,
    useDictionaries=useDictionaries,
    useAvgEmbeddings=useAvgEmbeddings,
    useMinEmbeddings=useMinEmbeddings,
    useMaxEmbeddings=useMaxEmbeddings,
    useCosineSim=useCosineSim,
    useTimeDate=useTimeDate,
    useFollowers=useFollowers,
    useFollowees=useFollowees,
    useRT=useRT,
    useGender=useGender,
    useRace=useRace,
    useHuman=useHuman,
    datumScaling=datumScaling,
    featureScaling=featureScaling,
    variable = "overweight") {
  val labels = Set("Overweight", "Not overweight")
}

object InformedBaselineOverweightClassifier {
  import ClassifierImpl._

  val logger = LoggerFactory.getLogger(this.getClass)

  def computeInformedBaselineLabel(ac: TwitterAccount, OWindicatingWords: Seq[String], NOindicatingWords: Seq[String]) : String = {
    
    val OWcounter = new Counter[String]
    
    OWcounter.setCount("Overweight", 0.0)
    OWcounter.setCount("NotOverweight", 0.0)
    OWcounter.setCount("token", 0.0)
    
    for(tweet <- ac.tweets) {
      val tokens = tweet.text.split(" +")
      for(tok <- tokens){
        OWcounter.incrementCount("token", 1)
        if(OWindicatingWords.contains(tok))
          OWcounter.incrementCount("Overweight", 1)
        else if(NOindicatingWords.contains(tok))
          OWcounter.incrementCount("NotOverweight", 1)
      }
    }
    
    val label = if(OWcounter.getCount("Overweight") > OWcounter.getCount("NotOverweight"))
                    "Overweight"
                else
                    "NotOverweight"    
    
    label
  }
  
  def main(args: Array[String]) {
    // Parse args using standard Config
    val params = Utils.parseArgs(args)
    val config = ConfigFactory.load

    // List of features (not counting domain adaptation)
    // if these are all false, set default to true to use unigrams anyway
    val allFeatures = Seq(
      params.useUnigrams,
      params.useBigrams,
      params.useName,
      params.useTopics,
      params.useDictionaries,
      params.useAvgEmbeddings,
      params.useMinEmbeddings,
      params.useMaxEmbeddings,
      params.useCosineSim,
      params.useTimeDate,
      params.useFollowees
    )
    val default = allFeatures.forall(!_) // true if all features are off

    val portions = if (params.learningCurve) (1 to 20).map(_.toDouble / 20) else Seq(1.0)

    val nonFeatures = Seq("--analysis", "--test", "--learningCurve")
    // This model and results are specified by all input args that represent featuresets
    val fileExt = args.filterNot(nonFeatures.contains).sorted.mkString("").replace("-", "")

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

    // Scale number of accounts so that weights aren't too biased against Overweight
    val desiredProps = Map( "Overweight" -> 0.5, "Not overweight" -> 0.5 )
    val subsampled = Utils.subsample(labeledAccts, desiredProps)

    val followers = if(params.useFollowers) {
      logger.info("Loading follower accounts...")
      Option(ClassifierImpl.loadFollowers(subsampled.map(_._1)))
    } else None

    val followees = if(params.useFollowees) {
      logger.info("Loading followee accounts...")
      Option(ClassifierImpl.loadFollowees(subsampled.map(_._1), "overweight"))
    } else None

    val evals = for {
      portion <- portions
      maxIndex = (portion * subsampled.length).toInt
    } yield {
      val (accts, lbls) = subsampled.slice(0, maxIndex).unzip

      val oc = new OverweightClassifier(
        useUnigrams = default || params.useUnigrams,
        useBigrams = params.useBigrams,
        useName = params.useName,
        useTopics = params.useTopics,
        useDictionaries = params.useDictionaries,
        useAvgEmbeddings = params.useAvgEmbeddings,
        useMinEmbeddings = params.useMinEmbeddings,
        useMaxEmbeddings = params.useMaxEmbeddings,
        useCosineSim = params.useCosineSim,
        useTimeDate = params.useTimeDate,
        useFollowers = params.useFollowers,
        useFollowees = params.useFollowees,
        useRT = params.useRT,
        useGender = params.useGender,
        useRace = params.useRace,
        datumScaling = params.datumScaling,
        featureScaling = params.featureScaling)

      logger.info("Running the informed baseline classifier...")
      
      val OWindicatingWords = Array[String]("abc", "def")
      val NOindicatingWords = Array[String]("xyz", "fgh")
      
      val x = accts.zip(lbls)
      val predictions = for((ac,lbl) <- accts.zip(lbls)) yield {
        val pred = computeInformedBaselineLabel(ac, OWindicatingWords, NOindicatingWords)
        (lbl, pred)
      }
      
      val highConfPercent = config.getDouble("classifiers.overweight.highConfPercent") //params.highConfPercent
//      val (predictions, avgWeights, falsePos, falseNeg) = oc.overweightCV(accts,
//          lbls,
//          followers,
//          followees,
//          Utils.svmFactory,
//          percentTopToConsider=highConfPercent)

      
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

      // Write analysis only on full portion
      if (portion == 1.0) {
//        if (params.fpnAnalysis) {
//          // Perform analysis on false negatives and false positives
//          outputAnalysis(outputDir, avgWeights, falsePos, falseNeg)
//        }

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
      }

      (portion, predictions.length, precision, recall, macroAvg, microAvg)
    }

    println(s"\n$fileExt\n%train\t#accts\tp\tr\tf1\tf1(r*5)\tmacro\tmicro")
    evals.foreach { case (portion, numAccounts, precision, recall, macroAvg, microAvg) =>
      println(s"$portion\t$numAccounts\t$precision\t$recall\t${fMeasure(precision, recall, 1)}\t${fMeasure(precision, recall, .2)}" +
        s"\t$macroAvg\t$microAvg")
    }
  }
}