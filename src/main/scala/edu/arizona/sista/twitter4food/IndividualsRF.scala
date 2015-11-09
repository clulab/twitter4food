package edu.arizona.sista.twitter4food

import java.io.{PrintWriter, File}
import edu.arizona.sista.learning.RandomForestClassifier
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.twitter4food.Experiment._
import edu.arizona.sista.utils.EvaluationStatistics

import org.apache.commons.io.FilenameUtils

case class IndividualsRFParameters[L,F](corpus: IndividualsRFCorpus,
                                        prfc: Option[RandomForestClassifier[L,F]],
                                        folds: Option[Int] = None)

/**
 * Evaluate individual users' tweets on a random forest,
 * either constructed from the users' tweets or preloaded
 * User: dane
 * Date: 10/27/15
 */
class IndividualsRF[L,F] (val rfParams: IndividualsRFParameters[L,F], val expParams: ExperimentParameters = ExperimentParameters(), val pw: PrintWriter, randomSeed: Int = 727)
  extends Experiment(parameters = expParams, printWriter = pw){

  def run(onlyFoodTweets: Boolean = false) = {
    val allTweets = rfParams.corpus.tweets.map(it => if (onlyFoodTweets) filterFoodTweets(it.tweets) else it.tweets)
    val allLabels = rfParams.corpus.tweets.map(_.label.get)

    val predictedLabels: Seq[(IndividualsTweets, Counter[Int])] = if (rfParams.prfc.nonEmpty) {
      val testingFeatures = mkViewFeatures(expParams.lexicalParameters.ngramThreshold)(allTweets)._1
      rfParams.corpus.tweets zip (for {
        testF <- testingFeatures
        // make a datum for the individual (label will not be used!)
        testingDatum = mkDatum(0, datasetFromFeatures(testingFeatures, allLabels)._2(testF))
        prediction = rfParams.prfc.get.asInstanceOf[RandomForestClassifier[Int,String]].scoresOf(testingDatum)
      } yield prediction)
    }
    else {
      (for{
        (testingTweets, trainingTweets) <- cvSets(rfParams.corpus.tweets, rfParams.folds, randomSeed)
        trainingLabels = trainingTweets.map(_.label.get)
        (trainingFeatures, filterFn) =  mkViewFeatures(parameters.lexicalParameters.ngramThreshold)(trainingTweets.map(_.tweets))
        testingFeatures = mkViewFeatures(None)(testingTweets.map(_.tweets))._1.map(_.filter(p => filterFn(p._1)))
        (clf, procFeats, featureSelector) = trainFromFeatures(trainingFeatures, trainingLabels)

        predictedLabels = testingTweets zip (for {
          testF <- testingFeatures
          // make a datum for the individual (label will not be used!)
          testingDatum = mkDatum(0, procFeats(testF))
          prediction = clf.scoresOf(testingDatum)
          _ = {println(prediction.toShortString)}
        } yield prediction)
      } yield predictedLabels).flatten
    }
    predictedLabels
  }

  // create cross-validation tuples with k folds, with the held-out test being the first tuple element
  def cvSets(tweets: Seq[IndividualsTweets], k: Option[Int], randomSeed: Int) = {
    val r = new scala.util.Random(randomSeed)
    val shuffledTweets = r.shuffle(tweets)
    val pieces = cut(shuffledTweets, k.getOrElse(10)).toSeq
    for (piece <- pieces) yield (piece, (pieces diff piece).flatten)
  }

  // http://stackoverflow.com/questions/11456107/partition-a-collection-into-k-close-to-equal-pieces-scala-but-language-agnos
  def cut[A](xs: Seq[A], n: Int) = {
    val m = xs.length
    val targets = (0 to n).map{x => math.round((x.toDouble*m)/n).toInt}
    def snip(xs: Seq[A], ns: Seq[Int], got: Vector[Seq[A]]): Vector[Seq[A]] = {
      if (ns.length<2) got
      else {
        val (i,j) = (ns.head, ns.tail.head)
        snip(xs.drop(j-i), ns.tail, got :+ xs.take(j-i))
      }
    }
    snip(xs, targets, Vector.empty)
  }
}

object IndividualsRF {
  def main(args: Array[String]) = {
    require(args.length > 1,
      """
        |First argument: Directory containing one folder per label, with each individual's tweets
        |  in its own file. Class labels must be integers!
        |Second argument: File to output results to.
        |Third argument (optional): Random Forest input file. If none is entered, cross-validated RF
        |  will be used.
        |""".stripMargin)
    require(new java.io.File(args(0)).exists, "Input tweet directory not found!")
    require(args.length < 3 || new java.io.File(args(2)).exists, "Input Random Forest not found!")

    val corp = new IndividualsRFCorpus(args(0))
    val pw: PrintWriter = new PrintWriter(new java.io.File(args(1)))
    val prfc = if (args.length > 2) Some(RandomForestClassifier.loadFrom[Int,String](args(2))) else None

    val folds = Some(10)

    val paramsAndPredictions = if (prfc.nonEmpty) {
      // We're loading a RF from a previous training session
      val params = IndividualsRFParameters(corp, prfc)
      // These parameters must match those of the previous RF
      val lexParams = new LexicalParameters(AllTokens, List(LDAAnnotator(AllTokens)), NoNorm, Some(3), Some(3))
      val expParams = new ExperimentParameters(lexicalParameters = lexParams)
      Seq((params, expParams) -> new IndividualsRF(params, expParams, pw).run())
    } else { for {
      // which base tokens to use? e.g. food words, hashtags, all words
      tokenTypes: TokenType <- List(AllTokens, HashtagTokens, FoodTokens, FoodHashtagTokens).par
      // tokenTypes: TokenType <- List(AllTokens).par
      // which annotators to use in addition to tokens?
      annotators <- List(
        // List(LDAAnnotator(tokenTypes), SentimentAnnotator))
        // List(SentimentAnnotator),
        List(LDAAnnotator(tokenTypes)))//,
        //List())
      // type of normalization to perform: normalize across a feature, across a state, or not at all
      // this has been supplanted by our normalization by the number of tweets for each state
      normalization = NoNorm
      // only keep ngrams occurring this many times or more
      //ngramThreshold <- List(None, Some(2), Some(3))
      ngramThreshold = Some(2)
      // split feature values into this number of quantiles
      numFeatureBins = None
      // use a bias in the SVM?
      useBias = false
      // use regions as features?
      regionType = NoRegions

      // Some(k) to use k classifiers bagged, or None to not do bagging
      baggingNClassifiers <- List(None)
      // force use of features that we think will be informative in random forests?
      forceFeatures = false
      // Some(k) to keep k features ranked by mutual information, or None to not do this
      miNumToKeep: Option[Int] = None
      // Some(k) to limit random forest tree depth to k levels, or None to not do this
      maxTreeDepth: Option[Int] <-  List(Some(3), Some(4), Some(5))
      // these were from failed experiments to use NNMF to reduce the feature space
      //reduceLexicalK: Option[Int] = None
      //reduceLdaK: Option[Int] = None

      numClasses = 2
      removeMarginals: Option[Int] = None
      filterFoodTweets = false

      rfParams = new IndividualsRFParameters[Int, String](corp, prfc, folds)
      expParams = new ExperimentParameters(new LexicalParameters(tokenTypes, annotators, normalization, ngramThreshold, numFeatureBins),
        classifierType=RandomForest, // note: this is ignored
        useBias, regionType, baggingNClassifiers, forceFeatures, numClasses,
        miNumToKeep, maxTreeDepth, removeMarginals, featureScalingFactor = Some(1.0))

      } yield (rfParams, expParams) -> new IndividualsRF(rfParams, expParams, pw).run()
    }

    for (((rfParams, expParams), predictions) <- paramsAndPredictions) {
      pw.println(expParams)

      val testingTweets = predictions.map(_._1)
      val testingPredictions = predictions.map(_._2)

      val actual = testingTweets.map(_.label.getOrElse(-1))

      // baseline
      val majorityBaseline: Seq[Int] = predictMajorityNoCV(actual)
      val labelledBaseline = testingTweets zip majorityBaseline

      val (baselineCorrect, baselineTotal) = IndividualsBaseline.labelledAccuracy(labelledBaseline)
      pw.println(s"overall accuracy\t${baselineCorrect} / ${baselineTotal}\t${baselineCorrect.toDouble / baselineTotal * 100.0}%")
      pw.println

      val confidences = testingPredictions.map(scoresToConf)
      val thresholds = (50 to 99).map(t => ((math rint (t * 0.01)) * 100) / 100)

      val thresholdedPredictions = thresholdByConf(testingTweets, confidences, thresholds).filter(_._2.nonEmpty)

      pw.println("key\tthreshold\ttotal\tprecision\trecall\tf1\tacc\tp\n============================================")
      thresholdedPredictions.indices.foreach{ predSet =>
        val (thresh, preds) = thresholdedPredictions(predSet)
        val (actualSubset, pred) = preds.unzip
        val tables = EvaluationStatistics.makeTables(actualSubset.map(_.label.get), pred)
        val baselineSubset: Seq[Int] = predictMajorityNoCV(actualSubset.map(_.label.get))
        println(baselineSubset.mkString(" "))
        val pvalue = EvaluationStatistics.classificationAccuracySignificance(pred, baselineSubset, actualSubset)

        tables.foreach { case (k, table) =>
          pw.println(s"$k\t$thresh\t${pred.length}\t${table.precision}\t${table.recall}\t${table.f1}\t${table.accuracy * 100.0}\t(p = $pvalue)")
        }
      }

/*
      val byClass: Map[Int, Seq[(IndividualsTweets, Int)]] = labelledInstances.groupBy(_._1.label.get)

      val byClassAccuracy = byClass.mapValues(IndividualsBaseline.labelledAccuracy).toMap
      // print the per-class accuracy
      pw.println("accuracy by class")
      for ((class_, (correct, total)) <- byClassAccuracy) {
        pw.println(s"class ${class_}\t${correct} / ${total}\t${correct.toDouble / total * 100.0}%")
      }
      pw.println

      // print each prediction
      for ((tweets, prediction) <- labelledInstances.sortBy( { case (it, prediction) => (it.label.get, it.username) } )) {
        pw.println(s"${tweets.username}\tact: ${tweets.label.get}\tpred: ${prediction}")
      }
*/

      pw.println
      pw.println
    }
  }

  def scoresToConf(scores: Counter[Int]): (Int, Double) = {
    var max = Double.MinValue
    var sum = 0.0
    var bestIndex = -1
    for(i <- 0 until scores.size) {
      val thisScore = scores.getCount(i)
      sum += thisScore
      if(thisScore > max) {
        max = thisScore
        bestIndex = i
      }
    }
    assert(bestIndex >= 0)
    (bestIndex, max / sum)
  }

  def thresholdByConf(tweets: Seq[IndividualsTweets],
                      confs: Seq[(Int, Double)],
                      cutoffs: Seq[Double] = 50 to 99): Seq[(Double, Seq[(IndividualsTweets, Int)])] = {
    for {
      co <- cutoffs
      thresholded = for {
        tweeter <- tweets.indices
        if confs(tweeter)._2 >= co
      } yield (co,(tweets(tweeter), confs(tweeter)._1))
    } yield thresholded
  }
}

class IndividualsRFCorpus(val baseDirectory: String) {
  val labelDirs: Array[File] = { new File(baseDirectory).listFiles }

  val dirsByLabel = (for {
    labelDir <- labelDirs
    if labelDir.isDirectory
  } yield labelDir.getName.toInt -> labelDir).toMap

  val tweetFilesByLabel: Map[Int, Array[File]] = dirsByLabel.mapValues(_.listFiles).map(identity)

  val tweets = getIndividualsTweets

  private def getIndividualsTweets: Seq[IndividualsTweets] = {
    val tweetParser = new MinimalTweetParser
    (for {
      (lbl, files) <- tweetFilesByLabel
      file <- files
      username = usernameForFile(file)
      tweets = tweetParser.parseTweetFile(io.Source.fromFile(file)).toList
      label = lbl.toInt
    } yield IndividualsTweets(tweets, username, Some(label), state=None)).toSeq
  }

  private def usernameForFile(file: File) = {
    FilenameUtils.removeExtension(file.getName)
  }
}