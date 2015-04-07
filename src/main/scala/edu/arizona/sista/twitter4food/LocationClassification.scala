package edu.arizona.sista.twitter4food

import scala.util.Random

import collection.JavaConversions._
import java.util.ArrayList
import java.io.{File, PrintWriter}
import edu.arizona.sista.struct.Counter
import Mixins._
import edu.arizona.sista.learning._
import edu.arizona.sista.utils.EvaluationStatistics

/**
 * Created by dfried on 4/16/14.
 */
object NycLaClassification {
  def main(args: Array[String]) = {

    val solrPager = if (args.size > 0) {
        new SolrPager(args(0))
    }
    else {
        new SolrPager
    }

    val nyQuery = "place:*New\\ York* OR userLocation:*NYC* OR userLocation:*New\\ York\\ City*"
    val laQuery = "place:*Los\\ Angeles* OR userLocation:*Los\\ Angeles* OR (userLocation:*LA* AND timeZone:Pacific*)"

    val nyTweets = Random.shuffle(solrPager.queryTokens(nyQuery))
    val laTweets = Random.shuffle(solrPager.queryTokens(laQuery))

    val N = Math.min(nyTweets.size, laTweets.size)

    val N_training = (N * 0.95).toInt
    val N_testing = (N * 0.05).toInt

    println("ny tweets:")
    println(nyTweets.size)
    println(nyTweets.take(10).mkString("\n"))
    println
    println("la tweets:")
    println(laTweets.size)
    println(nyTweets.take(10).mkString("\n"))

    val trainingSet = Map(
      "ny" -> nyTweets.take(N_training),
      "la" -> laTweets.take(N_training)
    )

    val testingSet = Map(
      "ny" -> nyTweets.drop(N_training).take(N_testing),
      "la" -> laTweets.drop(N_training).take(N_testing)
    )
    val classifier = new NycLaClassifier(trainingSet.mapValues(_.map(_.toSeq)))

    val testingPredictions = testingSet.mapValues(_.map(txt => classifier.predict("", txt)))

    val correctCount = for {
      (label, predictions) <- testingPredictions
    } yield predictions.filter(_ == label).size

    val nyWeights = classifier.classifier.getWeights(verbose = false)("ny")

    println("num training points in each class")
    println(trainingSet.mapValues(_.size))

    println("num testing points in each class")
    println(testingSet.mapValues(_.size))

    println("num correct in each class:")
    println(correctCount)

    println("weight analysis")
    val sortedWeights = nyWeights.sorted
    println("+\t-")
    def rep(list: Seq[(String, Double)]): Seq[String] = 
        list.map({case (feature, weight) => "%s %.4f".format(feature, weight)})
    println(rep(sortedWeights.take(25)).zip(rep(sortedWeights.reverse.take(25))).map({ case (s1, s2) => "%s\t%s".format(s1, s2) }).mkString("\n"))
  }
}

class NycLaClassifier(tweetsByLocation: Map[String, Seq[Seq[String]]],
                       properties: TCProperties = TCProperties()) extends TwitterClassifier[String](tweetsByLocation, properties) {
  lazy val stopwords = Set("#nyc", "#newyork", "#losangeles", "#la")
}

//class LocationClassifier(var classifier: LinearSVMClassifier[String, String],
class LocationClassifier(var classifier: RandomForestClassifier[String, String],
                         val properties: TCProperties,
                         val annotators: Seq[Annotator],
                         val tokenType: TokenType,
                         val numTrees: Int,
                         val maxTreeDepth: Int,
                         val ngramThreshold: Option[Int]) {

  def process(tweet: Tweet): Seq[String] =
    TweetView.view(annotators)(tokenType)(tweet)


  def features(tweets: Seq[Tweet]): Counter[String] = {
    val processed = (tweets map process)
    (new Counter[String](processed.flatten)) / (tweets.size / 1000.0)
  }

  def insertionLocation(x: Float, ys: Seq[Float]): Int = {
    for ((y, i) <- ys.zipWithIndex) if (y >= x) return i
    return ys.length
  }

  def quantiles(q: Int)(values: Seq[Float]): Seq[Float] = {
    // R-2, SAS-5, http://en.wikipedia.org/wiki/Quantile#Estimating_the_quantiles_of_a_population
    val sortedValues = values.sorted
    val N = values.length
    for {
      p <- 1 until q
      h = p.toFloat * N / q + 0.5
      lower = sortedValues(Math.ceil(h - 0.5).toInt - 1)
      upper = sortedValues(Math.floor(h + 0.5).toInt - 1)
    } yield (lower + upper) / 2
  }

  def binVals(numberOfBins: Int)(m: Map[String, Counter[String]]): (Map[String, Counter[String]], Option[Map[String, Seq[Float]]]) = {
    numberOfBins match {
      case noBinning if numberOfBins < 2 => (m, None)
      case _ =>
        val splits = (for {
          feat <- (for (c <- m.values) yield c.keySet).toSeq.flatten
        } yield (feat, quantiles(numberOfBins)((for (c <- m.values) yield c.getCount(feat).toFloat).toSeq))).toMap

        val binned = (for {
          (key, count) <- m
          c = new Counter[String]
          dummy = for (feat <- count.keySet) c.setCount(feat, insertionLocation(count.getCount(feat).toFloat, splits(feat)))
        } yield key -> c).toMap

        (binned, Some(splits))
    }
  }

  def processTweet(tweet: Tweet): Seq[String] = {
    TweetView.view(annotators)(tokenType)(tweet)
  }

  def mkViewFeatures(ngramThreshold: Option[Int])(tweets: Seq[Tweet]): Counter[String] = {
    val feats = new Counter[String](tweets.map(processTweet).flatten)
    val thresh = ngramThreshold match {
      case None => feats
      case Some(k) => feats.filterValues(_ >= k)
    }
    // scale each feature by the number of tweets aggregated (but divide by 1000 to avoid SVM numerical instability)
    thresh / (tweets.size / 1000.0)
  }

  //def train(dataset: Dataset[String, String]): LinearSVMClassifier[String, String] = {
    //val classifier = new LinearSVMClassifier[String, String](bias=true)
  def train(dataset: Dataset[String, String], numTrees: Int, maxTreeDepth: Int): RandomForestClassifier[String, String] = {
    val classifier = new RandomForestClassifier[String, String](numTrees = numTrees, maxTreeDepth = maxTreeDepth)
    classifier.train(dataset)
    classifier
  }

  def this(documentsByClass: Map[String, Seq[Tweet]], properties: TCProperties = TCProperties(),
           annotators: Seq[Annotator] = List(), tokenType: TokenType = AllTokens, numTrees: Int = 1000, maxTreeDepth: Int = 0,
           ngramThreshold: Option[Int] = None) = {
    // this(null:LinearSVMClassifier[String,String], properties, annotators, tokenType)
    this(null:RandomForestClassifier[String,String], properties, annotators, tokenType, numTrees, maxTreeDepth, ngramThreshold)
    println("extracting features")
    // val featuresByClass: Map[String, Counter[String]] = documentsByClass.mapValues(features).map(identity)
    val featuresByClass: Map[String, Counter[String]] = documentsByClass.mapValues(mkViewFeatures(ngramThreshold)).map(identity)
    // println("creating feature normalizer")
    // featureNormalizer = Some(new CounterProcessor(featuresByClass.values.flatten.toSeq, properties.normalization, None, None))
    // println("normalizing features")
    // val normalizedFeatures = featuresByClass.mapValues(_.map(counter => featureNormalizer.get.apply(counter)))
    println("training classifier")
    classifier = train(dataset(data(featuresByClass)), numTrees, maxTreeDepth)
  }

  def saveTo(fileName: String) = classifier.saveTo(fileName)

  def labelledDatum(label: String, features: Counter[String]) =
    new RVFDatum(label, features)

  def data(featuresByClass: Map[String, Counter[String]]): Seq[RVFDatum[String, String]] = for {
    (label, features) <- featuresByClass.toSeq
  } yield labelledDatum(label, features)

  def dataset(data: Seq[Datum[String, String]]): Dataset[String, String] = {
    val dataset = new RVFDataset[String, String]
    for (datum <- data) dataset += datum
    dataset
  }

  def predict(datum: Datum[String, String]): String = classifier.classOf(datum)

  def predict(default:String, tweets: Seq[Tweet]): String =
    classifier.classOf(labelledDatum(default, features(tweets)))


  //def weights: Option[Map[String, Counter[String]]] = Some(classifier.getWeights(verbose = false))
  def weights: Option[Map[String, Counter[String]]] = None

}

trait LocaleType
case object Region extends LocaleType
case object State extends LocaleType
case class TopCities(howMany: Int) extends LocaleType

object LocationClassifier {
  // experiment parameters
  val dropStateThreshold: Option[Int] = None
  val DO_ABLATION = true

  val localeType: LocaleType = TopCities(15)

  val normalizationType: NormalizationType = NoNorm

  def classifySet(training: Map[String, Seq[Tweet]], testing: Map[String, Seq[Tweet]], annotators: Seq[Annotator],
                  tokenType: TokenType, numTrees: Int, maxTreeDepth: Int, ngramThreshold: Option[Int]) = {
    println("training classifier")
    val classifier = new LocationClassifier(training, TCProperties(normalizationType), annotators, tokenType, numTrees, maxTreeDepth, ngramThreshold)
    println("evaluating classes")
    (testing.mapValues(tweets => classifier.predict("", tweets)), classifier.weights)
  }

  def main(args: Array[String]) = {
    val pw = if (args.size > 0)
      (new PrintWriter(new File(args(0))))
    else
      (new PrintWriter(System.out))

    val solrPager = new SolrPager()
    lazy val groupedTweets = localeType match {
      case Region => solrPager.tweetsGroupedByRegion(Datasets.regionsCoarse)
      case State => dropStateThreshold match {
        case None => solrPager.tweetsGroupedByLocation
        case Some(k) => solrPager.tweetsGroupedByLocation.filter({case (state, tweets) => tweets.size >= k})
      }
      case TopCities(k) => solrPager.tweetsGroupedByCity(k)
    }

    // get the tokenized tweets grouped by location or region, and sorted by post time
    val tweetsByLocation: Map[String, Seq[Tweet]] =
      groupedTweets.mapValues(_.sortBy(_.postTime).toList)

    pw.println(s"number of tweets per state: ${tweetsByLocation.mapValues(_.length).toSeq.sortBy(-_._2)}")
    pw.println

    val minNum: Int = tweetsByLocation.values.map(_.length).min
    pw.println(s"minimum number of tweets is ${minNum}")

    val numTraining = tweetsByLocation.mapValues(tweets => (tweets.length * 0.8).toInt)

    def makeTraining(fracToTake: Double) = for {
        (name, tweets) <- tweetsByLocation
        toTake = (numTraining(name) * fracToTake).toInt
      } yield name -> tweets.take(toTake)

    def makeTesting(fracToTake: Double) = for {
      (name, tweets) <- tweetsByLocation
      toTake = ((tweets.size - numTraining(name)) * fracToTake).toInt
    } yield name -> tweets.drop(numTraining(name)).take(toTake)

    if (DO_ABLATION) {
      val predictionsAndWeights: List[((TokenType, Int, Int), (Map[String, String], Option[Map[String, Counter[String]]]))] = for {
        tokenType <- List(AllTokens, HashtagTokens, FoodTokens, FoodHashtagTokens)
        annotators <- List(List(LDAAnnotator(tokenType)))//, List())
        trainingTweets = makeTraining(1.0)
        testingTweets = makeTesting(1.0)
        numTrees <- List(4,5,6,7,8,9,10)
        maxTreeDepth <- List(2,3,4,5)
        ngramThreshold <- List(Some(4),Some(5),Some(6),Some(7),Some(8),Some(9),Some(10))
      } yield (tokenType, numTrees, maxTreeDepth) -> classifySet(trainingTweets, testingTweets, annotators, tokenType, numTrees, maxTreeDepth, ngramThreshold)

      val byTokenType = predictionsAndWeights.groupBy({case ((tt, _, _), _ ) => tt})

      for ((tokenType, subPredsAndWeights) <- byTokenType) {
        pw.println(s"tokenType: s$tokenType")
        val (unigramPredictions, _) = predictionsAndWeights.filter({ case ((tt, _, _), _) => tt == tokenType; case _ => false}).head._2

        //scala.util.Random.setSeed(0L)
        // just get the state names from something we've already calculated
        val stateNames = unigramPredictions.keySet.toSeq
        // compute a random guessing baseline by randomly guessing one state
        val guessBaseline = stateNames map (_ => stateNames.head)
        pw.println("\t" + "baseline")
        val shuffledAcc = (new EvaluationStatistics[String](guessBaseline, stateNames)).accuracy * 100
        pw.println(f"\t\t$shuffledAcc%2.2f")

        // now iterate over the different annotator models for this token type
        for (((_, numTrees, maxTreeDepth), (predictions, weights)) <- subPredsAndWeights) {
          pw.println("\t" + "number of trees: " + numTrees + ", maximum depth: " + maxTreeDepth)
          val (actual, predicted) = predictions.toSeq.unzip
          // if we're using the unannotated version, then use a random permutation of the states as the baseline
          val baseline = actual.map(unigramPredictions)
          val accuracy = (new EvaluationStatistics[String](predicted, actual)).accuracy * 100
          val pValue = EvaluationStatistics.classificationAccuracySignificance(predicted, baseline, actual)
          pw.println(f"\t\t$accuracy%2.2f ($pValue%2.2f)")
        }
      }
    } /*else { // learning curves
      val results = (for {
        trainingFraction <- List(0.20, 0.40, 0.60, 0.80, 1.0).par
        testingFraction <- List(0.20, 0.40, 0.60, 0.80, 1.0)
        trainingTweets = makeTraining(trainingFraction)
        testingTweets = makeTesting(testingFraction)
        (predictions, weights) = classifySet(trainingTweets, testingTweets, List(LDAAnnotator(AllTokens)), AllTokens)
      } yield (trainingFraction, testingFraction) -> (predictions, weights)).seq.toMap

      def acc[L](pairs: Seq[(L, L)]) = {
        pairs.filter({ case (a, b) => a == b}).size.toFloat / pairs.size
      }

      val predictions = results.mapValues(_._1)
      val accuracies = predictions.mapValues(pred => acc(pred.toSeq))
      val weights = results.mapValues(_._2)

      pw.println
      pw.println("accuracies:")
      pw.println(accuracies.toSeq.sortBy(_._1).mkString("\n"))
      pw.println
      val (trainTest, maybeWeights) = weights.toSeq.sortBy(_._1).last
      val (_, fullPredictions) = predictions.toSeq.sortBy(_._1).last
      pw.println(trainTest)
      pw.println(fullPredictions)
      maybeWeights match {
        case None => pw.println("no weights!")
        case Some(map) => for ((locale, cntr) <- map.toSeq.sortBy(_._1)) {
          pw.println(locale)
          pw.println(s"\t +: ${cntr.sorted.take(20).map(_._1).mkString("\t")}")
          pw.println(s"\t -: ${cntr.sorted.reverse.take(20).map(_._1).mkString("\t")}")
        }
      }
    }*/
    pw.close()
  }
}
