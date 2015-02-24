package edu.arizona.sista.twitter4food

import java.io._
import edu.arizona.sista.learning._
import Mixins._
import scala.util.Random
import edu.arizona.sista.processors.corenlp._
import scala.io.Source
import scala.throws
import collection.JavaConversions._

import SentimentClassifier._
import edu.arizona.sista.struct._

object SentimentClassifier {

  // http://sentiment.christopherpotts.net/lingstruc.html
  val negation = ("(?:^(?:never|no|nothing|nowhere|noone|none|not|havent|hasnt|hadnt|cant|couldnt|shouldnt|wont" +
    "|wouldnt|dont|doesnt|didnt|isnt|arent|aint)$)|n't").r

  val clauseLevelPunctuation = "^[.:;!?]$".r

  val emoticon = """[<>]?[:;=8][\-o\*\']?[\)\]\(\[dDpP/\:\}\{@\|\\]|[\)\]\(\[dDpP/\:\}\{@\|\\][\-o\*\']?[:;=8][<>]?""".r

  val hashtag = """\#+[\w_]+[\w\'_\-]*[\w_]+""".r

  val tokenMatcher = "[\\p{L}\\p{M}\\p{N}[.:;!?']]+".r
/**
 * Implementation of Potts' method of labelling negated words:
   * http://sentiment.christopherpotts.net/lingstruc.html
   * label any tokens between a negation word and a sentence level clause punctuation with _NEG
   */
  val stopwords = Set("#happy", "#sad") // ++ Experiment.stopWords

  def train(positiveFile: Option[String],
            neutralFile: Option[String],
            negativeFile: Option[String]) = {
    val files = new collection.mutable.ListBuffer[(Int, String)]
    println("collecting tokens")
    val tokensByClass: Map[Int, Seq[Seq[String]]] = (for {
      (label, Some(file)) <- List(
        (-1, negativeFile),
        (0, neutralFile),
        (1, positiveFile)
      )
    } yield label -> TweetParser.parseTweetFile(file).map(tweet => tweet.tokens.toList)).toMap

    val minSize = tokensByClass.values.map(_.size).min

    val truncated = tokensByClass.mapValues(Random.shuffle(_).take(minSize))

    println("training classifier")
    new SentimentClassifier(truncated)
  }

  def defaultTokens = {
    val positiveFile = Source.fromURL(getClass.getResource("sentiment/happy.tweets"))
    val neutralFile = Source.fromURL(getClass.getResource("sentiment/random.tweets"))
    val negativeFile = Source.fromURL(getClass.getResource("sentiment/sad.tweets"))

    println("Reading in data...")
    val tokensByClass: Map[Int, Seq[Seq[String]]] = (for {
      (label, file) <- List((-1, negativeFile),
        (0, neutralFile),
        (1, positiveFile))
    } yield label -> TweetParser.parseTweetFile(file).map(tweet => tweet.tokens.toList)).toMap
    tokensByClass
  }

  def defaultClassifier = {
    new SentimentClassifier(defaultTokens)
  }

  def main(args: Array[String]) {
    println("working directory " + new File(".").getAbsolutePath)

    println(Runtime.getRuntime.maxMemory / (1024 * 1024))

    val tokensByClass = defaultTokens

    val minSize = tokensByClass.values.map(_.size).min

    val trainingSize: Int = minSize * 9 / 10
    val testingSize: Int = minSize / 10

    val trainingTokens: Map[Int, Seq[Seq[String]]] = tokensByClass.mapValues(_.take(trainingSize))
    val testingTokens: Map[Int, Seq[Seq[String]]] = tokensByClass.mapValues(_.drop(trainingSize).take(testingSize))

    val clf = new SentimentClassifier(trainingTokens)

    for ((datasetName, tokenSet) <- Map("TRAINING" -> trainingTokens, "TESTING" -> testingTokens)) {
      println(datasetName)

      val data: Seq[RVFDatum[Int, String]] = clf.data(tokenSet.mapValues(_ map clf.features))


      val actualLabels = data map (_.label)

      val predictedLabels = data map clf.predict

      val numCorrect = (predictedLabels zip actualLabels).map({
        case (p, a) => Utils.indicator(p == a)
      }).sum

      println(s"accuracy: $numCorrect / ${data.size} = ${numCorrect.toFloat / data.size}")

      println(ContingencyStats(predictedLabels, actualLabels))

      println("confusion matrix:")
      println(ConfusionMatrix(predictedLabels, actualLabels))
    }
  }

  def loadFrom(stream: InputStream): SentimentClassifier = {
    //val clf: LiblinearClassifier[Int,String] = LiblinearClassifier.loadFrom(stream)
    //new SentimentClassifier(clf.asInstanceOf[LinearSVMClassifier[Int,String]])
    sys.error("implement loadFrom")
  }

  def loadFrom(fileName: String): SentimentClassifier = loadFrom(new FileInputStream(fileName))

  def resourceClassifier =
    //loadFrom(getClass.getResource("sentiment/trainedSentimentClassifier.dat").openStream)
  sys.error("implement resourceClassifier")
}

class SentimentClassifier(var classifier: LinearSVMClassifier[Int, String]) extends Serializable {

  var processor = new CoreNLPProcessor

  def this(tokensByClass: Map[Int,Seq[Seq[String]]]) = {
    this(null:LinearSVMClassifier[Int,String])
    println("extracting features")
    val featuresByClass: Map[Int, Seq[Counter[String]]] = tokensByClass.mapValues(_ map features).map(identity)
    println("training classifier")
    classifier = train(dataset(data(featuresByClass)))
  }

  def saveTo(fileName: String) = classifier.saveTo(fileName)

  // this doesn't help at all
  def labelNegation(tokens: Seq[String]): Seq[Boolean] =
    tokens.scanLeft(false) {
      case (insideNegated, token) =>
        (negation matches token) || (insideNegated && !(clauseLevelPunctuation matches token))
    }

  def labelledDatum(label: Int, features: Counter[String]) =
    new RVFDatum(label, features)

  def unlabelledDatum(features: Counter[String]) = {
    labelledDatum(0, features)
  }

  def okToken(token: String): Boolean =
    (tokenMatcher.matches(token) || emoticon.matches(token) || hashtag.matches(token)) && !stopwords.contains(token)

  def process(tokens: Seq[String]): Seq[String] = {
    val lowerCased: Seq[String] = tokens map (_.toLowerCase)
    val negationLabels: Seq[Boolean] = labelNegation(lowerCased)
    val filtered = (lowerCased zip negationLabels) filter { case (token, label) => okToken(token) }
    filtered.map {
      case (token, negated) => if (negated) token + "_NEG" else token
    }
  }

  def tokenize(text: String): Seq[String] =
    processor.mkDocument(text).sentences.map(_.words).flatten.toSeq

  def features(tokens: Seq[String]): Counter[String] =
    new Counter(process(tokens))

  def structuredData(featuresByClass: Map[Int, Seq[Counter[String]]]): Map[Int, Seq[RVFDatum[Int, String]]] = for {
    (label, featureList) <- featuresByClass
  } yield label -> (featureList map (features => labelledDatum(label, features)))

  def data(featuresByClass: Map[Int, Seq[Counter[String]]]): Seq[RVFDatum[Int, String]] = {
    structuredData(featuresByClass).values.flatten.toSeq
  }

  def dataset(data: Seq[Datum[Int, String]]): Dataset[Int, String] = {
    val dataset = new RVFDataset[Int, String]
    for (datum <- data) dataset += datum
    dataset
  }

  private def train(dataset: Dataset[Int, String]): LinearSVMClassifier[Int, String] = {
    val classifier = new LinearSVMClassifier[Int, String]
    classifier.train(dataset)
    classifier
  }

  def predict(datum: Datum[Int, String]): Int = classifier.classOf(datum)

  def predict(string: String): Int = classifier.classOf(unlabelledDatum(features(tokenize(string))))
}
