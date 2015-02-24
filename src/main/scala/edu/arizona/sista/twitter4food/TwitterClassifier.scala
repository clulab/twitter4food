package edu.arizona.sista.twitter4food

import edu.arizona.sista.processors.corenlp.CoreNLPProcessor
import edu.arizona.sista.struct._
import edu.arizona.sista.learning._
import Mixins._

/**
 * Created by dfried on 4/28/14.
 */
object TwitterClassifier {
  val emoticon = """[<>]?[:;=8][\-o\*\']?[\)\]\(\[dDpP/\:\}\{@\|\\]|[\)\]\(\[dDpP/\:\}\{@\|\\][\-o\*\']?[:;=8][<>]?""".r

  val hashtag = """\#+[\w_]+[\w\'_\-]*[\w_]+""".r

  val tokenMatcher = "[\\p{L}\\p{M}\\p{N}[.:;!?']]+".r
}

case class TCProperties(normalization: NormalizationType = NoNorm)

abstract class TwitterClassifier[L](var classifier: LinearSVMClassifier[L, String],
                                    val properties: TCProperties) extends Serializable {
  import TwitterClassifier._
  /*
  // http://stackoverflow.com/questions/3307427/scala-double-definition-2-methods-have-the-same-type-erasure
  case class Tokens(list: Seq[String])
  case class Text(text: String)
  */

  var featureNormalizer: Option[CounterProcessor[String]] = None

  def stopwords: Set[String]

  def this(tokensByClass: Map[L, Seq[Seq[String]]], properties: TCProperties = TCProperties()) = {
    this(null:LinearSVMClassifier[L,String], properties)
    println("extracting features")
    val featuresByClass: Map[L, Seq[Counter[String]]] = tokensByClass.mapValues(_ map features).map(identity)
    println("creating feature normalizer")
    featureNormalizer = Some(new CounterProcessor(featuresByClass.values.flatten.toSeq, properties.normalization, None, None))
    println("normalizing features")
    val normalizedFeatures = featuresByClass.mapValues(_.map(counter => featureNormalizer.get.apply(counter)))
    println("training classifier")
    classifier = train(dataset(data(normalizedFeatures)))
  }

  def saveTo(fileName: String) = classifier.saveTo(fileName)

  def labelledDatum(label: L, features: Counter[String]) =
    new RVFDatum(label, features)


  def okToken(token: String): Boolean =
    (tokenMatcher.matches(token) || emoticon.matches(token) || hashtag.matches(token)) && !stopwords.contains(token)

  def process(tokens: Seq[String]): Seq[String] =
    tokens map (_.toLowerCase) filter okToken

  def features(tokens: Seq[String]): Counter[String] =
    new Counter(process(tokens))

  def structuredData(featuresByClass: Map[L, Seq[Counter[String]]]): Map[L, Seq[RVFDatum[L, String]]] = for {
    (label, featureList) <- featuresByClass
  } yield label -> (featureList map (features => labelledDatum(label, features)))

  def data(featuresByClass: Map[L, Seq[Counter[String]]]): Seq[RVFDatum[L, String]] = {
    structuredData(featuresByClass).values.flatten.toSeq
  }

  def dataset(data: Seq[Datum[L, String]]): Dataset[L, String] = {
    val dataset = new RVFDataset[L, String]
    for (datum <- data) dataset += datum
    dataset
  }

  def train(dataset: Dataset[L, String]): LinearSVMClassifier[L, String] = {
    val classifier = new LinearSVMClassifier[L, String]
    classifier.train(dataset)
    classifier
  }

  def predict(datum: Datum[L, String]): L = classifier.classOf(datum)

  def predict(default:L, tokens: Seq[String]): L = {
    val feats = features(tokens)
    val normedFeats = featureNormalizer match {
      case None => feats
      case Some(normalizer) => normalizer(feats)
    }
    classifier.classOf(labelledDatum(default, normedFeats))
  }

  // def weights: Option[Map[L, Counter[String]]] = Some(classifier.getWeights(verbose = false))

  // def predict(default:L, string: String): L = predict(default, tokenize(string))
}
