package org.clulab.twitter4food.featureclassifier

import java.util.regex.Pattern

import edu.arizona.sista.learning._
import edu.arizona.sista.processors.fastnlp.FastNLPProcessor
import edu.arizona.sista.struct.Counter
import org.clulab.twitter4food.struct.{FeatureExtractor, TwitterAccount}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, Map, HashSet}
import scala.io.Source

/**
  * Created by adikou on 1/22/16.
  */
class HumanClassifier extends FeatureClassifier {
  var subClassifier:Option[Classifier[String, String]] = None // isEmpty, isDefined, get (to access the element explicitly), getOrElseCreate

  val featureExtractor = new FeatureExtractor() // TODO: parameterize this with humanOrNot params

  override def scoresOf(account: TwitterAccount): Counter[String] = {
    if(subClassifier.isDefined) {
      subClassifier.get.scoresOf(featureExtractor.mkDatum(account, ""))
    } else {
      throw new RuntimeException("ERROR: must train before using scoresOf!")
    }
  }

  /**
    * Training from a set of users
    * Creates the subClassifier object as the output
    */
  override def train(accounts: Seq[TwitterAccount], labels: Seq[String]): Unit = {
    // TODO: create RVFDataset from accounts
    // TODO: actually train the subClassifier
  }
}

/*
class HumanClassifier extends FeatureClassifier {

  var dataset = new RVFDataset[String, String]()
  var subClassifier = new LinearSVMClassifier[String, String]()
  var labels = List("unknown", "human", "org")
  var trainingLabels = mutable.Map.empty[String, String]
  var trainingSet = ArrayBuffer.empty[TwitterAccount]

  val UNKNOWN = 0
  val HUMAN = 1
  val ORG = 2

  val DIR_RSC = "src/main/resources/"
  val DIR_FEAT = "org/clulab/twitter4food/featureclassifier/human/"
  val FILE_DEFAULT_TRAINING_SET = "DefaultTrainingSet.txt"
  val FILE_DEFAULT_TRAINING_LABELS = "DefaultLabels.txt"
  val FILE_HUMAN_NAMES = "HumanNames.txt"
  val FILE_HUMAN_FEAT = "HumanFeatures.txt"
  val FILE_ORG_NAMES = "OrgNames.txt"
  val FILE_ORG_FEAT = "OrgFeatures.txt"

  val numFeatures = 4
  val FEAT_HUMAN_NAME = 0
  val FEAT_ORG_NAME = 1
  val FEAT_HUMAN_FEAT = 2
  val FEAT_ORG_FEAT = 3
  val features = new Array[mutable.HashSet[String]](numFeatures)

  // TODO
  // Overload constructor to tune SVM classifier hyperparameter C

  def getFeaturesCounter(user: TwitterAccount): Counter[String] = {
    val name = user.getName
    val description = user.getDescription
    val handle = user.getHandle

    val p = Pattern.compile("[.,?!\"\\:']")
    val m = p.matcher(description)
    val desc = m.replaceAll(" ").toLowerCase.split("\\s+")

    val counter = new Counter[String]()
    val proc = new FastNLPProcessor()
    val doc = proc.annotate(description)

    var humanCounter = 0
    var orgCounter = 0

    println(s"Handle: $handle ")

    for (i <- FEAT_HUMAN_NAME to FEAT_ORG_FEAT) {
      val it = features(i).iterator
      while (it.hasNext) {
        val str = it.next.toLowerCase

        if (i < FEAT_HUMAN_FEAT) {
          if (str.contains(handle.substring(1).toLowerCase)
            || handle.substring(1).toLowerCase.contains(str)) {
            if (i == FEAT_HUMAN_NAME) humanCounter += 1 else orgCounter += 1
          }

          for (n <- name.toLowerCase.split("\\s+")) {
            if (n.equals(str)) {
              if (i == FEAT_HUMAN_NAME) humanCounter += 1 else orgCounter += 1
            }
          }
        }

        desc.foreach {
          d => if (d.equals(str))
            if (i == FEAT_HUMAN_FEAT) humanCounter += 1 else orgCounter += 1
        }
      }
    }

    counter.incrementCount(labels(HUMAN), humanCounter)
    counter.incrementCount(labels(ORG), orgCounter)

    val nounsAndVerbs = ArrayBuffer[String]()
    for(s <- doc.sentences) {
      val words = s.words
      val tags = s.tags.get

      for(i <- 0 until tags.length)
        if(tags(i).matches("NN") || tags(i).matches("NNS")
        || tags(i).matches("VB(.*)"))
          nounsAndVerbs += words(i)
    }

    nounsAndVerbs.foreach(x => counter.incrementCount(x, 1))

    counter
  }

  def populateDatum(user: TwitterAccount,
                    isTrain: Boolean): Datum[String, String] = {
    val label = if(!isTrain) labels(UNKNOWN)
                else trainingLabels(user.getHandle)
    new RVFDatum[String, String](label, getFeaturesCounter(user))
  }

  /** Training from a set of users */
  override def train(users: Seq[TwitterAccount]) = {
    users.foreach(user => dataset += populateDatum(user, isTrain = true))
    subClassifier.train(dataset)
  }

  /** Use default training set */
  override def train() = train(trainingSet)

  /** Predicting the given feature (or a distribution of) for a given account */
  override def classify(user: TwitterAccount): String = {
    subClassifier.classOf(populateDatum(user, isTrain = false))
  }

  override def assignLabels(users: Seq[TwitterAccount]): Unit = {
    val lines = Source.fromFile(DIR_RSC + DIR_FEAT +
                                FILE_DEFAULT_TRAINING_LABELS).getLines.toList
    for(line <- lines) {
      val splits = line.split(",")
      trainingLabels += (splits(0) -> splits(1).trim)
    }
  }

  override def assignLabels() = assignLabels(trainingSet)

  def loadTrainingSet() = {
    val lines = Source.fromFile(DIR_RSC + DIR_FEAT + FILE_DEFAULT_TRAINING_SET)
                      .getLines.toList
    var count = 0
    var account: TwitterAccount = null

    lines foreach {
      line => {
        val str = line.split("\t")
        if(str.nonEmpty) {
          count match {
            case 0 => account = new TwitterAccount()
                      account.setHandle(str(0))
                             .setUrl(str(1))
            case 1 => account.setDescription(str(0))
            case 2 => account.setName(str(0))
                             .setLocation(str(1))
                             .setLang(str(3))
            case 3 => trainingSet += account
          }
        }

        count += 1
        count %= 4
      }
    }
  }

  def createFeatures() = {
    for(i <- FEAT_HUMAN_NAME to FEAT_ORG_FEAT) {
      var fin = DIR_RSC + DIR_FEAT
      i match {
        case FEAT_HUMAN_NAME => fin += FILE_HUMAN_NAMES
        case FEAT_ORG_NAME => fin += FILE_ORG_NAMES
        case FEAT_HUMAN_FEAT => fin += FILE_HUMAN_FEAT
        case FEAT_ORG_FEAT => fin += FILE_ORG_FEAT
      }

      features(i) = new mutable.HashSet[String]()

      val lines = Source.fromFile(fin).getLines.toList
      lines.foreach(x => features(i) += x)
    }
  }

  def init() = {
    loadTrainingSet()
    assignLabels()
    createFeatures()
  }
}
*/
