package org.clulab.twitter4food.lda

import java.io.{File, PrintWriter, Serializable}
import java.util.ArrayList

import scala.collection.JavaConversions.asScalaBuffer

import cc.mallet.pipe.{Pipe, SerialPipes, TokenSequence2FeatureSequence}
import cc.mallet.topics.{ParallelTopicModel, TopicModelDiagnostics}
import cc.mallet.types._
import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.struct.FeatureExtractor._
import org.clulab.twitter4food.util.FileUtils
import org.clulab.utils.Serializer
import org.slf4j.LoggerFactory


/**
  * Create topics from Tweets using MALLET. Relies heavily on edu.arizona.sista.twitter4food.LDA by Daniel Fried.
  *
  * @author Dane Bell
  * @date 06-13-2016
  */

class LDA(val model: ParallelTopicModel, val pipe: SerialPipes) extends Serializable {
  def distributions(tokens: Seq[String],
                    numIterations: Int = 10,
                    thinning: Int = 1,
                    burnIn: Int = 5) = {
    val inst = pipe.instanceFrom(LDA.mkInstance(tokens))
    model.getInferencer.getSampledDistribution(inst, numIterations, thinning, burnIn)
  }

  def mostLikelyTopic(tokens: Seq[String]): Int = {
    val dists = distributions(tokens)
    dists.zipWithIndex.max._2
  }
}

object LDA {

  case class Config(numTopics:Int = 200, numIterations:Int = 100)

  val logger = LoggerFactory.getLogger(this.getClass)
  val config = ConfigFactory.load

  val stopWordsFile = scala.io.Source.fromFile(config.getString("classifiers.features.stopWords"))
  val ldaStopWords = stopWordsFile.getLines.toSet ++ Set("<URL>", "<@MENTION>") ++ Set("breakfast", "lunch", "dinner", "supper", "brunch", "snack")
  stopWordsFile.close

  def filterLDAStopWords(tokens: Array[String]): Array[String] = tokens filterNot ldaStopWords.contains

  def stripHashtag(token: String) = {
    if (token.startsWith("#")) token.substring(1, token.length) else token
  }

  def mkInstance(tokens: Seq[String], tokenFilter: (Array[String] => Array[String]) = filterStopWords): Instance = {
    val t = tokenFilter(tokens map stripHashtag toArray)
    new Instance(new TokenSequence(t.map(new Token(_)).toArray), null, null, null)
  }

  def train(tokensList: Seq[Array[String]], numTopics: Int = 200, numIterations: Int = 2000): (LDA, Alphabet) = {
    // Begin by importing documents from text to feature sequences
    val pipeList = new java.util.ArrayList[Pipe]

    pipeList.add( new TokenSequence2FeatureSequence() )

    val pipe = new SerialPipes(pipeList)

    val instances = new InstanceList (pipe)

    val pb = new me.tongfei.progressbar.ProgressBar("train()", 100)
    pb.start
    pb.maxHint(tokensList.size)
    pb.setExtraMessage("Populating...")

    for (tokens <- tokensList) {
      instances.addThruPipe(mkInstance(tokens))
      pb.step
    }
    pb.stop

    val model = new ParallelTopicModel(numTopics, 2.0, 0.01)

    model.addInstances(instances)

    // Use X parallel samplers, which each look at 1 / X of the corpus and combine
    //  statistics after every iteration.
    model.setNumThreads(20)

    // System.out.printf("running for %d iterations", numIterations)
    model.setNumIterations(numIterations)
    model.estimate
    val lda = new LDA(model, pipe)

    val alphabet = instances.getDataAlphabet

    (lda, alphabet)
  }

  def load(filename: String): LDA = Serializer.load[LDA](filename)

  def save(lda: LDA, filename: String): Unit = Serializer.save[LDA](lda, filename)

  def main(args: Array[String]) = {
    def parseArgs(args: Array[String]): Config = {
      val parser = new scopt.OptionParser[Config]("lda") {
        opt[Int]('t', "topics") action { (x, c) =>
          c.copy(numTopics = x)} text "number of topics to produce"
        opt[Int]('i', "iterations") action { (x, c) =>
          c.copy(numIterations = x)} text "number of LDA iterations to run"
        help("help") text "prints this usage text"
      }
      parser.parse(args, Config()).get
    }

    val params = parseArgs(args)
    val config = ConfigFactory.load

    logger.info(s"Loading and filtering tweets...")

    val twoLine: Seq[Array[String]] = (for {
      file <- config.getStringList("lda.2lineTrainingData").toSeq
    } yield FileUtils.loadTwoLineTexts(file, englishOnly = true))
      .flatten
      .map(tweet => tweet.split("\\s+"))
      .map(filterLDAStopWords)

    val threeLine: Seq[Array[String]] = (for {
      file <- config.getStringList("lda.3lineTrainingData").toSeq
    } yield FileUtils.loadThreeLineTexts(file, englishOnly = true))
      .flatten
      .map(tweet => tweet.split("\\s+"))
      .map(filterLDAStopWords)

    val tweets = twoLine ++ threeLine

    logger.info(s"Tweets: ${tweets.length}")

    val (lda, alphabet) = LDA.train(tweets, params.numTopics, params.numIterations)

    logger.info(s"Exporting model...")

    val topicModel = lda.model.getSortedWords

    val textOutFile = new PrintWriter(new File(config.getString("lda.modelDir") +
      s"""/lda_${params.numTopics}t_${params.numIterations}i.txt"""))

    for {
      topic <- 0 until params.numTopics // which topic
      iterator = topicModel.get(topic).iterator()
    } {
      val wds = for {
        j <- 0 to 4 // number of words to display
        if iterator.hasNext
        idCountPair = iterator.next
      } yield alphabet.lookupObject(idCountPair.getID).toString
      textOutFile.write(s"""(${wds.mkString(", ")})\n""")
    }

    if (config.getBoolean("lda.verbose")) {
      val diag = new TopicModelDiagnostics(lda.model, 10)
      textOutFile.write("\n" + diag.toString)
    }

    textOutFile.close()

    // save the model itself
    save(lda, config.getString("lda.modelDir") + s"""/lda_${params.numTopics}t_${params.numIterations}i.model""")
  }
}
