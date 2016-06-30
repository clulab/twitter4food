package org.clulab.twitter4food.lda

import java.io.{File, PrintWriter, Serializable}
import java.util.ArrayList

import scala.collection.JavaConversions.asScalaBuffer

import cc.mallet.pipe.{Pipe, SerialPipes, TokenSequence2FeatureSequence}
import cc.mallet.topics.{ParallelTopicModel, TopicModelDiagnostics}
import cc.mallet.types._
import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.util.{FileUtils, Tokenizer}
import org.clulab.twitter4food.util.FileUtils._
import org.clulab.twitter4food.util.Filter._
import org.clulab.twitter4food.struct._
import org.clulab.utils.Serializer
import org.slf4j.LoggerFactory

import scala.io.Source

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

  val logger = LoggerFactory.getLogger(this.getClass)

  val config = ConfigFactory.load
  val stopWords: Set[String] = try {
    (Source.fromFile(config.getString("lda.stopWords")).getLines map(_.stripLineEnd)).toSet
  } catch {
    case _: Throwable => println("Stopword file not found!")
      Set.empty[String]
  }

  case class Config(numTopics:Int = 200, numIterations:Int = 100)

  def stripHashtag(token: String) = {
    if (token.startsWith("#")) token.substring(1, token.length) else token
  }

  def mkInstance(tokens: Seq[String], tokenFilter: (Seq[String] => Seq[String]) = filterStopWords): Instance = {
    val t = tokenFilter(tokens map stripHashtag)
    new Instance(new TokenSequence(t.map(new Token(_)).toArray), null, null, null)
  }

  def filterStopWords(tokens: Seq[String]): Seq[String] = tokens filterNot stopWords.contains

  def train(tokensList: Iterable[Seq[String]], numTopics: Int = 200, numIterations: Int = 2000): (LDA, Alphabet) = {
    // Begin by importing documents from text to feature sequences
    val pipeList = new ArrayList[Pipe]

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

    // Use two parallel samplers, which each look at one half the corpus and combine
    //  statistics after every iteration.
    model.setNumThreads(2)

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
    val fe = new FeatureExtractor

    logger.info(s"Loading and filtering tweets...")

    val tweetsRaw = {
      val twoLines = for {
        df <- asScalaBuffer(config.getStringList("lda.2lineTrainingData")).toList
      } yield {
        val d = FileUtils.load(df)
        val englishOnly = d.keys.filter(_.lang == "en")
        englishOnly.flatMap(_.tweets).map(_.text)
      }

      val threeLines = for {
        df <- asScalaBuffer(config.getStringList("lda.3lineTrainingData")).toList
      } yield FileUtils.loadSingletonTexts(df)

      twoLines.flatten ++ threeLines.flatten
    }

    val pb = new me.tongfei.progressbar.ProgressBar("LDA", 100)
    pb.start()
    pb.maxHint(tweetsRaw.length)
    pb.setExtraMessage("Parsing and filtering...")

    val tweets = tweetsRaw
      .par
      .map { tweet =>
        pb.step()
        fe.filterTags(Tokenizer.annotate(tweet.toLowerCase))
          .map(_.token)
          .toSeq
      }.seq

    pb.stop()

    logger.info(s"Accounts: ${tweets.size}, Tweets: ${tweets.flatten.size}")

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
