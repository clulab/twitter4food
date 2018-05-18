package org.clulab.twitter4food.util

import java.nio.charset.CodingErrorAction

import scala.io.{Codec, Source}
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import org.clulab.struct.{Counter, Lexicon}

case class ExpandDictsConfig(
                              dictName: String = "",
                              expandBy: Int = 0,
                              highestFreq: Int = 0,
                              lowestFreq: Int = 0,
                              minIdf:Double = 0.0,
                              maxIdf:Double = 0.0,
                              limitPerWord: Int = 10
                            )

/**
  * Expand an existing dictionary by finding the _n_ nearest neighbors according to word2vec vectors.
  * e.g., runMain org.clulab.twitter4food.util.ExpandDicts food_words_less 500 -h 100 -l 15000 -n 5
  */
object ExpandDicts extends App {

  def parseArgs(args: Array[String]): ExpandDictsConfig = {
    val parser = new scopt.OptionParser[ExpandDictsConfig]("expandDicts") {
      arg[String]("dictionaryName") action { (x, c) =>
        c.copy(dictName = x)} text "dictionary to expand"
      arg[Int]("expandBy") action { (x, c) =>
        c.copy(expandBy = x)} text "how many words to add"
      opt[Int]('h', "highestFreq") action { (x, c) =>
        c.copy(highestFreq = x)} text "ignore the top h most frequent words"
      opt[Int]('l', "lowestFreq") action { (x, c) =>
        c.copy(lowestFreq = x)} text "ignore words less frequent than word l"
      opt[Double]('i', "minimumIdf") action { (x, c) =>
        c.copy(minIdf = x)} text "ignore words with log idf less than this"
      opt[Double]('a', "maximumIdf") action { (x, c) =>
        c.copy(maxIdf = x)} text "ignore words with log idf greater than this"
      opt[Int]('n', "neighbors") action { (x, c) =>
        c.copy(limitPerWord = x)} text "an existing word can only expand to the n closest"
    }

    val arguments = parser.parse(args, ExpandDictsConfig())

    if(arguments.isEmpty) throw new IllegalArgumentException(s"args ${args.mkString(" ")} are not supported!")

    arguments.get
  }

  // cosine similarity, i.e. cosine of the angle between two vectors ranging [0,1] with 1 being identical
  def cosSim(x: Seq[Double], y: Seq[Double]): Double = {
    val dotProduct = x.zip(y).map{ case(a, b) => a * b }.sum
    val magnitudeX = math.sqrt(x.map(i => i * i).sum)
    val magnitudeY = math.sqrt(y.map(i => i * i).sum)
    dotProduct / (magnitudeX * magnitudeY)
  }

  def softmax(x: Seq[Double]): Double = 1 - x.map(1.0 - _).product

  // return the log inverse document frequency for each word in a list of documents, each of which is a list of words
  def logidf(x: Seq[Seq[String]]): Counter[String] = {
    val numDocs = x.length
    val appearances = new Counter[String]

    // count the number of documents each word appears in
    for {
      tweet <- x
      wd <- tweet.distinct
    } appearances.incrementCount(wd)

    appearances.mapValues(count => math.log(numDocs / count))
  }

  def isPossibleTerm(term: String): Boolean = {
    val punct = "[…\\p{Punct}&&[^#'-]]".r // keep # for hashtags
    (! stopWords.contains(term)) & punct.findFirstIn(term).isEmpty
  }

  def rightIdf(term: String): Boolean = {
    if (! idfs.contains(term)) return false
    val termIdf = idfs.getCount(term)
    (arguments.minIdf, arguments.maxIdf) match {
      case (n, x) if n > 0 & x > 0 => termIdf >= n & termIdf <= x
      case (_, x) if x > 0 => termIdf <= x
      case (n, _) if n > 0 => termIdf >= n
      case noRestriction => true
    }
  }

  // deal with difficult characters by removing them
  implicit val codec: Codec = Codec("UTF-8")
  codec.onMalformedInput(CodingErrorAction.REPLACE)
  codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

  val logger = LoggerFactory.getLogger(this.getClass)
  val config = ConfigFactory.load
  val arguments = parseArgs(args)

  logger.info(s"finding ${arguments.expandBy} new words for ${arguments.dictName}")
  logger.info(s"min freq rank: ${arguments.highestFreq}; max freq rank: ${arguments.lowestFreq}; " +
    s"total: ${arguments.lowestFreq - arguments.highestFreq}")
  logger.info(s"min log IDF: ${arguments.minIdf}; max log IDF: ${arguments.maxIdf}"

  val lastDictLoc = config.getString(s"lexicons.${arguments.dictName}")
  val expandedDictLoc = config.getString(s"expanded_lexicons.${arguments.dictName}")

  val dictionary = Lexicon.loadFrom[String](lastDictLoc)
  logger.info(s"${arguments.dictName} contains ${dictionary.size} words")

  val stopWords = FileUtils.readFromCsv(config.getString("classifiers.features.stopWords")).flatten
  logger.info(s"${stopWords.length} stop words: ${stopWords.take(5).mkString(", ")}...")

  val corpus = FileUtils.loadTwitterAccounts(config.getString("classifiers.diabetes.data")).keys.toSeq
  val wds = corpus.flatMap(_.tweets.map(_.text.split("\\s+").toSeq))
  val idfs = logidf(wds)

  val vectorLoc = arguments.dictName match {
    case food if food.startsWith("food") => config.getString("classifiers.features.food_vectors")
    case _ => config.getString("classifiers.features.generic_vectors")
  }

  val goldilocksFrequency = if(arguments.lowestFreq == 0) {
    Source
      .fromFile(vectorLoc)
      .getLines
      .drop(arguments.highestFreq + 1) // + 1 because we are skipping first line, which is metadata
  } else {
    Source
      .fromFile(vectorLoc)
      .getLines
      .slice(arguments.highestFreq + 1, arguments.lowestFreq + 1)
  }

  val vectorMap = goldilocksFrequency.flatMap{ line =>
    val splits = line.split(" ")
    // Find right IDF values and filter stop words out here
    if (isPossibleTerm(splits.head))
      Option(splits.head -> splits.tail.map(_.toDouble))
    else
      None
  }

  val (starting, candidates) = vectorMap.toSeq.partition{ case (k, _) => dictionary.contains(k) }
  logger.info(s"${candidates.length} candidates: ${candidates.take(5).map(_._1).mkString(", ")}...")
  val goldilocksIdf = candidates.filter{ case (term, _) => rightIdf(term) }
  logger.info(s"${goldilocksIdf.length} have correct frequency: ${goldilocksIdf.take(5).map(_._1).mkString(", ")}...")

  val pb = new me.tongfei.progressbar.ProgressBar("winnowing", 100)
  pb.start()
  pb.maxHint(starting.length)

  val distances = for {
    (startWord, startVec) <- starting.par
  } yield {
    val allDistances = for ((candWord, candVec) <- goldilocksIdf) yield (candWord, startWord, cosSim(candVec, startVec))
    pb.step()
    allDistances.sortBy(_._3).takeRight(arguments.limitPerWord)
  }
  pb.stop()

  val softmaxes = distances
    .flatten
    .seq
    .groupBy(_._1)
    .par
    .map { case (candWord, dists) =>
      (dists.maxBy(_._3)._2, candWord, softmax(dists.map(_._3)))
    }

  val wordsToAdd = softmaxes.toSeq.seq.sortBy(_._3).takeRight(arguments.expandBy)

  println("\nexisting\twordToAdd\tdistance")
  println("=====================================")
  wordsToAdd.foreach{ case(closestExisting, wordToAdd, dist) =>
    println(f"$closestExisting\t$wordToAdd\t$dist%1.5f")
  }

  wordsToAdd.foreach{ case (_, wta, _) => dictionary.add(wta) }

  logger.info(s"writing to $expandedDictLoc")
  dictionary.saveTo(expandedDictLoc)
}