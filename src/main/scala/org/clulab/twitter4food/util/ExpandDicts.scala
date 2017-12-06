package org.clulab.twitter4food.util

import java.nio.charset.CodingErrorAction
import scala.io.{Source, Codec}

import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import org.clulab.struct.Lexicon


case class ExpandDictsConfig(dictName: String = "", expandBy: Int = 0, fromFreqRank: Int = 0, limitPerWord: Int = 10)

/**
  * Expand an existing dictionary by finding the _N_ nearest neighbors according to word2vec vectors.
  */
object ExpandDicts extends App {

  def parseArgs(args: Array[String]): ExpandDictsConfig = {
    val parser = new scopt.OptionParser[ExpandDictsConfig]("expandDicts") {
      arg[String]("dictionaryName")action { (x, c) =>
        c.copy(dictName = x)} text "dictionary to expand"
      arg[Int]("expandBy")action { (x, c) =>
        c.copy(expandBy = x)} text "how many words to add"
      opt[Int]('k', "mostFrequentK")action { (x, c) =>
        c.copy(fromFreqRank = x)} text "choose from the top k most frequent words"
      opt[Int]('l', "limit")action { (x, c) =>
        c.copy(limitPerWord = x)} text "an existing word can only expand to the l closest"
    }

    val arguments = parser.parse(args, ExpandDictsConfig())

    if(arguments.isEmpty) throw new IllegalArgumentException(s"args ${args.mkString(" ")} are not supported!")

    arguments.get
  }

  def cosSim(x: Seq[Double], y: Seq[Double]): Double = {
    val dotProduct = x.zip(y).map{ case(a, b) => a * b }.sum
    val magnitudeX = math.sqrt(x.map(i => i * i).sum)
    val magnitudeY = math.sqrt(y.map(i => i * i).sum)
    dotProduct / magnitudeX * magnitudeY
  }

  implicit val codec = Codec("UTF-8")
  codec.onMalformedInput(CodingErrorAction.REPLACE)
  codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

  val logger = LoggerFactory.getLogger(this.getClass)
  val config = ConfigFactory.load
  val arguments = parseArgs(args)

  logger.info(s"finding ${arguments.expandBy} new words for ${arguments.dictName} from top ${arguments.fromFreqRank} words")

  val lastDictLoc = config.getString(s"classifiers.features.lexicons.${arguments.dictName}")
  val expandedDictLoc = config.getString(s"classifiers.features.expanded_lexicons.${arguments.dictName}")

  val dictionary = Lexicon.loadFrom[String](lastDictLoc)

  logger.info(s"${arguments.dictName} contains ${dictionary.size} words")

  val vectorLoc = config.getString("classifiers.features.food_vectors")
  val lines = Source.fromFile(vectorLoc).getLines.toSeq.tail // skip first meta-info line

  val freqCutoff = if(arguments.fromFreqRank > 0) arguments.fromFreqRank else lines.length

  val vectorMap = (for (line <- lines.take(freqCutoff)) yield {
    val splits = line.split(" ")
    splits.head -> splits.tail.map(_.toDouble)
  }).toMap

  val (starting, candidates) = vectorMap.partition{ case (k, v) => dictionary.contains(k) }

  val pb = new me.tongfei.progressbar.ProgressBar("findingNearest", 100)
  pb.start()
  pb.maxHint(candidates.size)

  val nearestDist = for ((startWord, startVec) <- starting.par) yield {
    val distances = for ((candWord, candVec) <- candidates) yield candWord -> cosSim(candVec, startVec)
    pb.step()
    startWord -> distances.toSeq.sortBy(_._2).takeRight(arguments.limitPerWord)
  }
  pb.stop()

  // flatten representation to ease sorting (existing word, candidate word, distance)
  val sortable = nearestDist.seq.flatMap{ case (source, targets) =>
    targets.map(target => (source, target._1, target._2))
  }

  // we only want a candidate to appear once, even if it's close to more than one existing word
  val oneCandInstance = sortable
    .groupBy(_._2)
    .map{ case (candidate, proposals) =>
      proposals.maxBy(_._3)
    }
  
  val wordsToAdd = oneCandInstance.toSeq.sortBy(_._3).takeRight(arguments.expandBy)

  println("\nwordToAdd\texisting\tdistance")
  println("=====================================")
  wordsToAdd.foreach{ case(wordToAdd, closestExisting, dist) =>
    println(f"$wordToAdd\t$closestExisting\t$dist%1.5f")
  }

  wordsToAdd.foreach{ case (wta, _, _) => dictionary.add(wta) }

  dictionary.saveTo(expandedDictLoc)
}