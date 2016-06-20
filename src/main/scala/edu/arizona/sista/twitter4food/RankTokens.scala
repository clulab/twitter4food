package edu.arizona.sista.twitter4food

import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.struct.Counter
import scala.collection.mutable
import org.clulab.processors.Document

/**
 * Ranks tokens such as hashtags based on their frequency and/or PMI with food tweets
 * User: mihais
 * Date: 10/10/13
 */
class RankTokens {
  val processor = new CoreNLPProcessor()

  def isHashtag(t:String):Boolean = t.startsWith("#") && t.length > 1

  def mostCommonHashtags(file:String, excludes:Set[String]):List[(String, Double)] = {
    var count = 0
    val counter = new Counter[String]
    for (line <- Utils.loadFile(file).getLines) {
      count += 1
      if(count % 3 == 0) {
        //println(line)
        val doc = processor.mkDocument(line)
        doc.clear()

        for(sent <- doc.sentences) {
          for(word <- sent.words) {
            //print(" " + word)
            val w = word.toLowerCase()
            if(isHashtag(w) && ! excludes.contains(w)) {
              counter.incrementCount(w)
            }
          }
        }
        // println()
      }
    }
    println(s"Processed ${count/3} tweets.")

    val sortedTags = counter.sorted
    count = 0
    for(t <- sortedTags if count < 100) {
      println(t)
      count += 1
    }
    sortedTags
  }

  def mostCorrelatedHashtags(posFile:String, negFile:String, excludes:Set[String]):List[(String, Double)] = {
    val A = new Counter[String]
    val B = new Counter[String]
    var totalPos = 0
    var totalNeg = 0

    // traverse positive file
    var count = 0
    for (line <- Utils.loadFile(posFile).getLines) {
      count += 1
      if(count % 3 == 0) {
        totalPos += 1
        val doc = processor.mkDocument(line)
        doc.clear()
        val tags = toHashtagSet(doc, excludes)
        tags.foreach(tag => {
          A.incrementCount(tag)
        })
      }
    }

    // traverse negative file
    count = 0
    for (line <- Utils.loadFile(negFile).getLines) {
      count += 1
      if(count % 3 == 0) {
        totalNeg += 1
        val doc = processor.mkDocument(line)
        doc.clear()
        val tags = toHashtagSet(doc, excludes)
        tags.foreach(tag => {
          B.incrementCount(tag)
        })
      }
    }

    val pmis = new Counter[String]()
    for(t <- A.keySet) {
      val a = A.getCount(t)
      val b = B.getCount(t)
      val c = totalPos - a
      val n = totalNeg + totalPos
      val pmi = math.log((a * n) / ((a + c) * (a + b)) )
      pmis.setCount(t, pmi * math.log(a))
    }

    val sortedTags = pmis.sorted
    count = 0
    for(t <- sortedTags if count < 100) {
      println(t)
      count += 1
    }
    sortedTags
  }

  def toHashtagSet(doc:Document, excludes:Set[String]):Set[String] = {
    val set = new mutable.HashSet[String]()
    for(sent <- doc.sentences) {
      for(word <- sent.words) {
        val w = word.toLowerCase()
        if(isHashtag(w) && ! excludes.contains(w)) {
          set += w
        }
      }
    }
    set.toSet
  }
}

object RankTokens {
  def main(args:Array[String]) {
    val ranker = new RankTokens
    val excludes = new mutable.HashSet[String]
    excludes += "#dinner"
    excludes += "#lunch"
    excludes += "#breakfast"
    ranker.mostCommonHashtags(args(0), excludes.toSet)
    ranker.mostCorrelatedHashtags(args(0), args(1), excludes.toSet)
  }
}
