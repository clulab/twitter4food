package org.clulab.twitter4food.util

import cmu.arktweetnlp._
import cmu.arktweetnlp.Tagger._
import scala.collection.JavaConverters._

object Tokenizer{
  val modelFileName = "/cmu/arktweetnlp/model.20120919"
  val tagger = new Tagger()
  tagger.loadModel(modelFileName)

  /* Returns Array[TaggedToken] which has members tag, token */
  def annotate(text: String): Array[TaggedToken] = {
    try {
      tagger.tokenizeAndTag(text).asScala.toArray
    } catch {
      case e: Exception => {
        //print(s"Exception caused at: ${text}"); 
        Array[TaggedToken]()
      }
    }
  }
}