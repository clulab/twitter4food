package org.clulab.twitter4food.twitter4j

import cmu.arktweetnlp._
import scala.collection.JavaConverters._

object Tokenizer{
  class Sentence(token: String, tag: String)

  val modelFileName = "/cmu/arktweetnlp/model.20120919"
  val tagger = new Tagger()
  tagger.loadModel(modelFileName)

  /* Returns List[TaggedToken] which has members tag, token */
  def annotate(text: String) = tagger.tokenizeAndTag(text).asScala
}