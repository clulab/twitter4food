package edu.arizona.sista.twitter4food

import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.sentiment.SentimentCoreAnnotations
import java.util.Properties
import edu.stanford.nlp.ling.CoreAnnotations
import scala.collection.JavaConversions._
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.trees.Tree
// import edu.stanford.nlp.rnn._

import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.Sentence
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation

/**
 * Created by dfried on 1/21/14.
 */

//
// ms: disabled for now. needs to be updated to the latest processors
//

/*
class SentimentProcessor {

  val properties =  new Properties()

  properties.setProperty("annotators", "tokenize, ssplit, parse, sentiment")

  val pipeline =  new StanfordCoreNLP(properties)

  val proc = new CoreNLPProcessor

  def annotate(text: String): CoreMap = pipeline.process(text)

  def sentences(annotation: CoreMap): Seq[Sentence] = {
    val sas = annotation.get(classOf[SentencesAnnotation])
    sas map proc.mkSentence
  }

  def sentences(text: String): Seq[Sentence] = sentences(annotate(text))

  def sentimentTrees(annotation: CoreMap): Seq[Tree] = for {
    sentence: CoreMap <- annotation.get(classOf[CoreAnnotations.SentencesAnnotation])
  } yield sentence.get(classOf[SentimentCoreAnnotations.AnnotatedTree])

  def sentimentTrees(text: String): Seq[Tree] = sentimentTrees(annotate(text))

  def sentimentClasses(trees: Seq[Tree]): Seq[Int] =
    sys.error("todo: import version of CoreNLP with sentiment")
    // trees map RNNCoreAnnotations.getPredictedClass

  def sentimentClasses(annotation: CoreMap): Seq[Int] =
    sentimentClasses(sentimentTrees(annotation))

  def sentimentClasses(text: String): Seq[Int] =
    sentimentClasses(annotate(text))

  def sentencesAndSentiment(text: String): (Seq[Sentence], Seq[Int]) = {
    val annotation = annotate(text)
    (sentences(annotation), sentimentClasses(annotation))
  }

  def meanSentimentScore(text: String): Float = {
    val scores = sentimentClasses(text)
    scores.sum.toFloat / scores.length
  }

}

object SentimentProcessor {
  def main(args: Array[String]) = {
    val sentiment = new SentimentProcessor
    val meanScores = args map sentiment.meanSentimentScore
    println((args zip meanScores).toList)
  }
}
*/
