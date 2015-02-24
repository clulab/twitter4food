package edu.arizona.sista.twitter4food

import scala.util.matching.Regex

/**
 * Created by dfried on 5/14/14.
 */
class Preprocessor(val regexPattern: Option[Regex] = None, val stopWords: Option[Set[String]] = None) {

  def processTokens(tokens: Seq[String]) : Seq[String] = {
    var toks = tokens
    regexPattern.foreach(r => toks = Utils.filterByRegex(r)(toks))
    toks = toks.map(_.toLowerCase)
    stopWords.foreach(sw => toks = toks.filterNot(sw.contains(_)))
    toks
  }
}

