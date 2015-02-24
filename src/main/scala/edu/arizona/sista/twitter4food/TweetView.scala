package edu.arizona.sista.twitter4food

import collection.JavaConversions._

/**
 * Created by dfried on 5/21/14.
 */
object TweetView {
  def view(annotators: Seq[Annotator])(tokenType: TokenType)(tweet: Tweet) = {
    val preprocessed = TwitterLocationPreprocessor.processTokens(tweet.tokens)
    val tokens = preprocessed.filter(tokenType.okToken)
    val annotations = annotators.map(_.annotate(tweet, preprocessed)).flatten
    tokens ++ annotations
  }
}
