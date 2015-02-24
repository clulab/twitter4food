package edu.arizona.sista.twitter4food

/**
 * Created by dfried on 5/21/14.
 */
object TwitterLocationPreprocessor extends Preprocessor(Some(Tweet.tokenPattern),
  Some(Tweet.stopWords ++ LocationStopwords.stopWords))
