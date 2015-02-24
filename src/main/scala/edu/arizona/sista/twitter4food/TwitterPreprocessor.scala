package edu.arizona.sista.twitter4food

/**
 * Created by dfried on 5/14/14.
 */
object TwitterPreprocessor extends Preprocessor(Some(Tweet.tokenPattern), Some(Tweet.stopWords))

