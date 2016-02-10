package org.clulab.twitter4food.struct

/**
  * Stores information about an individual tweet
  * User: mihais
  * Date: 12/14/15
  */
class Tweet (val tweetText: String,
             val tweetId: Long,
             val tweetLang: String,
             val user: String
             ) {
  // TODO: add time!

  val text: String = tweetText
  val id: Long = tweetId
  val lang: String = tweetLang
  val userHandle: String = user

  def getText = text

  def getId = id

  def getLang = lang

  def getUserHandle = userHandle

}
