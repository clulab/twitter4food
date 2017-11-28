package org.clulab.twitter4food.struct

/**
  * Stores information about an individual tweet
  * User: mihais
  * Date: 12/14/15
  */
class Tweet (val text: String,
             val id: Long,
             val lang: String,
             val createdAt: java.util.Date,
             val handle: String) { // TODO: Add image locations

  override def toString = s"$handle: $text [$createdAt]"

  /**
    * Returns a copy of the tweet, with optionally altered arguments
    */
  def copy(
    text: String = this.text,
    id: Long = this.id,
    lang: String = this.lang,
    createdAt: java.util.Date = this.createdAt,
    handle: String = this.handle): Tweet = {
    new Tweet(text, id, lang, createdAt, handle)
  }

//  /**
//    * Returns a new [[Tweet]] with the images of both copies of the tweet. The argument tweet's other info is discarded.
//    */
//  def merge(that: Tweet): Tweet = {
//    assert(this.id == that.id, "Merged tweets must have the same ID!")
//    val allImages = (this.images ++ that.images).distinct
//    this.copy(images = allImages)
//  }

  /**
    * Returns true if the tweet is a retweet. Assumes pre-tokenized text
    */
  def isRetweet: Boolean = this.text.startsWith("RT ")

  /**
    * Returns true if the tweet is addressed using (at least one) @mention. Assumes pre-tokenized text
    */
  def isAddressed: Boolean = this.text.startsWith("<@MENTION>")

  /**
    * Returns true if the tweet is "normal", i.e. not a retweet or addressed to other accounts
    */
  def isNormal: Boolean = !this.isAddressed && !this.isRetweet

  def canEqual(that: Any): Boolean = that.isInstanceOf[Tweet]

  override def equals(that: Any): Boolean = {
    that match {
      case that: Tweet => this.canEqual(that) && this.id == that.id // TODO: check if images are the same
      case _ => false
    }
  }
}
