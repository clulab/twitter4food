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
             val handle: String) {
  override def toString = s"$handle: $text [$createdAt]"

  def copy(
    text:String = this.text,
    id:Long = this.id,
    lang:String = this.lang,
    createdAt:java.util.Date = this.createdAt,
    handle:String = this.handle): Tweet = {
    new Tweet(text, id, lang, createdAt, handle)
  }
}
