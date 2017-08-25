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
             val handle: String,
             val images: Seq[String] = Nil) {

  override def toString = s"$handle: $text [$createdAt]"

  def sanitize(s: String) = s.replaceAll("(\\\\)*\"", "\\\\\"")
  val space = "  "

  def toJson(indents: Int = 0, sp: String = space): String = {
    s"""${sp * indents}{
       |${sp * (indents + 1)}"id": $id,
       |${sp * (indents + 1)}"lang": "$lang",
       |${sp * (indents + 1)}"created_at": "${createdAt.toString}",
       |${sp * (indents + 1)}"text": "${sanitize(text)}",
       |${sp * (indents + 1)}"images": [
       |${sp * (indents + 1)}]
       |${sp * indents}}
     """.stripMargin
  }

  def copy(
    text: String = this.text,
    id: Long = this.id,
    lang: String = this.lang,
    createdAt: java.util.Date = this.createdAt,
    handle: String = this.handle,
    images: Seq[String] = this.images): Tweet = new Tweet(text, id, lang, createdAt, handle, images)

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
}
