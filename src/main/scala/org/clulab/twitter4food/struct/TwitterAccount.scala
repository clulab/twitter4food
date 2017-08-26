package org.clulab.twitter4food.struct

/**
  * Stores information about a single Twitter account, including handle, description, tweets, network
  * User: mihais
  * Date: 12/14/15
  */
class TwitterAccount (
  // TODO: add network information

  val handle: String,
  val id: Long,
  val name: String,
  val lang: String,
  val url: String,
  val location: String,
  val description: String,
  val tweets: Seq[Tweet],
  val activeFollowers: Seq[TwitterAccount]) {

  override def toString = s"$handle: ($name, $description)"

  def sanitize(s: String) = s.replaceAll("(\\\\)*\"", "\\\\\"")
  val space = "  " //space

  def toJson(indents: Int = 0, sp: String = space, label: Option[String] = None) = {
    val labelLine = if (label.isEmpty)
      ""
    else
      s"""${sp * (indents + 1)}"label": "${label.get}",\n"""

    s"""${sp * indents}{
       |$labelLine${sp * (indents + 1)}"id": $id,
       |${sp * (indents + 1)}"handle": "$handle",
       |${sp * (indents + 1)}"name": "${sanitize(name)}",
       |${sp * (indents + 1)}"lang": "$lang",
       |${sp * (indents + 1)}"url": "${sanitize(url)}",
       |${sp * (indents + 1)}"location": "${sanitize(location)}",
       |${sp * (indents + 1)}"description": "${sanitize(description)}",
       |${sp * (indents + 1)}"tweets": [
       |${tweets.map{t => t.toJson(indents + 2, sp)}.mkString(",\n")}
       |${sp * (indents + 1)}]
       |}""".stripMargin
  }


  /**
    * Returns a copy of this [[TwitterAccount]], optionally specifying new input values
    */
  def copy(
    handle: String = this.handle,
    id: Long = this.id,
    name: String = this.name,
    lang: String = this.lang,
    url: String = this.url,
    location: String = this.location,
    description: String = this.description,
    tweets: Seq[Tweet] = this.tweets,
    activeFollowers: Seq[TwitterAccount] = this.activeFollowers): TwitterAccount = {
    new TwitterAccount(handle, id, name, lang, url, location, description, tweets, activeFollowers)
  }

  /**
    * Returns a merged [[TwitterAccount]] with all the tweets of both input accounts.
    */
  def merge(that: TwitterAccount): TwitterAccount = {
    assert(this.id == that.id, "They must be instantiations of the same account!")
    this.copy(tweets = (this.tweets.toSet ++ that.tweets.toSet).toSeq.sortBy(_.createdAt))
  }

  /**
    * Returns only those tweets that are "normal", i.e. neither retweets nor addressed to other accounts
    */
  def normalTweets: Seq[Tweet] = this.tweets.filter(_.isNormal)
}
