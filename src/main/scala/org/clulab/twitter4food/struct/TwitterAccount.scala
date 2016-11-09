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

  /**
    * Returns a copy of this [[TwitterAccount]], optionally specifying new input values
    */
  def copy(
    handle: String = this.handle,
    id:Long = this.id,
    name:String = this.name,
    lang:String = this.lang,
    url:String = this.url,
    location:String = this.location,
    description:String = this.description,
    tweets:Seq[Tweet] = this.tweets,
    activeFollowers:Seq[TwitterAccount] = this.activeFollowers): TwitterAccount = {
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
