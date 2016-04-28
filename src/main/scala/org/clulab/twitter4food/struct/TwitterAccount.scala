package org.clulab.twitter4food.struct

/**
  * Stores information about a single Twitter account, including handle, description, tweets, network
  * User: mihais
  * Date: 12/14/15
  */
class TwitterAccount (
  // TODO: add network information
  // TODO: Currently only stores top 100 tweets
  // TODO: Filter RTs and replies

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
}
