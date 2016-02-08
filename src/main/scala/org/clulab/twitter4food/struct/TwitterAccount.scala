package org.clulab.twitter4food.struct

/**
  * Stores information about a single Twitter account, including handle, description, tweets, network
  * User: mihais
  * Date: 12/14/15
  */
class TwitterAccount {
  // TODO: add network information
  // TODO: Currently only stores top 100 tweets
  // TODO: Filter RTs and replies

  var handle: String = null
  var id: Long = 0
  var name: String = null
  var lang: String = null
  var url: String = null
  var location: String = null
  var description: String = null
  var tweets: Seq[Tweet] = null

  def getHandle = { this.handle }

  def setHandle(handle: String) = { this.handle = handle; this }

  def getId = { this.id }

  def setId(id: Long) = { this.id = id; this }

  def getName = { this.name }

  def setName(name: String) = { this.name = name; this }

  def getLang = { this.lang }

  def setLang(lang: String) = { this.lang = lang; this }

  def getUrl = { this.url }

  def setUrl(url: String) = { this.url = url; this }

  def getLocation = { this.location }

  def setLocation(location: String) = { this.location = location; this }

  def getDescription = { this.description }

  def setDescription(description: String) = {
    this.description = description; this
  }

  def getTweets = { this.tweets }

  def setTweets(tweets: Seq[Tweet]) = { this.tweets = tweets; this }
}
