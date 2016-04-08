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
  override def toString = s"@$handle: $text [$createdAt]"
}
