package org.clulab.twitter4food.struct

/**
  * Stores information about a single Twitter account, including handle, description, tweets, network
  * User: mihais
  * Date: 12/14/15
  */
class TwitterAccount (val handle:String,
                      val description:AccountDescription,
                      val tweets:Seq[Tweet]) {
  // TODO: add network information
}
