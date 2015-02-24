package edu.arizona.sista.twitter4food

trait Annotator {
  /**
   * produce additional String features for the given tweet
   * @param tweet
   * @return
   */
  def annotate(tweet: Tweet, tokens: Seq[String]): Seq[String]
}

/*
object LDAAnnotator extends Annotator {
  def annotate(tweet: Tweet, tokens: Seq[String]) =
  if (tweet.topic != null)
    List("TOPIC_" + tweet.topic)
  else
    List()
}
*/

case class LDAAnnotator(val tokenType: TokenType) extends Annotator {
  def annotate(tweet: Tweet, tokens: Seq[String]) = {
    val topic = tokenType match {
      case AllTokens => tweet.allTokensTopic
      case HashtagTokens => tweet.hashtagTokensTopic
      case FoodTokens => tweet.foodTokensTopic
      case FoodHashtagTokens => tweet.foodHashtagTokensTopic
    }
    List("TOPIC_" + topic)
  }

  override def toString = "LDA"
}

object SentimentAnnotator extends Annotator {
  def annotate(tweet: Tweet, tokens: Seq[String]) = {
    tweet.sentiment match {
      case 0 => List()
      case 1 => tokens map (_ + "_+")
      case -1 => tokens map (_ + "_-")
    }
  }

  override def toString = "Sentiment"
}

class NGramAnnotator(val n: Int) extends Annotator {
  def annotate(tweet: Tweet, tokens: Seq[String]) =
    Ngrams.ngramsStrings(tokens, n)
}

object BigramAnnotator extends NGramAnnotator(2)

