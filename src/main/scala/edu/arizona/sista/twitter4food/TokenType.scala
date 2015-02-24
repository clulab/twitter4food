package edu.arizona.sista.twitter4food

trait TokenType {
  def okToken(token: String): Boolean
}

case object AllTokens extends TokenType {
  def okToken(token: String): Boolean =
    true

  override def toString = "AllTokens"
}

case object HashtagTokens extends TokenType {
  def okToken(token: String): Boolean =
    token.length > 0 && token.charAt(0) == '#'

  override def toString = "HashtagTokens"
}

case object FoodTokens extends TokenType {
  def okToken(token: String): Boolean =
    if (HashtagTokens.okToken(token))
      FoodWords.words.contains(token.substring(1))
    else
      FoodWords.words.contains(token)

  override def toString = "FoodTokens"
}

/**
 * Union of HashtagTokens and FoodTokens
 */
case object FoodHashtagTokens extends TokenType {
  def okToken(token: String): Boolean =
    HashtagTokens.okToken(token) || FoodTokens.okToken(token)

  override def toString = "FoodHashtagTokens"
}

