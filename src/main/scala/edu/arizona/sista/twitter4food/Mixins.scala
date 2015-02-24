package edu.arizona.sista.twitter4food

import edu.arizona.sista.struct._
import scala.util.matching.Regex
import scala.collection.Set
import java.text.DecimalFormat

/**
 * Created by dfried on 2/2/14.
 */
object Mixins {
  implicit class CounterMixins[A](val counter: Counter[A])  {


    /*
    could implement + as zipWith(true)(_+_)
     */


  }

  val NoneRepresentation = "NIL"

  // http://stackoverfow.com/questions/3021813/how-to-check-whether-a-string-fully-matches-a-regex-in-scala
  implicit class RegexMixins(val regex: Regex) {
    def matches(s: String): Boolean = regex.pattern.matcher(s).matches
  }

  // http://stackoverflow.com/questions/4636610/regular-expression-and-pattern-matching-in-scala
  implicit class RegexInterpolation(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  implicit class OptionMixins[A](val o: Option[A]) {
    def toShortString = o.map(_.toString).getOrElse(NoneRepresentation)

    def functor[B](fn: A => (B => B)) = o.map(fn).getOrElse(identity _)
  }

  // adapted from https://coderwall.com/p/lcxjzw
  implicit class StringMixins(val s: String) {
    import scala.util.control.Exception._
    // delay param execution so we can catch the exception
    def optionParse[A](method: => A) = {
      // this is probably terrible
      catching(classOf[Exception]) opt method
    }

    def toFloatOpt = optionParse(s.toFloat)

    def toIntOpt = optionParse(s.toInt)

    def toStringOpt = if (s == NoneRepresentation || s == null) None else Some(s)
  }

  implicit class MapMixins[K, V](val map: Map[K, V]) {
    def zipWith[R, V2](f: (V, V2) => R)(other: Map[K, V2]): Map[K, R] = for {
      (k, v) <- map
      v2 <- other.get(k)
      r = f(v, v2)
    } yield (k -> r)
  }

}

