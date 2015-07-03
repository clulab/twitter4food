package edu.arizona.sista.twitter4food

import java.io._

object Serialization {
  def save[A <: Serializable](model: A, serializedFile: File): Unit = {
    val oos = new ObjectOutputStream(new FileOutputStream(serializedFile))
    oos.writeObject(model)
    oos.close
  }

  def load[A](serializedFile: File): A = {
    val ois = new ObjectInputStream(new FileInputStream (serializedFile))
    val obj = ois.readObject()
    ois.close
    obj.asInstanceOf[A]
  }

  def load[A](serializedFilename: String): A = load(new File(serializedFilename))

  def save[A <: Serializable](model: A, serializedFilename: String): Unit = save(model, new File(serializedFilename))

  def main(args: Array[String]) = {
    val tweets: List[Tweet] = load(new File("/home/dfried/tweets_small.dat"))
    println(tweets(0))
  }
}
