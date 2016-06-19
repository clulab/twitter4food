package org.clulab.twitter4food.util

import org.clulab.twitter4food.struct._
import org.clulab.twitter4food.util._
import scala.collection.mutable.ArrayBuffer
import java.util.concurrent.{Executors, ExecutorService}
import java.util.concurrent.{Callable, CyclicBarrier, Future}

/*
 * Call as val accounts = (new MultiThreadLoader(_, _, _, numThreads)).call
 */

class MultiThreadLoader(handles: Seq[String], isAppOnly: Boolean,
  fetchTweets: Boolean, fetchNetwork: Boolean, 
  poolSize: Int) extends Callable[Seq[TwitterAccount]] {
  val pool = Executors.newFixedThreadPool(poolSize)
  def call(): Seq[TwitterAccount] = {
    val futureSet = ArrayBuffer[Future[Seq[TwitterAccount]]]()
    try { 
      for(i <- 0 until poolSize) {
        futureSet += pool.submit(new ThreadRunner(handles, isAppOnly, 
          fetchTweets, fetchNetwork, poolSize))
      }
    } catch {
      case ie: InterruptedException => println("Something got interrupted")
    } finally {
      pool.shutdown()
    }

    futureSet.foldLeft(Array[TwitterAccount]())(
      (accounts, future) => accounts ++ future.get)
  }
}

class ThreadRunner(handles: Seq[String], isAppOnly: Boolean,
  fetchTweets: Boolean, fetchNetwork: Boolean, N: Int) 
  extends Callable[Seq[TwitterAccount]] {
  def call(): Seq[TwitterAccount] = {
    val id = Thread.currentThread.getId.toInt % N
    val (api, config) = TestUtils.init(id)
    val hlMap = handles.foldLeft(Map[String, String]())(
      (map, h) => map + (h -> ""))
    val (subH, subL) = TestUtils.splitHandles(id, N, hlMap)
    val accounts = TestUtils.fetchAccounts(api, subH, fetchTweets, 
      fetchNetwork, isAppOnly)
    accounts
  }
}

object MultiThreadLoader {
  def multiTheadFetch(handles: Seq[String], isAppOnly: Boolean,
  fetchTweets: Boolean, fetchNetwork: Boolean, poolSize: Int) = {
    (new MultiThreadLoader(handles, isAppOnly, fetchTweets, fetchNetwork,
      poolSize)).call
  }

  def main(args: Array[String]) = {
    if(args.length < 1) throw new RuntimeException("Error! Enter classifier type")
    val c = TestUtils.init(16)._2
    val hlMap = TestUtils.loadHandles(
      c.getString(s"classifiers.${args(0)}.annotatedUsersFile"))
    val (handles, labels) = (hlMap.keys.toArray, hlMap.values.toArray)
    val accounts = multiTheadFetch(handles, true, true, false, 16)
    FileUtils.saveToFile(accounts, labels,
      c.getString(s"classifiers.${args(0)}.allTrainData"))
  }
}
