package edu.arizona.sista.twitter4food

import edu.arizona.sista.learning._
import edu.arizona.sista.struct._
import Utils._
import java.io._
import java.util.Properties
import scala.collection.{immutable, mutable, GenSeq}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util._
import StateExperiment._
import Mixins._
import de.bwaldvogel.liblinear.SolverType
import scala.io.Source
import collection.JavaConversions._
import edu.arizona.sista.utils.EvaluationStatistics

/**
 * Created by dfried on 5/6/15.
 */
class IndividualsExperiment(parameters: ExperimentParameters, printWriter: PrintWriter = new java.io.PrintWriter(System.out))
  extends Experiment(parameters = parameters, printWriter = printWriter) {

  def run(individualsCorpus: IndividualsCorpus, testingCorpus: Option[Seq[(Seq[Tweet], Int)]]) = {
    // maps from state abbreviations to integer labels
    val stateLabels = Experiment.makeLabels(Datasets.overweight, parameters.numClasses, parameters.removeMarginals)

    // take a mapping from state abbvs to a dictionary of userName -> tweets
    // return two lists: the user tweets, and the labels for those tweets (assuming each user has his/her state's label)
    def propLabels(tweetsByUserByState: Map[String, Map[String, Seq[Tweet]]]): (Seq[Seq[Tweet]], Seq[Int]) = (for {
      (state, tweetsByUser) <- tweetsByUserByState.toSeq
      tweet <- tweetsByUser.values
    } yield (tweet, stateLabels(state))).unzip

    val ((trainingTweets, trainingLabels), (testingTweets, testingLabels)) = testingCorpus match {
      case None => {
        (propLabels(individualsCorpus.trainingTweets), propLabels(individualsCorpus.testingTweets))
      }
      case Some(testC) => {
        (propLabels(individualsCorpus.allTweets), testC.unzip)
      }

    }

    val trainingViewFeatures = trainingTweets.map(mkViewFeatures(parameters.lexicalParameters.ngramThreshold))
    val trainingRegionalFeatures = trainingTweets.map(mkRegionalFeatures)

    val testingViewFeatures = trainingTweets.map(mkViewFeatures(parameters.lexicalParameters.ngramThreshold))
    val testingRegionalFeatures = trainingTweets.map(mkRegionalFeatures)

    val trainingFeatures = trainingTweets.map(tweets => mkViewFeatures(parameters.lexicalParameters.ngramThreshold)(tweets) + mkRegionalFeatures(tweets))
    val testingFeatures = testingTweets.map(tweets => mkViewFeatures(parameters.lexicalParameters.ngramThreshold)(tweets) + mkRegionalFeatures(tweets))

    val labels = (trainingLabels ++ testingLabels).toSet

    // keep track of the highly weighted features for each label. This will be updated by updateWeights in each fold
    // of the cross validation
    val weights = new mutable.HashMap[Int, Counter[String]]()
    for (label <- labels) weights(label) = new Counter[String]

    def updateWeights(clf: Classifier[Int, String]) = {
      val marginalWeights: Option[Map[Int, Counter[String]]] = clf match {
        case clf:LiblinearClassifier[Int, String] => Some(clf.getWeights(verbose = false))
        //case clf:BaggingClassifier[L, String] => clf.getWeights
        case _ => None
      }
      if (marginalWeights != None) {
        for ((l, w) <- marginalWeights.get) {
          println(w.sorted.take(10) mkString(" "))
          // normalize the weights and add them into the total
          weights(l) = weights(l) + w / Math.sqrt(w.dotProduct(w))
        }
      }
    }

    val (clf, procFeats, featureSelector) = trainFromFeatures(trainingFeatures, trainingLabels)

    val predictedLabels: Seq[Int] = for {
      testF <- testingFeatures
      // make a datum for the individual (label will not be used!)
      testingDatum = mkDatum(0, procFeats(testF))
      _ = { // this funky notation just allows us to do side effects in the for comprehension,
        // specifically updating the feature weights
        println(clf.toString)
        if (featureSelector != None)
          println(featureSelector.get.featureScores.toSeq.sortBy(_._2).reverse.take(20))
        if (parameters.classifierType == SVM_L1 || parameters.classifierType == SVM_L2) {
          // get the feature weights for this state for the true class
          updateWeights(clf)
        }
      }
      prediction = clf.classOf(testingDatum)
    } yield prediction

    (testingLabels, predictedLabels, weights)
  }
}
