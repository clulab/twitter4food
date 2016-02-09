package org.clulab.twitter4food.t2dm

import edu.arizona.sista.learning.{RVFDataset, LinearSVMClassifier}
import org.clulab.twitter4food.featureclassifier.FeatureClassifier
import org.clulab.twitter4food.struct.TwitterAccount

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by Terron on 2/9/16.
  */
class T2DMClassifier extends FeatureClassifier {
    override var subClassifier: LinearSVMClassifier[String, String] = _
    override var labels: List[String] = _
    override var trainingLabels: mutable.Map[String, String] = _
    override var trainingSet: ArrayBuffer[TwitterAccount] = _

    override var dataset: RVFDataset[String, String] = _

    /** Assign labels for different features predicted */
    override def assignLabels(users: Seq[TwitterAccount]): Unit = ???

    /** Assign labels to default training set */
    override def assignLabels(): Unit = ???

    /** Training from a set of users */
    override def train(users: Seq[TwitterAccount]): Unit = ???

    /** Use default training set */
    override def train(): Unit = ???

    /** Predicting the given feature (or a distribution of) for a given account */
    override def classify(user: TwitterAccount): String = ???
}
