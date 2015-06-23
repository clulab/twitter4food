package edu.arizona.sista.twitter4food

import edu.stanford.nlp.kbp.slotfilling.classify.{JointBayesRelationExtractor, MultiLabelDataset}
import edu.stanford.nlp.ling.{RVFDatum => S_RVFDatum, Datum => S_Datum}
import edu.stanford.nlp.stats
import edu.stanford.nlp.stats.{Counter => S_Counter}
import scala.collection.JavaConverters._
import edu.arizona.sista.struct.Counter

/**
 * Created by dfried on 5/28/15.
 */
trait FeatureModel
case object AtLeastOnce extends FeatureModel
case object LabelDependencies extends FeatureModel

trait LocalDataFilter
case object AllFilter extends LocalDataFilter
case object SingleFilter extends LocalDataFilter
case object RedundancyFilter extends LocalDataFilter
case class LargeFilter(k: Int) extends LocalDataFilter

trait InferenceType
case object Stable extends InferenceType
case object Slow extends InferenceType

case class MIML[L, F](group: Seq[Set[F]], labels: Set[L])

class MIMLWrapper(initialModelPath: String, numberOfTrainEpochs: Int = 6, numberOfFolds: Int = 5, localFilter: LocalDataFilter = AllFilter, featureModel: FeatureModel = AtLeastOnce, inferenceType: InferenceType = Stable, trainY: Boolean = true, onlyLocalTraining: Boolean = false) {

  val jbre = new JointBayesRelationExtractor(
    initialModelPath,
    numberOfTrainEpochs,
    numberOfFolds,
    localFilter match {
      case AllFilter => "all"
      case SingleFilter => "single"
      case RedundancyFilter => "redundancy"
      case LargeFilter(k) => s"large$k"
    },
    featureModel match { case AtLeastOnce => 0; case LabelDependencies => 1},
    inferenceType match {
      case Stable => "stable"
      case Slow => "slow"
    },
    trainY,
    onlyLocalTraining)

  def makeMultiLabelDataset[L, F](groups: Seq[MIML[L, F]]) = {
    val mld = new MultiLabelDataset[L, F]
    for (labelledGroup <- groups) {
      mld.add(labelledGroup.labels.asJava, Set[L]().asJava, labelledGroup.group.map(_.asJava.asInstanceOf[java.util.Collection[F]]).asJava)
    }
    mld
  }

  def train(groups: Seq[MIML[String, String]]) = {
    jbre.train(makeMultiLabelDataset(groups))
  }

  def classifyGroup(group: Seq[Set[String]]): Seq[(String, Double)] = {
    val counter = jbre.classifyMentions(group.map(set => set.map(string => string.asInstanceOf[java.lang.String]).asJava.asInstanceOf[java.util.Collection[java.lang.String]]).asJava)
    MIMLWrapper.sortCounterDescending(counter)
  }

  def classifyIndividual(individualFeatures: Set[String]) = {
    val counter = jbre.classifyLocally(individualFeatures.asJava.asInstanceOf[java.util.Collection[java.lang.String]])
    MIMLWrapper.sortCounterDescending(counter)
  }
}

object MIMLWrapper {
  def sortCounterDescending(counter: S_Counter[String]) = (for {
      f <- counter.keySet().asScala.toSeq
  } yield f -> counter.getCount(f)).sortBy(-_._2)
}
