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
}
