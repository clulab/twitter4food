package edu.stanford.nlp.kbp.slotfilling.classify;

import java.util.*;

import java.io.File;
import java.io.IOException;

import edu.stanford.nlp.classify.*;
import edu.stanford.nlp.kbp.slotfilling.common.Log;
import edu.stanford.nlp.ling.Datum;
import edu.stanford.nlp.ling.RVFDatum;
import edu.stanford.nlp.stats.ClassicCounter;
import edu.stanford.nlp.stats.Counter;
import edu.stanford.nlp.util.ErasureUtils;
import edu.stanford.nlp.util.HashIndex;
import edu.stanford.nlp.util.Index;
import edu.stanford.nlp.util.Pair;

import java.util.List;

/**
 * Created by df345 on 06/07/15.
 */
public class TwoClassJointBayes extends JointBayesRelationExtractor {
    public String positiveClass;
    public String negativeClass;

    public int positiveIndex;
    public int negativeIndex;

    public LinearClassifier<String, String> yClassifier;

    protected static String POSITIVE_CLASS_FRACTION = "positive_fraction";
    protected static String BIAS_FEAT = "bias";
    protected static List<String> Y_FEATURES_FOR_INITIAL_MODEL;

    static {
        Y_FEATURES_FOR_INITIAL_MODEL = new ArrayList<String>();
        Y_FEATURES_FOR_INITIAL_MODEL.add(POSITIVE_CLASS_FRACTION);
        Y_FEATURES_FOR_INITIAL_MODEL.add(BIAS_FEAT);
    }

    public TwoClassJointBayes(String initialModelPath, int numberOfTrainEpochs, int numberOfFolds, String localFilter, int featureModel, String inferenceType, boolean trainY, boolean onlyLocalTraining, boolean useRVF, double zSigma, double ySigma, String positiveClass, String negativeClass) {
        super(initialModelPath, numberOfTrainEpochs, numberOfFolds, localFilter, featureModel, inferenceType, trainY, onlyLocalTraining, useRVF, zSigma, ySigma);
        this.positiveClass = positiveClass;
        this.negativeClass = negativeClass;
    }

    public TwoClassJointBayes(Properties props, boolean onlyLocal, boolean useRVF, double zSigma, double ySigma, String positiveClass, String negativeClass) {
        super(props, onlyLocal, useRVF, zSigma, ySigma);
        this.positiveClass = positiveClass;
        this.negativeClass = negativeClass;
    }

    public TwoClassJointBayes(Properties props, boolean useRVF, String positiveClass, String negativeClass) {
        this(props, false, useRVF, 1.0, 1.0, positiveClass, negativeClass);
    }

    // @Override
    public void train(MultiLabelDataset<String, String> data) {

        // filter some of the groups
        if(localDataFilter instanceof LargeFilter) {
            List<Datum<String,String>[]> filteredGroups = new ArrayList<Datum<String,String>[]>();
            List<Set<Integer>> filteredPosLabels = new ArrayList<Set<Integer>>();
            List<Set<Integer>> filteredNegLabels = new ArrayList<Set<Integer>>();
            for(int i = 0; i < data.size(); i ++) {
                if(localDataFilter.filterY(data.getDataArray()[i], data.getPositiveLabelsArray()[i])) {
                    filteredGroups.add(data.getDataArray()[i]);
                    filteredPosLabels.add(data.getPositiveLabelsArray()[i]);
                    filteredNegLabels.add(data.getNegativeLabelsArray()[i]);
                }
            }
            data = new MultiLabelDataset<String, String>(
                    filteredGroups.toArray(new Datum[filteredGroups.size()][]),
                    data.featureIndex(), data.labelIndex(),
                    filteredPosLabels.toArray(ErasureUtils.<Set<Integer> []>uncheckedCast(new Set[filteredPosLabels.size()])),
                    filteredNegLabels.toArray(ErasureUtils.<Set<Integer> []>uncheckedCast(new Set[filteredNegLabels.size()])));
        }

        LinearClassifierFactory<String, String> zFactory =
                new LinearClassifierFactory<String, String>(1e-4, false, zSigma);
        LinearClassifierFactory<String, String> yFactory =
                new LinearClassifierFactory<String, String>(1e-4, false, ySigma);
        zFactory.setVerbose(false);
        yFactory.setVerbose(false);

        if(initialModelPath != null && new File(initialModelPath).exists()) {
            try {
                loadInitialModels(initialModelPath);
                // yClassifiers = initializeYClassifiersWithAtLeastOnce(yLabelIndex);
            } catch (Exception e1) {
                throw new RuntimeException(e1);
            }
        } else {
            featureIndex = data.featureIndex();
            yLabelIndex = data.labelIndex();
            zLabelIndex = new HashIndex<String>(yLabelIndex);
            zLabelIndex.add(JointlyTrainedRelationExtractor.UNRELATED);

            positiveIndex = yLabelIndex.indexOf(positiveClass);
            negativeIndex = yLabelIndex.indexOf(negativeClass);

            // initialize classifiers
            zClassifiers = initializeZClassifierLocally(data, featureIndex, zLabelIndex);
            yClassifier = initializeYStupidly(yLabelIndex);

            if(initialModelPath != null) {
                try {
                    saveInitialModels(initialModelPath);
                } catch (IOException e1) {
                    throw new RuntimeException(e1);
                }
            }
        }

        // stop training after initialization
        // this is essentially a local model!
        if(onlyLocalTraining) return;

        detectDependencyYFeatures(data);

        for(String y: yLabelIndex) {
            int yi = yLabelIndex.indexOf(y);
            Log.severe("YLABELINDEX " + y + " = " + yi);
        }

        // calculate total number of sentences
        int totalSentences = 0;
        for (Datum<String,String>[] group : data.getDataArray())
            totalSentences += group.length;

        // initialize predicted z labels
        int[][] zLabels = initializeZLabels(data);
        computeConfusionMatrixForCounts("LOCAL", zLabels, data.getPositiveLabelsArray());
        computeYScore("LOCAL", zLabels, data.getPositiveLabelsArray());

        // z dataset initialized with nil labels and the sentence-level features array
        // Dataset<String, String> zDataset = initializeZDataset(totalSentences, zLabels, data.getDataArray());

        // y dataset initialized to be empty, as it will be populated during the E step
        RVFDataset<String, String> yDataset = new RVFDataset<String, String>();

        // run EM
        for (int epoch = 0; epoch < numberOfTrainEpochs; epoch++) {
            zUpdatesInOneEpoch = 0;
            Log.severe("***EPOCH " + epoch + "***");

            // we compute scores in each epoch using these labels
            int [][] zLabelsPredictedByZ = new int[zLabels.length][];
            for(int i = 0; i < zLabels.length; i ++)
                zLabelsPredictedByZ[i] = new int[zLabels[i].length];

            //
            // E-step
            //
            Log.severe("E-STEP");
            // for each group, infer the hidden sentence labels z_i,s
            for(int fold = 0; fold < numberOfFolds; fold ++) {
                LinearClassifier<String, String> zClassifier = zClassifiers[fold];
                int start = foldStart(fold, data.getDataArray().length);
                int end = foldEnd(fold, data.getDataArray().length);

                for (int i = start; i < end; i++) {
                    Datum<String,String>[] group = data.getDataArray()[i];
                    randomizeGroup(group, epoch);

                    Set<Integer> positiveLabels = data.getPositiveLabelsArray()[i];
                    Set<Integer> negativeLabels = data.getNegativeLabelsArray()[i];
                    Counter<String> [] zLogProbs =
                            ErasureUtils.uncheckedCast(new Counter[group.length]);

                    predictZLabels(group, zLabelsPredictedByZ[i], zClassifier);

                    switch(inferenceType) {
                        case SLOW:
                            inferZLabels(group, positiveLabels, negativeLabels, zLabels[i], zLogProbs, zClassifier, epoch);
                            break;
                        case STABLE:
                            inferZLabelsStable(group, positiveLabels, negativeLabels, zLabels[i], zLogProbs, zClassifier, epoch);
                            break;
                        default:
                            throw new RuntimeException("ERROR: unknown inference type: " + inferenceType);
                    }

                    assert(positiveLabels.size() == 1);
                    String yLabel = yLabelIndex.get(positiveLabels.iterator().next());

                    // given these predicted z labels, update the features in the y dataset
                    //printGroup(zLabels[i], positiveLabels);
                    addYDatum(yDataset, yLabel, zLabels[i]);
                }
            }

            computeConfusionMatrixForCounts("EPOCH " + epoch, zLabels, data.getPositiveLabelsArray());
            computeYScore("EPOCH " + epoch, zLabels, data.getPositiveLabelsArray());
            computeYScore("(Z ONLY) EPOCH " + epoch, zLabelsPredictedByZ, data.getPositiveLabelsArray());

            Log.severe("In epoch #" + epoch + " zUpdatesInOneEpoch = " + zUpdatesInOneEpoch);
            if(zUpdatesInOneEpoch == 0){
                Log.severe("Stopping training. Did not find any changes in the Z labels!");
                break;
            }

            // update the labels in the z dataset
            GeneralDataset<String, String> zDataset = initializeZDataset(totalSentences, zLabels, data.getDataArray());

            //
            // M step
            //
            Log.severe("M-STEP");
            // learn the weights of the sentence-level multi-class classifier
            for(int fold = 0; fold < numberOfFolds; fold ++){
                Log.severe("EPOCH " + epoch + ": Training Z classifier for fold #" + fold);
                int [][] foldTrainArray = makeTrainDataArrayForFold(zDataset.getDataArray(), fold);
                int [] foldTrainLabels = makeTrainLabelArrayForFold(zDataset.getLabelsArray(), fold);
                Dataset<String, String> zd = new Dataset<String, String>(zLabelIndex, foldTrainLabels, featureIndex, foldTrainArray);
                LinearClassifier<String, String> zClassifier = zFactory.trainClassifier(zd);
                zClassifiers[fold] = zClassifier;
            }

            // learn the weights of each of the top-level two-class classifiers
            if(trainY) {
                Log.severe("EPOCH " + epoch + ": Training Y classifier");
                yClassifier = yFactory.trainClassifier(yDataset);
            }

            // save this epoch's model
            String epochPath = makeEpochPath(epoch);
            try {
                if(epochPath != null) {
                    makeSingleZClassifier(zDataset, zFactory);
                    save(epochPath);
                }
            } catch (IOException ex) {
                Log.severe("WARNING: could not save model of epoch " + epoch + " to path: " + epochPath);
                Log.severe("Exception message: " + ex.getMessage());
            }

            // clear our y dataset so it can be repopulated on next iteration
            yDataset = new RVFDataset<String, String>();
        }

        GeneralDataset<String, String> zDataset = initializeZDataset(totalSentences, zLabels, data.getDataArray());
        makeSingleZClassifier(zDataset, zFactory);
    }

    private LinearClassifier<String, String> initializeYStupidly(Index<String> labelIndex) {
        Index<String> yFeatureIndex = new HashIndex<String>();
        yFeatureIndex.addAll(Y_FEATURES_FOR_INITIAL_MODEL);

        Index<String> thisYLabelIndex = new HashIndex<String>();
        thisYLabelIndex.add(positiveClass);
        thisYLabelIndex.add(negativeClass);

        double[][] weights = initializeWeights(yFeatureIndex.size(), thisYLabelIndex.size());
        // do nothing, keep weights as 0
        LinearClassifier<String, String> classifier =  new LinearClassifier<String, String>(weights, yFeatureIndex, thisYLabelIndex);
        Log.severe("Created the classifier with " + yFeatureIndex.size() + " features");
        return classifier;
    }

    private void addYDatum(
            RVFDataset<String, String> yDataset,
            String yLabel,
            int [] zLabels) {
        Counter<String> features = extractYFeatures(zLabels);
        RVFDatum<String, String> datum = new RVFDatum<String, String>(features, yLabel);
        yDataset.add(datum);
    }

    private Counter<String> extractYFeatures(int[] zLabels) {
        Counter<String> features = new ClassicCounter<String>();
        int positiveCount = ThresholdedJointBayes.count(zLabels, positiveIndex);
        int negativeCount = ThresholdedJointBayes.count(zLabels, negativeIndex);
        double fraction = ((double) positiveCount) / (positiveCount + negativeCount);
        features.setCount(POSITIVE_CLASS_FRACTION, fraction);
        features.setCount(BIAS_FEAT, 1.0);
        return features;
    }

    @Override
    /** updates the zLabels array with new predicted z labels */
    void inferZLabelsStable(Datum<String,String>[] group,
                            Set<Integer> positiveLabels,
                            Set<Integer> negativeLabels,
                            int[] zLabels,
                            Counter<String> [] zLogProbs,
                            LinearClassifier<String, String> zClassifier,
                            int epoch) {
        boolean showProbs = false;
        boolean verbose = true;

        assert(positiveLabels.size() == 1);
        if(verbose) {
            System.err.print("inferZLabels: ");
            if(positiveLabels.size() > 1) System.err.println("MULTI RELATION");
            else if(positiveLabels.size() == 1) System.err.println("SINGLE RELATION");
            else System.err.println("NIL RELATION");
            System.err.println("positiveLabels: " + positiveLabels);
            System.err.println("negativeLabels: " + negativeLabels);
            System.err.print("Current zLabels:");
            for(int i = 0; i < zLabels.length; i ++) System.err.print(" " + zLabels[i]);
            System.err.println();
        }

        // compute the Z probabilities; these do not change
        computeZLogProbs(group, zLogProbs, zClassifier, epoch);

        for (int s = 0; s < group.length; s++) {
            double maxProb = Double.NEGATIVE_INFINITY;
            int bestLabel = -1;

            Counter<String> zProbabilities = zLogProbs[s];
            Counter<String> jointProbabilities = new ClassicCounter<String>();

            int origZLabel = zLabels[s];
            for (String candidate : zProbabilities.keySet()) {
                int candidateIndex = zLabelIndex.indexOf(candidate);

                // start with z probability
                if(showProbs) System.err.println("\tProbabilities for z[" + s + "]:");
                double prob = zProbabilities.getCount(candidate);
                zLabels[s] = candidateIndex;
                if(showProbs) System.err.println("\t\tlocal (" + zLabels[s] + ") = " + prob);

                // add the y probabilities
                String yLabel = yLabelIndex.get(positiveLabels.iterator().next());
                Datum<String, String> yDatum = new RVFDatum<String, String>(extractYFeatures(zLabels), "");
                Counter<String> yProbabilities = yClassifiers.get(yLabel).logProbabilityOf(yDatum);
                double v = yProbabilities.getCount(yLabel);
                if(showProbs) System.err.println("\t\t\ty+ (" + yLabel + ") = " + v);
                prob += v;

                if(showProbs) System.err.println("\t\ttotal (" + zLabels[s] + ") = " + prob);
                jointProbabilities.setCount(candidate, prob);

                // update the current maximum
                if (prob > maxProb) {
                    maxProb = prob;
                    bestLabel = zLabels[s];
                }
            }

            if(bestLabel != -1 && bestLabel != origZLabel) {
                // found the best flip for this mention
                if(verbose) System.err.println("\tNEW zLabels[" + s + "] = " + bestLabel);
                zLabels[s] = bestLabel;
                zUpdatesInOneEpoch ++;
            } else {
                // nothing good found
                zLabels[s] = origZLabel;
            }
        } // end scan for group
    }

    @Override
    /** updates the zLabels array with new predicted z labels */
    void inferZLabels(Datum<String,String>[] group,
                      Set<Integer> positiveLabels,
                      Set<Integer> negativeLabels,
                      int[] zLabels,
                      Counter<String> [] zLogProbs,
                      LinearClassifier<String, String> zClassifier,
                      int epoch) {
        throw new UnsupportedOperationException("inferZLabels not implemented!");
    }

    @Override
    public Counter<String> classifyMentions(List<Datum<String,String>> sentences) {
        throw new UnsupportedOperationException("classifyMentions not implemented!");
    }

}
