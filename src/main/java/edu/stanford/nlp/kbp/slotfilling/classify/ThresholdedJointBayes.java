package edu.stanford.nlp.kbp.slotfilling.classify;

import java.util.*;

import java.io.File;
import java.io.IOException;

import edu.stanford.nlp.classify.*;
import edu.stanford.nlp.kbp.slotfilling.common.Log;
import edu.stanford.nlp.ling.Datum;
import edu.stanford.nlp.stats.ClassicCounter;
import edu.stanford.nlp.stats.Counter;
import edu.stanford.nlp.util.ErasureUtils;
import edu.stanford.nlp.util.HashIndex;
import edu.stanford.nlp.util.Pair;

/**
 * Created by df345 on 04/07/15.
 */
public class ThresholdedJointBayes extends JointBayesRelationExtractor {
    public double initialThreshold;
    public double currentThreshold;

    public String positiveClass;
    public String negativeClass;

    public int positiveIndex;
    public int negativeIndex;

    public ThresholdedJointBayes(String initialModelPath, int numberOfTrainEpochs, int numberOfFolds, String localFilter, int featureModel, String inferenceType, boolean trainY, boolean onlyLocalTraining, boolean useRVF, double zSigma, double initialThreshold, String positiveClass, String negativeClass) {
        super(initialModelPath, numberOfTrainEpochs, numberOfFolds, localFilter, featureModel, inferenceType, trainY, onlyLocalTraining, useRVF, zSigma, 1.0);
        this.initialThreshold = initialThreshold;
        this.positiveClass = positiveClass;
        this.negativeClass = negativeClass;
    }

    public ThresholdedJointBayes(Properties props, boolean onlyLocal, boolean useRVF, double zSigma, double initialThreshold, String positiveClass, String negativeClass) {
        super(props, onlyLocal, useRVF, zSigma, 1.0);
        this.initialThreshold = initialThreshold;
        this.positiveClass = positiveClass;
        this.negativeClass = negativeClass;
    }

    public ThresholdedJointBayes(Properties props, boolean useRVF, double initialThreshold, String positiveClass, String negativeClass) {
        this(props, false, useRVF, 1.0, initialThreshold, positiveClass, negativeClass);
    }

    @Override
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

            positiveIndex = zLabelIndex.indexOf(positiveClass);
            negativeIndex = zLabelIndex.indexOf(negativeClass);

            // initialize classifiers
            zClassifiers = initializeZClassifierLocally(data, featureIndex, zLabelIndex);
            // yClassifiers = initializeYClassifiersWithAtLeastOnce(yLabelIndex);
            currentThreshold = initialThreshold;

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

        // detectDependencyYFeatures(data);

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
        // Map<String, RVFDataset<String, String>> yDatasets = initializeYDatasets();

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
            int[] yLabels = new int[zLabels.length];
            for (int i = 0; i < data.getPositiveLabelsArray().length; i++) {
                Set<Integer> positiveLabels = data.getPositiveLabelsArray()[i];
                assert(positiveLabels.size() == 1);
                yLabels[i] = positiveLabels.iterator().next();
            }

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

                    // given these predicted z labels, update the features in the y dataset
                    //printGroup(zLabels[i], positiveLabels);
//                    for (int y : positiveLabels) {
//                        String yLabel = yLabelIndex.get(y);
//                        addYDatum(yDatasets.get(yLabel), yLabel, zLabels[i], zLogProbs, true);
//                    }
//                    for (int y : negativeLabels) {
//                        String yLabel = yLabelIndex.get(y);
//                        addYDatum(yDatasets.get(yLabel), yLabel, zLabels[i], zLogProbs, false);
//                    }
                }
            }

            computeConfusionMatrixForCounts("EPOCH " + epoch, zLabels, data.getPositiveLabelsArray());
//            computeYScore("EPOCH " + epoch, zLabels, data.getPositiveLabelsArray());
//            computeYScore("(Z ONLY) EPOCH " + epoch, zLabelsPredictedByZ, data.getPositiveLabelsArray());

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
                currentThreshold = setThreshold(zLabels, yLabels);
                Log.severe("EPOCH " + epoch + ": threshold value " + currentThreshold);
                System.err.println("EPOCH " + epoch + ": threshold value " + currentThreshold);
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

            // clear our y datasets so they can be repopulated on next iteration
//            yDatasets = initializeYDatasets();
        }

        GeneralDataset<String, String> zDataset = initializeZDataset(totalSentences, zLabels, data.getDataArray());
        makeSingleZClassifier(zDataset, zFactory);
    }

    public double setThreshold(int[][] groupedZLabelIndices, int[] yLabels) {
        double[] ratios = new double[groupedZLabelIndices.length];
        for (int index = 0; index < groupedZLabelIndices.length; index++) {
            ratios[index] = ((double) count(groupedZLabelIndices[index], positiveIndex)) / (count(groupedZLabelIndices[index], negativeIndex) + count(groupedZLabelIndices[index], positiveIndex));
        }

        double[] sortedRatios = Arrays.copyOf(ratios, ratios.length);
        sortedRatios[sortedRatios.length - 1] = 1.0;
        Arrays.sort(sortedRatios);

        double last = 0.0;
        double splitThreshold;
        List<Double> bestThresholds = new ArrayList<Double>();
        int bestCorrectClassification = 0;

        for (double next: sortedRatios) {
            if (next == last) continue;
            splitThreshold = (last + next) / 2;

            int correctClassification = 0;
            for (int group = 0; group < groupedZLabelIndices.length; group++) {
                if ((ratios[group] >= splitThreshold) == (yLabels[group] == positiveIndex)) correctClassification++;
            }

            if (correctClassification > bestCorrectClassification) {
                bestThresholds.clear();
                bestThresholds.add(splitThreshold);
            } else if (correctClassification == bestCorrectClassification) {
                bestThresholds.add(splitThreshold);
            }

            last = next;
        }

        return bestThresholds.get((new Random()).nextInt(bestThresholds.size()));
    }

    public static int count(int[] xs, int x) {
        int count = 0;
        for (int xP: xs) {
            if (xP == x) count++;
        }
        return count;
    }

    public int classifyY(int[] zLabelIndices) {
        int positiveCount = count(zLabelIndices, positiveIndex);
        int negativeCount = count(zLabelIndices, negativeIndex);

        if ((double)positiveCount / negativeCount >= currentThreshold)
            return positiveIndex;
        else
            return negativeIndex;
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
        throw new UnsupportedOperationException("not implemented");
    }

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

        assert(positiveLabels.size() == 1);

        int yLabel = positiveLabels.iterator().next();

        // compute the Z probabilities; these do not change
        computeZLogProbs(group, zLogProbs, zClassifier, epoch);

        int[] indices = new int[group.length];
        for (int i = 0; i < indices.length; i++) indices[i] = i;
        randomizeGroup(indices, epoch);

        for (int s: indices) {
            double maxProb = Double.NEGATIVE_INFINITY;
            int bestLabel = -1;

            Counter<String> zProbabilities = zLogProbs[s];

            int origZLabel = zLabels[s];
            for (String candidate : zProbabilities.keySet()) {
                int candidateIndex = zLabelIndex.indexOf(candidate);

                if(showProbs) System.err.println("\tProbabilities for z[" + s + "]:");
                double prob = zProbabilities.getCount(candidate);
                if(showProbs) System.err.println("\t\tlocal (" + zLabels[s] + ") = " + prob);


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
                // switch if we're flipping within the threshold or toward the threshold
                if (classifyY(zLabels) == yLabel || bestLabel == yLabel) {
                    zUpdatesInOneEpoch++;
                    // otherwise switch back
                } else {
                    zLabels[s] = origZLabel;
                }
            } else {
                // nothing good found
                zLabels[s] = origZLabel;
            }
        } // end scan for group
        while (classifyY(zLabels) != yLabel) {
            double maxProb = Double.NEGATIVE_INFINITY;
            int bestIndex = -1;
            for (int s = 0; s < group.length; s++) {
                // only possibly flip labels that aren't equal to yLabel
                if (zLabels[s] == yLabel) continue;

                Counter<String> zProbabilities = zLogProbs[s];

                double thisProb;
                if (yLabel == positiveIndex) {
                    thisProb = zProbabilities.getCount(positiveClass);
                } else if (yLabel == negativeIndex) {
                    thisProb = zProbabilities.getCount(negativeClass);
                } else {
                    throw new RuntimeException("invalid label in training data " + yLabel);
                }

                if (thisProb > maxProb) {
                    maxProb = thisProb;
                    bestIndex = s;
                }
            }
            if (bestIndex == -1) {
                Log.severe("no labels left to switch, but does not match class label");
                break;
            } else {
                Log.severe("switching label at index " + bestIndex + ", to class " + yLabel + " with probability " + maxProb);
                zLabels[bestIndex] = yLabel;
                zUpdatesInOneEpoch++;
            }
        }
    }

    @Override
    public Counter<String> classifyMentions(List<Datum<String,String>> sentences) {
        throw new UnsupportedOperationException("classifyMentions not implemented");
    }
}
