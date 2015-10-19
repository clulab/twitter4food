package edu.arizona.sista.twitter4food;

import edu.stanford.nlp.classify.*;
import edu.stanford.nlp.kbp.slotfilling.classify.JointlyTrainedRelationExtractor;
import edu.stanford.nlp.kbp.slotfilling.classify.ModelType;
import edu.stanford.nlp.kbp.slotfilling.classify.MultiLabelDataset;
import edu.stanford.nlp.kbp.slotfilling.common.Constants;
import edu.stanford.nlp.kbp.slotfilling.common.Log;
import edu.stanford.nlp.kbp.slotfilling.common.Props;
import edu.stanford.nlp.ling.BasicDatum;
import edu.stanford.nlp.ling.Datum;
import edu.stanford.nlp.ling.RVFDatum;
import edu.stanford.nlp.stats.ClassicCounter;
import edu.stanford.nlp.stats.Counter;
import edu.stanford.nlp.stats.Counters;
import edu.stanford.nlp.util.*;

import java.io.*;
import java.util.*;

/**
 * Modification of the MIML-RE model to predict real-valued y-outputs from three-class z-labels. Z-labels have a positive class, a negative, and a neutral, and the y-value is simply positive / (positive + negative)
 * @author Julie Tibshirani (jtibs)
 * @author nallapat@ai.sri.com
 * @author Mihai
 * @author dfried
 *
 */
public class GroupRegression {
    public String positiveClass;
    public String negativeClass;

    public int positiveIndex;
    public int negativeIndex;

    private static final long serialVersionUID = -7961154075748697901L;

    static enum LOCAL_CLASSIFICATION_MODE {
        WEIGHTED_VOTE,
        SINGLE_MODEL
    }

    protected static final LOCAL_CLASSIFICATION_MODE localClassificationMode =
            LOCAL_CLASSIFICATION_MODE.WEIGHTED_VOTE;

    /**
     * sentence-level multi-class classifier, trained across all sentences
     * one per fold to avoid overfitting
     */
    public LinearClassifier<String, String> [] zClassifiers;
    /** this is created only if localClassificationMode == SINGLE_MODEL */
    LinearClassifier<String, String> zSingleClassifier;

    protected Index<String> featureIndex;
    protected Index<String> zLabelIndex;

    /** Run EM for this many epochs */
    protected final int numberOfTrainEpochs;

    /** Organize the Z classifiers into this many folds for cross validation */
    protected int numberOfFolds;

    /**
     * Should we skip the EM loop?
     * If true, this is essentially a multiclass local LR classifier
     */
    protected final boolean onlyLocalTraining;

    /** Required to know where to save the initial models */
    protected final String initialModelPath;

    /** Sigma for the Z classifiers */
    protected final double zSigma;

    /** Counts number of flips for Z labels in one epoch */
    protected int zUpdatesInOneEpoch = 0;

    protected final LocalFilter localDataFilter;

    /** Which feature model to use */
    protected final int featureModel;

    /** Should we train Y models? */
    protected final boolean trainY;

    /** These label dependencies were seen in training */
    protected Set<String> knownDependencies;

    protected String serializedModelPath;

    protected final boolean useRVF;

    public GroupRegression(Properties props, boolean onlyLocal, boolean useRVF, double zSigma, String positiveClass, String negativeClass) {
        // We need workDir, serializedRelationExtractorName, modelType, samplingRatio to serialize the initial models
        String workDir = props.getProperty(Props.WORK_DIR);
        String srn = props.getProperty(Props.SERIALIZED_MODEL_PATH, "kbp_relation_model");
        if (srn.endsWith(Constants.SER_EXT))
            srn = srn.substring(0, srn.length() - Constants.SER_EXT.length());
        String serializedRelationExtractorName = srn;
        ModelType modelType = ModelType.stringToModel(props.getProperty(Props.MODEL_TYPE, Constants.DEFAULT_MODEL));
        double samplingRatio = PropertiesUtils.getDouble(props,
                Props.NEGATIVES_SAMPLE_RATIO,
                Constants.DEFAULT_NEGATIVES_SAMPLING_RATIO);
        initialModelPath = makeInitialModelPath(
                workDir,
                serializedRelationExtractorName,
                modelType,
                samplingRatio);
        numberOfTrainEpochs = PropertiesUtils.getInt(props,
                Props.EPOCHS, 10);
        numberOfFolds = PropertiesUtils.getInt(props,
                Props.FOLDS, 5);
        this.zSigma = zSigma;
        localDataFilter =
                makeLocalDataFilter(props.getProperty(
                        Props.FILTER, "all"));
        featureModel = PropertiesUtils.getInt(props,
                Props.FEATURES, 0);
        trainY = PropertiesUtils.getBool(props,
                Props.TRAINY, true);
        onlyLocalTraining = onlyLocal;
        serializedModelPath = makeModelPath(
                workDir,
                serializedRelationExtractorName,
                modelType,
                samplingRatio);
        this.useRVF = useRVF;
        this.positiveClass = positiveClass;
        this.negativeClass = negativeClass;
    }

    public GroupRegression(Properties props, boolean useRVF, String positiveClass, String negativeClass) {
        this(props, false, useRVF, 1.0, positiveClass, negativeClass);
    }

    private static LocalFilter makeLocalDataFilter(String fv) {
        LocalFilter localDataFilter;
        if(fv.equalsIgnoreCase("all"))
            localDataFilter = new AllFilter();
        else if(fv.equalsIgnoreCase("single"))
            localDataFilter = new SingleFilter();
        else if(fv.equals("redundancy"))
            localDataFilter = new RedundancyFilter();
        else if(fv.startsWith("large")) {
            int thr = Integer.valueOf(fv.substring(5));
            assert(thr > 0);
            localDataFilter = new LargeFilter(thr);
        }
        else
            throw new RuntimeException("ERROR: unknown local data filter " + fv);
        Log.severe("Using local data filter: " + fv);
        return localDataFilter;
    }

    public GroupRegression(
            String initialModelPath,
            int numberOfTrainEpochs,
            int numberOfFolds,
            String localFilter,
            int featureModel,
            boolean trainY,
            boolean onlyLocalTraining,
            boolean useRVF,
            double zSigma) {
        this.initialModelPath = initialModelPath;
        this.numberOfTrainEpochs = numberOfTrainEpochs;
        this.numberOfFolds = numberOfFolds;
        this.zSigma = zSigma;
        this.onlyLocalTraining = onlyLocalTraining;
        this.localDataFilter = makeLocalDataFilter(localFilter);
        this.featureModel = featureModel;
        this.trainY = trainY;
        this.serializedModelPath = null;
        this.useRVF = useRVF;
    }

    private static String makeInitialModelPath(
            String workDir,
            String serializedRelationExtractorName,
            ModelType modelType,
            double samplingRatio) {
        return workDir + File.separator + serializedRelationExtractorName +
                "." + modelType + "." + (int) (100.0 * samplingRatio) +
                ".initial" + Constants.SER_EXT;
    }
    private static String makeModelPath(
            String workDir,
            String serializedRelationExtractorName,
            ModelType modelType,
            double samplingRatio) {
        return workDir + File.separator + serializedRelationExtractorName +
                "." + modelType + "." + (int) (100.0 * samplingRatio) +
                Constants.SER_EXT;
    }

    protected int foldStart(int fold, int size) {
        int foldSize = size / numberOfFolds;
        assert(foldSize > 0);
        int start = fold * foldSize;
        assert(start < size);
        return start;
    }

    protected int foldEnd(int fold, int size) {
        // padding if this is the last fold
        if(fold == numberOfFolds - 1)
            return size;

        int foldSize = size / numberOfFolds;
        assert(foldSize > 0);
        int end = (fold + 1) * foldSize;
        assert(end <= size);
        return end;
    }

    protected int [][] initializeZLabels(MultiLabelDataset<String, String> data) {
        // initialize Z labels with the predictions of the local classifiers
        int[][] zLabels = new int[data.getDataArray().length][];
        for(int f = 0; f < numberOfFolds; f ++){
            LinearClassifier<String, String> zClassifier = zClassifiers[f];
            assert(zClassifier != null);
            for(int i = foldStart(f, data.getDataArray().length); i < foldEnd(f, data.getDataArray().length); i ++){
                Datum<String,String>[] group = data.getDataArray()[i];
                zLabels[i] = new int[group.length];
                for(int j = 0; j < group.length; j ++){
                    Datum<String, String> datum = group[j];
                    Counter<String> scores = zClassifier.scoresOf(datum);
                    List<Pair<String, Double>> sortedScores = sortPredictions(scores);
                    int sys = zLabelIndex.indexOf(sortedScores.get(0).first());
                    assert(sys != -1);
                    zLabels[i][j] = sys;
                }
            }
        }

        return zLabels;
    }

    protected void detectDependencyYFeatures(MultiLabelDataset<String, String> data) {
        knownDependencies = new HashSet<String>();
        for(int i = 0; i < data.size(); i ++){
            Set<Integer> labels = data.getPositiveLabelsArray()[i];
            for(Integer src: labels) {
                String srcLabel = data.labelIndex().get(src);
                for(Integer dst: labels) {
                    if(src.intValue() == dst.intValue()) continue;
                    String dstLabel = data.labelIndex().get(dst);
                    String f = makeCoocurrenceFeature(srcLabel, dstLabel);
                    Log.severe("FOUND COOC: " + f);
                    knownDependencies.add(f);
                }
            }
        }
    }

    private static String makeCoocurrenceFeature(String src, String dst) {
        return "co:s|" + src + "|d|" + dst + "|";
    }

    public void train(MultiLabelDataset<String, String> data) {
        String [][] initialLabels = new String[data.getDataArray().length][];
        for (int i = 0; i < initialLabels.length; i++) {
            initialLabels[i] = new String[data.getDataArray()[i].length];
        }
        train(data, initialLabels);
    }

    public void train(MultiLabelDataset<String, String> data, String [][] initialZLabels) {

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

        featureIndex = data.featureIndex();
        yLabelIndex = data.labelIndex();
        zLabelIndex = new HashIndex<String>(yLabelIndex);
        zLabelIndex.add(JointlyTrainedRelationExtractor.UNRELATED);

        positiveIndex = yLabelIndex.indexOf(positiveClass);
        negativeIndex = yLabelIndex.indexOf(negativeClass);

        // initialize classifiers
        zClassifiers = initializeZClassifierLocally(data, featureIndex, zLabelIndex, initialZLabels);
        //yClassifier = initializeYStupidly(yLabelIndex);
        yClassifier = initializeYStepFn(yLabelIndex);

        if(initialModelPath != null) {
            try {
                saveInitialModels(initialModelPath);
            } catch (IOException e1) {
                throw new RuntimeException(e1);
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

                    inferZLabelsStable(group, positiveLabels, negativeLabels, zLabels[i], zLogProbs, zClassifier, epoch);

                    assert(positiveLabels.size() == 1);
                    String yLabel = yLabelIndex.get(positiveLabels.iterator().next());

                    // given these predicted z labels, update the features in the y dataset
                    //printGroup(zLabels[i], positiveLabels);
                    addYDatum(yDataset, yLabel, zLabels[i]);
                }
            }

            // Log.severe("class " + negativeClass);
            // Log.severe("" + POSITIVE_CLASS_FRACTION + ":" + yClassifier.weight(POSITIVE_CLASS_FRACTION, negativeClass));
            // Log.severe("" + BIAS_FEAT + ":" + yClassifier.weight(BIAS_FEAT, negativeClass));

            computeConfusionMatrixForCounts("EPOCH " + epoch, zLabels, data.getPositiveLabelsArray());

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

            Log.severe("yDataset\n"+yDataset);
            for (int i = 0; i < yDataset.getLabelsArray().length; i++) {
                Log.severe("" + i + ": " + yDataset.getRVFDatum(i));
            }
            Log.severe("classifierWeights");
            Log.severe("class " + positiveClass);
            Log.severe("" + POSITIVE_CLASS_FRACTION + ":" + yClassifier.weight(POSITIVE_CLASS_FRACTION, positiveClass));
            Log.severe("" + BIAS_FEAT + ":" + yClassifier.weight(BIAS_FEAT, positiveClass));

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

    void randomizeGroup(Datum<String,String>[] group, int randomSeed) {
        Random rand = new Random(randomSeed);
        for(int j = group.length - 1; j > 0; j --){
            int randIndex = rand.nextInt(j);

            Datum<String,String> tmp = group[randIndex];
            group[randIndex] = group[j];
            group[j] = tmp;
        }
    }

    void computeConfusionMatrixForCounts(String name, int [][] zLabels, Set<Integer> [] golds) {
        Counter<Integer> pos = new ClassicCounter<Integer>();
        Counter<Integer> neg = new ClassicCounter<Integer>();
        int nilIndex = zLabelIndex.indexOf(JointlyTrainedRelationExtractor.UNRELATED);
        for(int i = 0; i < zLabels.length; i ++) {
            int [] zs = zLabels[i];
            Counter<Integer> freqs = new ClassicCounter<Integer>();
            for(int z: zs)
                if(z != nilIndex)
                    freqs.incrementCount(z);
            Set<Integer> gold = golds[i];
            for(int z: freqs.keySet()) {
                int f = (int) freqs.getCount(z);
                if(gold.contains(z)){
                    pos.incrementCount(f);
                } else {
                    neg.incrementCount(f);
                }
            }
        }
        Log.severe("CONFUSION MATRIX for " + name);
        Log.severe("CONFUSION MATRIX POS: " + pos);
        Log.severe("CONFUSION MATRIX NEG: " + neg);
    }

    protected void makeSingleZClassifier(
            GeneralDataset<String, String> zDataset,
            LinearClassifierFactory<String, String> zFactory) {
        if(localClassificationMode == LOCAL_CLASSIFICATION_MODE.SINGLE_MODEL) {
            // train a single Z classifier over the entire data
            Log.severe("Training the final Z classifier...");
            zSingleClassifier = zFactory.trainClassifier(zDataset);
        } else {
            zSingleClassifier = null;
        }
    }

    static abstract class LocalFilter {
        public abstract boolean filterZ(Datum<String,String>[] data, Set<Integer> posLabels);
        public boolean filterY(Datum<String,String>[] data, Set<Integer> posLabels) { return true; }
    }
    static class LargeFilter extends LocalFilter {
        final int threshold;
        public LargeFilter(int thresh) {
            this.threshold = thresh;
        }
        @Override
        public boolean filterZ(Datum<String,String>[] data, Set<Integer> posLabels) {
            if(data.length > threshold) return false;
            return true;
        }
        @Override
        public boolean filterY(Datum<String,String>[] data, Set<Integer> posLabels) {
            if(data.length > threshold) return false;
            return true;
        }
    }
    static class AllFilter extends LocalFilter {
        @Override
        public boolean filterZ(Datum<String,String>[] data, Set<Integer> posLabels) {
            return true;
        }
    }
    static class SingleFilter extends LocalFilter {
        @Override
        public boolean filterZ(Datum<String,String>[] data, Set<Integer> posLabels) {
            if(posLabels.size() <= 1) return true;
            return false;
        }
    }
    static class RedundancyFilter extends LocalFilter {
        @Override
        public boolean filterZ(Datum<String,String>[] data, Set<Integer> posLabels) {
            if(posLabels.size() <= 1 && data.length > 1) return true;
            return false;
        }
    }

    private static GeneralDataset<String, String> makeLocalData(
            Datum<String,String>[][] dataArray,
            Set<Integer> [] posLabels,
            Index<String> labelIndex,
            Index<String> featureIndex,
            LocalFilter f,
            int fold,
            boolean useRVF,
            String[][] overrideZLabels) {
        // Detect the size of the dataset for the local classifier
        int flatSize = 0, posGroups = 0, negGroups = 0;
        for(int i = 0; i < dataArray.length; i ++) {
            if(! f.filterZ(dataArray[i], posLabels[i])) continue;
            if(posLabels[i].size() == 0) {
                // negative example
                flatSize += dataArray[i].length;
                negGroups ++;
            } else {
                // 1+ positive labels
                flatSize += dataArray[i].length * posLabels[i].size();
                posGroups ++;
            }
        }
        Log.severe("Explored " + posGroups + " positive groups and " + negGroups + " negative groups, yielding " + flatSize + " flat/local datums.");

        //
        // Construct the flat local classifier
        //
        Datum<String,String>[] localTrainData = new Datum[flatSize];
        int [] localTrainLabels = new int[flatSize];
        float [] weights = new float[flatSize];
        int offset = 0, posCount = 0;
        Set<Integer> negLabels = new HashSet<Integer>();
        int nilIndex = labelIndex.indexOf(JointlyTrainedRelationExtractor.UNRELATED);
        negLabels.add(nilIndex);
        for(int i = 0; i < dataArray.length; i ++) {
            if(! f.filterZ(dataArray[i], posLabels[i])) continue;
            Datum<String,String>[] group = dataArray[i];
            Set<Integer> labels = posLabels[i];
            if(labels.size() == 0) labels = negLabels;

            float weight = (float) 1.0 / (float) labels.size();
            for(Integer label: labels) {
                for(int j = 0; j < group.length; j ++){
                    localTrainData[offset] = group[j];
                    if (overrideZLabels[i][j] != null) {
                        localTrainLabels[offset] = labelIndex.indexOf(overrideZLabels[i][j]);
                    } else {
                        localTrainLabels[offset] = label;
                    }
                    weights[offset] = weight;
                    if(label != nilIndex) posCount ++;
                    offset ++;
                    if(offset >= flatSize) break;
                }
                if(offset >= flatSize) break;
            }
            if(offset >= flatSize) break;
        }

        GeneralDataset<String, String> dataset;

        if (useRVF) {
            dataset = new WeightedRVFDataset<String,String>();
            dataset.featureIndex = featureIndex;
            dataset.labelIndex = labelIndex;
            for (int i = 0; i < localTrainData.length; i++) {
                RVFDatum<String, String> datum = (RVFDatum<String, String>) localTrainData[i];
                datum.setLabel(labelIndex.get(localTrainLabels[i]));
                ((WeightedRVFDataset)dataset).add(datum, weights[i]);
            }
        } else {
            dataset = new WeightedDataset<String, String>();
            dataset.labelIndex = labelIndex;
            dataset.featureIndex = featureIndex;

            for (int i = 0; i < localTrainData.length; i++) {
                ((BasicDatum<String,String>) localTrainData[i]).setLabel(labelIndex.get(localTrainLabels[i]));
                ((WeightedDataset<String, String>) dataset).add(localTrainData[i], weights[i]);
            }
        }

        Log.severe("Fold #" + fold + ": Constructed a dataset with " + localTrainData.length +
                " datums, out of which " + posCount + " are positive.");
        if(posCount == 0) throw new RuntimeException("ERROR: cannot handle a dataset with 0 positive examples!");

        return dataset;
    }

    protected int [] makeTrainLabelArrayForFold(int [] labelArray, int fold) {
        int start = foldStart(fold, labelArray.length);
        int end = foldEnd(fold, labelArray.length);
        int [] train = new int[labelArray.length - end + start];
        int trainOffset = 0;
        for(int i = 0; i < start; i ++){
            train[trainOffset] = labelArray[i];
            trainOffset ++;
        }
        for(int i = end; i < labelArray.length; i ++){
            train[trainOffset] = labelArray[i];
            trainOffset ++;
        }
        return train;
    }

    protected int [][] makeTrainDataArrayForFold(int [][] dataArray, int fold) {
        int start = foldStart(fold, dataArray.length);
        int end = foldEnd(fold, dataArray.length);
        int [][] train = new int[dataArray.length - end + start][];
        int trainOffset = 0;
        for(int i = 0; i < start; i ++){
            train[trainOffset] = dataArray[i];
            trainOffset ++;
        }
        for(int i = end; i < dataArray.length; i ++){
            train[trainOffset] = dataArray[i];
            trainOffset ++;
        }
        return train;
    }

    private Pair<Datum<String,String>[][], Datum<String,String>[][]> makeDataArraysForFold(Datum<String,String>[][] dataArray, int fold) {
        int start = foldStart(fold, dataArray.length);
        int end = foldEnd(fold, dataArray.length);
        Datum<String,String>[][] train = new Datum[dataArray.length - end + start][];
        Datum<String,String>[][] test = new Datum[end - start][];
        int trainOffset = 0, testOffset = 0;
        for(int i = 0; i < dataArray.length; i ++){
            if(i < start){
                train[trainOffset] = dataArray[i];
                trainOffset ++;
            } else if(i < end) {
                test[testOffset] = dataArray[i];
                testOffset ++;
            } else {
                train[trainOffset] = dataArray[i];
                trainOffset ++;
            }
        }
        return new Pair<Datum<String,String>[][], Datum<String,String>[][]>(train, test);
    }

    @SuppressWarnings("unchecked")
    private Pair<String [][], String [][] > makeZLabelSetsForFold(String [][] labelSet, int fold) {
        int start = foldStart(fold, labelSet.length);
        int end = foldEnd(fold, labelSet.length);
        String [][] train = new String[labelSet.length - end + start][];
        String [][] test = new String[end - start][];
        int trainOffset = 0, testOffset = 0;
        for(int i = 0; i < labelSet.length; i ++){
            if(i < start){
                train[trainOffset] = labelSet[i];
                trainOffset ++;
            } else if(i < end) {
                test[testOffset] = labelSet[i];
                testOffset ++;
            } else {
                train[trainOffset] = labelSet[i];
                trainOffset ++;
            }
        }
        return new Pair<String[][], String[][]>(train, test);

    }

    @SuppressWarnings("unchecked")
    private Pair<Set<Integer> [], Set<Integer> []> makeLabelSetsForFold(Set<Integer> [] labelSet, int fold) {
        int start = foldStart(fold, labelSet.length);
        int end = foldEnd(fold, labelSet.length);
        Set<Integer>[] train = new HashSet[labelSet.length - end + start];
        Set<Integer>[] test = new HashSet[end - start];
        int trainOffset = 0, testOffset = 0;
        for(int i = 0; i < labelSet.length; i ++){
            if(i < start){
                train[trainOffset] = labelSet[i];
                trainOffset ++;
            } else if(i < end) {
                test[testOffset] = labelSet[i];
                testOffset ++;
            } else {
                train[trainOffset] = labelSet[i];
                trainOffset ++;
            }
        }
        return new Pair<Set<Integer>[], Set<Integer>[]>(train, test);
    }

    @SuppressWarnings("unchecked")
    protected LinearClassifier<String, String> [] initializeZClassifierLocally(
            MultiLabelDataset<String, String> data,
            Index<String> featureIndex,
            Index<String> labelIndex,
            // initialZLabels: groups of individual z labels, or null - if an individual label is null, use the label from the multilabel dataset
            String[][] initialZLabels) {


        LinearClassifier<String, String> [] localClassifiers = new LinearClassifier[numberOfFolds];

        // construct the initial model for each fold
        for(int fold = 0; fold < numberOfFolds; fold ++){
            Log.severe("Constructing dataset for the local model in fold #" + fold + "...");
            Pair<Datum<String,String>[][], Datum<String,String>[][]> dataArrays = makeDataArraysForFold(data.getDataArray(), fold);
            Pair<Set<Integer> [], Set<Integer> []> labelSets =
                    makeLabelSetsForFold(data.getPositiveLabelsArray(), fold);
            Pair<String[][], String[][]> zLabelSets = makeZLabelSetsForFold(initialZLabels, fold);

            Datum<String,String>[][] trainDataArray = dataArrays.first();
            Set<Integer> [] trainPosLabels = labelSets.first();
            String[][] trainInitialZLabels = zLabelSets.first();
            assert(trainDataArray.length == trainPosLabels.length);
            assert(trainInitialZLabels.length == trainPosLabels.length);

            Datum<String,String>[][] testDataArray = dataArrays.second();
            Set<Integer> [] testPosLabels = labelSets.second();
            String[][] testInitialZLabels = zLabelSets.second();
            assert(testDataArray.length == testPosLabels.length);
            assert(testInitialZLabels.length == testPosLabels.length);

            //
            // Construct the flat local classifier
            //
            GeneralDataset<String, String> dataset =
                    makeLocalData(trainDataArray, trainPosLabels, labelIndex, featureIndex, localDataFilter, fold, useRVF, trainInitialZLabels);

            //
            // Train local classifier
            //
            Log.severe("Fold #" + fold + ": Training local model...");
            LinearClassifierFactory<String, String> factory =
                    new LinearClassifierFactory<String, String>(1e-4, false, zSigma);
            LinearClassifier<String, String> localClassifier = factory.trainClassifier(dataset);
            Log.severe("Fold #" + fold + ": Training of the local classifier completed.");

            //
            // Evaluate the classifier on the multidataset
            //
            // TODO: this evaluation doesn't work with the override labels
            int nilIndex = labelIndex.indexOf(JointlyTrainedRelationExtractor.UNRELATED);
            Log.severe("Fold #" + fold + ": Evaluating the local classifier on the hierarchical dataset...");
            int total = 0, predicted = 0, correct = 0;
            for(int i = 0; i < testDataArray.length; i ++){
                Datum<String,String>[] group = testDataArray[i];
                Set<Integer> gold = testPosLabels[i];
                Set<Integer> pred = new HashSet<Integer>();
                for(int j = 0; j < group.length; j ++){
                    Datum<String,String> datum = group[j];
                    Counter<String> scores = localClassifier.scoresOf(datum);
                    List<Pair<String, Double>> sortedScores = sortPredictions(scores);
                    int sys = labelIndex.indexOf(sortedScores.get(0).first());
                    if(sys != nilIndex) pred.add(sys);
                }
                total += gold.size();
                predicted += pred.size();
                for(Integer pv: pred) {
                    if(gold.contains(pv)) correct ++;
                }
            }
            double p = (double) correct / (double) predicted;
            double r = (double) correct / (double) total;
            double f1 = (p != 0 && r != 0 ? 2*p*r/(p+r) : 0);
            Log.severe("Fold #" + fold + ": Training score on the hierarchical dataset: P " + p + " R " + r + " F1 " + f1);

            Log.severe("Fold #" + fold + ": Created the Z classifier with " + labelIndex.size() +
                    " labels and " + featureIndex.size() + " features.");
            localClassifiers[fold] = localClassifier;
        }

        return localClassifiers;
    }

    private static final double BIG_WEIGHT = +10;

    protected GeneralDataset<String, String> initializeZDataset(int totalSentences, int[][] zLabels, Datum<String,String>[][] data) {
        GeneralDataset<String,String> dataset;

        if (useRVF) {
            dataset = new RVFDataset<String, String>(featureIndex, zLabelIndex);
        } else {
            dataset = new Dataset<String, String>(featureIndex, zLabelIndex);
        }

        int count = 0;
        for (int i = 0; i < data.length; i++) {
            Datum<String,String>[] group = data[i];
            for (int s = 0; s < group.length; s++) {
                Datum<String,String> datum = group[s];
                if (useRVF) {
                    ((RVFDatum<String,String>) datum).setLabel(zLabelIndex.get(zLabels[i][s]));
                } else {
                    ((BasicDatum<String,String>) datum).setLabel(zLabelIndex.get(zLabels[i][s]));
                }
                dataset.add(datum);
                count++;
            }
        }

        Log.severe("Created the Z dataset with " + count + " datums.");

        return dataset;
    }

    void predictZLabels(Datum<String,String>[] group,
                        int[] zLabels,
                        LinearClassifier<String, String> zClassifier) {
        for (int s = 0; s < group.length; s++) {
            Counter<String> probs = zClassifier.logProbabilityOf(group[s]);
            zLabels[s] = zLabelIndex.indexOf(Counters.argmax(probs));
        }
    }

    void computeZLogProbs(Datum<String,String>[] group,
                          Counter<String> [] zLogProbs,
                          LinearClassifier<String, String> zClassifier,
                          int epoch) {
        for (int s = 0; s < group.length; s ++) {
            zLogProbs[s] = zClassifier.logProbabilityOf(group[s]);
        }
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
                Counter<String> yProbabilities = yClassifier.logProbabilityOf(yDatum);
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

    public static List<Pair<String, Double>> sortPredictions(Counter<String> scores) {
        List<Pair<String, Double>> sortedScores = new ArrayList<Pair<String,Double>>();
        for(String key: scores.keySet()) {
            sortedScores.add(new Pair<String, Double>(key, scores.getCount(key)));
        }
        sortPredictions(sortedScores);
        return sortedScores;
    }

    private static void sortPredictions(List<Pair<String, Double>> scores) {
        Collections.sort(scores, new Comparator<Pair<String, Double>>() {
            @Override
            public int compare(Pair<String, Double> o1, Pair<String, Double> o2) {
                if(o1.second() > o2.second()) return -1;
                if(o1.second().equals(o2.second())){
                    // this is an arbitrary decision to disambiguate ties
                    int c = o1.first().compareTo(o2.first());
                    if(c < 0) return -1;
                    else if(c == 0) return 0;
                    return 1;
                }
                return 1;
            }
        });
    }

    /**
     * Implements weighted voting over the different Z classifiers in each fold
     * @return Probabilities (NOT log probs!) for each known label
     */
    public Counter<String> classifyLocally(Datum<String,String> datum) {
        if(localClassificationMode == LOCAL_CLASSIFICATION_MODE.WEIGHTED_VOTE) {
            Counter<String> sumProbs = new ClassicCounter<String>();

            for(int fold = 0; fold < numberOfFolds; fold ++) {
                LinearClassifier<String, String> zClassifier = zClassifiers[fold];
                Counter<String> probs = zClassifier.probabilityOf(datum);
                sumProbs.addAll(probs);
            }

            for(String l: sumProbs.keySet())
                sumProbs.setCount(l, sumProbs.getCount(l) / numberOfFolds);
            return sumProbs;
        }

        if(localClassificationMode == LOCAL_CLASSIFICATION_MODE.SINGLE_MODEL) {
            Counter<String> probs = zSingleClassifier.probabilityOf(datum);
            return probs;
        }

        throw new RuntimeException("ERROR: classification mode " + localClassificationMode + " not supported!");
    }


    public Counter<String> classifyMentions(List<Datum<String,String>> sentences) {
        throw new UnsupportedOperationException("classifyMentions not implemented!");
    }

    public static int count(int[] xs, int x) {
        int count = 0;
        for (int xP: xs) {
            if (xP == x) count++;
        }
        return count;
    }
}
