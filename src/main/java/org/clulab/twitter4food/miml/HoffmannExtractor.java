package org.clulab.twitter4food.miml;

import edu.stanford.nlp.ie.machinereading.structure.RelationMention;
import edu.stanford.nlp.stats.ClassicCounter;
import edu.stanford.nlp.stats.Counter;
import edu.stanford.nlp.util.*;
import org.clulab.twitter4food.struct.MultiLabelDataset;
import org.clulab.twitter4food.struct.RvfMLDataset;

import java.io.*;
import java.util.*;

/**
 * Implements as closely as possible the MultiR algorithm from (Hoffmann et al., 2011),
 * edited to apply to Twitter accounts rather than entity relations in text.
 * @author Mihai
 * @author Dane
 */
public class HoffmannExtractor extends JointlyTrainedRelationExtractor {
  private static final long serialVersionUID = 1L;
  private static final int LABEL_ALL = -1;
  private static final double noneThreshold = 0.5;

  /**
   * Stores weight information for one label
   */
  static class LabelWeights implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * Weights for a binary classifier vector (for one label)
     * This stores the expanded vector for all known features
     */
    double [] weights;

    /** Indicates how many iterations has this vector survived */
    int survivalIterations;

    /**
     * Average vector computed as a weighted sum of all seen vectors
     * The weight for each vector is the number of iterations it survived
     */
    double [] avgWeights;

    LabelWeights(int numFeatures) {
      weights = new double[numFeatures];
      Arrays.fill(weights, 0.0);
      survivalIterations = 0;
      avgWeights = new double[numFeatures];
      Arrays.fill(avgWeights, 0.0);
    }

    void clear() {
      weights = null;
    }

    void updateSurvivalIterations() {
      survivalIterations ++;
    }

    /** Adds the latest weight vector to the average vector */
    public void addToAverage() {
      double confidenceInThisVector = survivalIterations;
      for(int i = 0; i < weights.length; i ++){
        avgWeights[i] += weights[i] * confidenceInThisVector;
      }
    }

    void update(int [] datum, double weight) {
      // add this vector to the avg
      addToAverage();

      // actual update
      for(int d: datum){
        if(d > weights.length) expand();
        weights[d] += weight;
      }

      // this is a new vector, so let's reset its survival counter
      survivalIterations = 0;
    }

    private void expand() {
      throw new RuntimeException("ERROR: LabelWeights.expand() not supported yet!");
    }

    double dotProduct(Counter<Integer> vector) {
      return dotProduct(vector, weights);
    }

    double avgDotProduct(Collection<String> features, Index<String> featureIndex) {
      Counter<Integer> vector = new ClassicCounter<Integer>();
      for(String feat: features) {
        int idx = featureIndex.indexOf(feat);
        if(idx >= 0) vector.incrementCount(idx);
      }

      return dotProduct(vector, avgWeights);
    }

    static double dotProduct(Counter<Integer> vector, double [] weights) {
      double dotProd = 0;
      for (Map.Entry<Integer, Double> entry : vector.entrySet()) {
        if(entry.getKey() == null) throw new RuntimeException("NULL key in " + entry.getKey() + "/" + entry.getValue());
        if(entry.getValue() == null) throw new RuntimeException("NULL value in " + entry.getKey() + "/" + entry.getValue());
        if(weights == null) throw new RuntimeException("NULL weights!");
        if(entry.getKey() < 0 || entry.getKey() >= weights.length) throw new RuntimeException("Invalid key " + entry.getKey() + ". Should be >= 0 and < " + weights.length);
        dotProd += entry.getValue() * weights[entry.getKey()];
      }
      return dotProd;
    }
  }

  /** Stores weight information for each known Z label (including NIL) */
  LabelWeights [] zWeights;

  Index<String> labelIndex;
  Index<String> zFeatureIndex;
  /** Index of the NIL label */
  int nilIndex;
  /** Number of epochs during training */
  final int epochs;

  public HoffmannExtractor(int epochs) {
    this.epochs = epochs;
  }
  public HoffmannExtractor(Properties props) {
    Log.info("HoffmannExtractor configured with the following properties:");
    this.epochs = PropertiesUtils.getInt(props, Props.PERCEPTRON_EPOCHS, 10);
    Log.info("epochs = " + epochs);
  }

  @Override
  public void train(MultiLabelDataset<String, String> dataset) {
    Log.info("Training the \"at least once\" model using "
        + dataset.featureIndex().size() + " features and "
        + "the following labels: " + dataset.labelIndex().toString());

    labelIndex = dataset.labelIndex();
    // add the NIL label
    labelIndex.add(RelationMention.UNRELATED);
    nilIndex = labelIndex.indexOf(RelationMention.UNRELATED);
    zFeatureIndex = dataset.featureIndex();

    zWeights = new LabelWeights[labelIndex.size()];
    for(int i = 0; i < zWeights.length; i ++)
      zWeights[i] = new LabelWeights(dataset.featureIndex().size());

    // repeat for a number of epochs
    for(int t = 0; t < epochs; t ++){
      // randomize the data set in each epoch
      // use a fixed seed for replicability
      Log.info("Started epoch #" + t + "...");
      Log.info("Randomizing...");
      dataset.randomize(t);

      Counter<Integer> posUpdateStats = new ClassicCounter<Integer>();
      Counter<Integer> negUpdateStats = new ClassicCounter<Integer>();
      Counter<Integer> epochLabels = new ClassicCounter<Integer>();
      Map<Integer, Map<Integer, List<Double>>> instLabels = new HashMap<Integer, Map<Integer, List<Double>>>();
      for(int i = 0; i < dataset.labelIndex.size(); i++){
        instLabels.put(i, new HashMap<Integer, List<Double>>());
        for(int j = 0; j < dataset.labelIndex.size(); i++) {
          instLabels.getOrDefault(i, new HashMap<Integer, List<Double>>()).put(j, new ArrayList<Double>());
        }
      }

      // traverse the relation dataset
      for(int i = 0; i < dataset.size(); i ++){
        Log.info("Row " + i);
        int [][] crtGroup = dataset.getDataArray()[i];
        Set<Integer> goldPos = dataset.getLabelsArray()[i];

        trainJointly(crtGroup, goldPos, posUpdateStats, negUpdateStats, epochLabels, instLabels);

        // update the number of iterations an weight vector has survived
        for(LabelWeights zw: zWeights) zw.updateSurvivalIterations();
      }

      Log.info("Epoch #" + t + " completed. Inspected " +
          dataset.size() + " datum groups. Performed " +
          posUpdateStats.getCount(LABEL_ALL) + " ++ updates and " +
          negUpdateStats.getCount(LABEL_ALL) + " -- updates.");
      Log.info("Label distribution: " + epochLabels.toString());
    }

    // finalize learning: add the last vector to the avg for each label
    for(LabelWeights zw: zWeights) zw.addToAverage();
  }

  public void train(RvfMLDataset<String, String> dataset) {
    Log.info("Training the majority model using "
        + dataset.featureIndex().size() + " features and "
        + "the following labels: " + dataset.labelIndex().toString());

    labelIndex = dataset.labelIndex();
    nilIndex = labelIndex.indexOf(RelationMention.UNRELATED);
    zFeatureIndex = dataset.featureIndex();

    zWeights = new LabelWeights[labelIndex.size()];
    for(int i = 0; i < zWeights.length; i ++)
      zWeights[i] = new LabelWeights(dataset.featureIndex().size());

    // repeat for a number of epochs
    for(int t = 0; t < epochs; t ++){
      // randomize the data set in each epoch
      // use a fixed seed for replicability
      Log.info("Started epoch #" + t + "...");
      dataset.randomize(t);

      Counter<Integer> posUpdateStats = new ClassicCounter<Integer>();
      Counter<Integer> negUpdateStats = new ClassicCounter<Integer>();
      Counter<Integer> epochLabels = new ClassicCounter<Integer>();
      Map<Integer, Map<Integer, List<Double>>> instLabels = new HashMap<Integer, Map<Integer, List<Double>>>();
      for(int i = 0; i < dataset.labelIndex.size(); i++){
        for(int j = 0; j < dataset.labelIndex.size(); i++) {
          instLabels.getOrDefault(i, new HashMap<Integer, List<Double>>()).put(j, new ArrayList<Double>());
        }
      }

      // traverse the relation dataset
      for(int i = 0; i < dataset.size(); i ++){
        int [][] crtGroup = dataset.getDataArray()[i];
        double [][] crtGroupValues = dataset.getValueArray()[i];
        Set<Integer> gold = dataset.getLabelsArray()[i];

        trainJointly(crtGroup, crtGroupValues, gold, posUpdateStats, negUpdateStats, epochLabels, instLabels);

        // update the number of iterations an weight vector has survived
        for(LabelWeights zw: zWeights) zw.updateSurvivalIterations();
      }

      Log.info("Epoch #" + t + " completed. Inspected " +
          dataset.size() + " datum groups. Performed " +
          posUpdateStats.getCount(LABEL_ALL) + " ++ updates and " +
          negUpdateStats.getCount(LABEL_ALL) + " -- updates.");
      Log.info("Label distribution: " + epochLabels.toString());

      int nLabels = dataset.labelIndex.size();
      for(int i = 0; i < nLabels; i++){ // i: predicted class label
        Counter<String> lblInstDist = new ClassicCounter<>();
        Map<Integer, List<Double>> lblInstLbls = instLabels.get(i);
        for(Map.Entry<Integer, List<Double>> kv: lblInstLbls.entrySet()) {
          double avgProp = kv.getValue().stream().mapToDouble(k -> k).sum() / (double) lblInstLbls.size();
          lblInstDist.setCount(dataset.labelIndex.get(kv.getKey()), avgProp);
        }
        Log.info("Label distribution for accts predicted " + i + ": " + lblInstDist.toString());
      }
    }

    // finalize learning: add the last vector to the avg for each label
    for(LabelWeights zw: zWeights) zw.addToAverage();
  }

  private void trainJointly(
      int [][] crtGroup,
      Set<Integer> goldPos,
      Counter<Integer> posUpdateStats,
      Counter<Integer> negUpdateStats,
      Counter<Integer> epochLabels,
      Map<Integer, Map<Integer, List<Double>>> instLabels) {
    // all local predictions using local Z models
    List<Counter<Integer>> zs = estimateZ(crtGroup);
    // best predictions for each mention
    int [] zPredicted = generateZPredicted(zs);

    // yPredicted - Y labels predicted using the current Zs (full inference)
    Counter<Integer> yPredicted = estimateY(zPredicted);

    int y = yPredicted.keySet().iterator().next();
    // always update epochLabels (NB: assume only one label in set)
    epochLabels.incrementCount(y);

    // counter to find proportions of instance labels
    Counter<Integer> insts = new ClassicCounter<>();
    for(int pred: zPredicted){
      insts.incrementCount(pred);
    }
    for(int lbl: insts.keySet()){
      double prop = insts.getCount(lbl) / (double) zPredicted.length;
      instLabels.get(y).get(lbl).add(prop);
    }

    if(updateCondition(yPredicted.keySet(), goldPos)){
      // conditional inference
      Set<Integer> [] zUpdate = generateZUpdate(goldPos, zs);
      // update weights
      updateZModel(zUpdate, zPredicted, crtGroup, posUpdateStats, negUpdateStats);
    }
  }

  private void trainJointly(
      int [][] crtGroup,
      double [][] crtGroupValues,
      Set<Integer> goldPos,
      Counter<Integer> posUpdateStats,
      Counter<Integer> negUpdateStats,
      Counter<Integer> epochLabels,
      Map<Integer, Map<Integer, List<Double>>> instLabels) {
    // all local predictions using local Z models
    // this is simply generating *all* predictions for each tweet
    List<Counter<Integer>> zs = estimateZ(crtGroup, crtGroupValues);
    // best predictions for each instance
    int [] zPredicted = generateZPredicted(zs);

    // yPredicted - Y labels predicted using the current Zs (full inference)
    // this is picking the account label supported by most instances
    Counter<Integer> yPredicted = estimateY(zPredicted);

    int y = yPredicted.keySet().iterator().next();
    // always update epochLabels (NB: assume only one label in set)
    epochLabels.incrementCount(y);

    // counter to find proportions of instance labels
    Counter<Integer> insts = new ClassicCounter<>();
    for(int pred: zPredicted){
      insts.incrementCount(pred);
    }
    for(int lbl: insts.keySet()){
      double prop = insts.getCount(lbl) / (double) zPredicted.length;
      instLabels.get(y).get(lbl).add(prop);
    }

    // this is checking if the account label != gold (no need to change anything)
    if(updateCondition(yPredicted.keySet(), goldPos)){
      // conditional inference
      Set<Integer> [] zUpdate = generateZUpdate(goldPos, zs);
      // update weights
      updateZModel(zUpdate, zPredicted, crtGroup, posUpdateStats, negUpdateStats);
    }
  }

  private void updateZModel(
      Set<Integer> [] goldZ,
      int [] predictedZ,
      int [][] group,
      Counter<Integer> posUpdateStats,
      Counter<Integer> negUpdateStats) {
    assert(goldZ.length == group.length);
    assert(predictedZ.length == group.length);

    for(int i = 0; i < group.length; i ++) {
      // list of all possible gold labels for this mention (z)
      // for theoretical reasons this is a set, but in practice it will have a single value
      // for NIL labels, this set is *not* empty, it has _NR (RelationMention.UNRELATED)
      Set<Integer> gold = goldZ[i];
      int pred = predictedZ[i];
      int [] datum = group[i];

      // negative update
      if(! gold.contains(pred)) {
        zWeights[pred].update(datum, -1.0);
        negUpdateStats.incrementCount(pred);
        negUpdateStats.incrementCount(LABEL_ALL);
      }
      // positive update
      for(int l: gold) {
        if(l != pred) {
          zWeights[l].update(datum, +1.0);
          posUpdateStats.incrementCount(l);
          posUpdateStats.incrementCount(LABEL_ALL);
        }
      }
    }
  }

  private static boolean updateCondition(Set<Integer> y, Set<Integer> yPos) {
    if(y.size() != yPos.size()) return true;

    for(Integer l: yPos) {
      if(! y.contains(l)) {
        return true;
      }
    }

    return false;
  }

  /** Implements the edge cover problem in the conditional inference algorithm */
  static class Edge {
    int mention;
    int y;
    double score;
    Edge(int m, int y, double s) {
      this.mention = m;
      this.y = y;
      this.score = s;
    }
    public String toString() {
      return "(" + mention + ", " + y + ", " + score + ")";
    }
  }

  Map<Integer, List<Edge>> byY(List<Edge> edges) {
    Map<Integer, List<Edge>> edgesByY = new HashMap<Integer, List<Edge>>();
    for(Edge e: edges) {
      //if(e.y == nilIndex) continue; // nilIndex labels are allowed!
      List<Edge> yEdges = edgesByY.get(e.y);
      if(yEdges == null) {
        yEdges = new ArrayList<Edge>();
        edgesByY.put(e.y, yEdges);
      }
      yEdges.add(e);
    }
    for(Integer y: edgesByY.keySet()) {
      List<Edge> es = edgesByY.get(y);
      Collections.sort(es, new Comparator<Edge>() {
        @Override
        public int compare(Edge o1, Edge o2) {
          if(o1.score > o2.score) return -1;
          else if(o1.score == o2.score) return 0;
          return 1;
        }
      });
    }
    return edgesByY;
  }

  Map<Integer, List<Edge>> byZ(List<Edge> edges) {
    Map<Integer, List<Edge>> edgesByZ = new HashMap<Integer, List<Edge>>();
    for(Edge e: edges) {
      List<Edge> mentionEdges = edgesByZ.get(e.mention);
      if(mentionEdges == null) {
        mentionEdges = new ArrayList<Edge>();
        edgesByZ.put(e.mention, mentionEdges);
      }
      mentionEdges.add(e);
    }
    for(Integer m: edgesByZ.keySet()) {
      List<Edge> es = edgesByZ.get(m);
      Collections.sort(es, new Comparator<Edge>() {
        @Override
        public int compare(Edge o1, Edge o2) {
          if(o1.score > o2.score) return -1;
          else if(o1.score == o2.score) return 0;
          return 1;
        }
      });
    }
    return edgesByZ;
  }

  /** The conditional inference from (Hoffmann et al., 2011) */
  private Set<Integer> [] generateZUpdate(
      Set<Integer> goldPos,
      List<Counter<Integer>> zs) {
    Set<Integer> [] zUpdate = ErasureUtils.uncheckedCast(new Set[zs.size()]);
    for(int i = 0; i < zUpdate.length; i ++)
      zUpdate[i] = new HashSet<Integer>();

    // build all edges, for NIL + gold labels
    // this is the graph in Hoffman Fig 3; it is exhaustive: from each mention to all labels!
    List<Edge> edges = new ArrayList<Edge>();
    for(int m = 0; m < zs.size(); m ++) {
      for(Integer y: zs.get(m).keySet()) {
        if(goldPos.contains(y) || y == nilIndex) {
          double s = zs.get(m).getCount(y);
          edges.add(new Edge(m, y, s));
        }
      }
    }

    // assume there are more mentions than relations
    // for each Y, pick the highest edge(s) from an unmapped mention
    // this is where we flip the ones that are most easily flippable to the gold label
    Map<Integer, List<Edge>> edgesByY = byY(edges);
    for(Integer y: goldPos) {
      List<Edge> es = edgesByY.get(y);
      assert(es != null);
      //System.out.print(String.valueOf(es.size()) + "\n");
      int flipThreshold = howManyToFlip(es, y);
      int flipped = 0;
      for(Edge e: es) {
        if(flipped >= flipThreshold) {
          break; // this means that the condition is satisfied
        }
        if(zUpdate[e.mention].size() == 0) {
          zUpdate[e.mention].add(e.y);
          flipped++;
        }
      }
    }

    // map the leftover mentions to their highest scoring Y
    Map<Integer, List<Edge>> edgesByZ = byZ(edges);
    for(int m = 0; m < zUpdate.length; m ++) {
      if(zUpdate[m].size() == 0) {
        List<Edge> es = edgesByZ.get(m);
        assert(es != null);
        assert(es.size() > 0);
        zUpdate[m].add(es.get(0).y); // allow NILs as well
      }
    }

    return zUpdate;
  }

  /**
   * Determine how many labels to flip according to proportion weighed against the other labels.
   */
  private int howManyToFlip(List<Edge> edges, int y) {
    if (edges.size() == 0) return 0;
    Map<Integer, List<Edge>> edgesByZ = byZ(edges);
    double minimumGolds = 0.1;
    double majorityThreshold = 0.5;
    double golds = 0.0;
    double nils = 0.0;
    double others = 0.0;
    for(int m = 0; m < edgesByZ.keySet().size(); m ++) {
      List<Edge> es = edgesByZ.get(m);
      assert(es != null);
      assert(es.size() > 0);
      if(es.get(0).y == y)
        golds++;
      else if(es.get(0).y == nilIndex)
        nils++;
      else others++;
    }

    double total = golds + nils + others;
    double toMajority = Math.ceil(majorityThreshold * (golds + others));
    // gold is _NF or golds are more than 10% of the total labels -- flip the greatest among:
    // 0
    // # needed to exceed other (non-nil) label
    if(y == nilIndex || golds > 0.10 * total)
      return (int) Math.max(0.0, toMajority);
      // too few golds -- flip the greatest number among:
      // 1
      // # needed to get to minimumGolds proportion
      // # needed to exceed other (non-nil) label
    else {
      double toMinimum = Math.round(minimumGolds * total) - golds;
      return (int) Math.max(1.0, Math.max(toMinimum, toMajority));
    }
  }

  /**
   * Estimate Y labels given a list of Z predictions for this tuple
   * This is done using a deterministic OR
   */
  private Counter<Integer> estimateY(int [] zPredicted) {
    Counter<Integer> ys = new ClassicCounter<Integer>();
    for(int zp: zPredicted) {
      ys.setCount(zp, 1);
    }
    return ys;
  }

  private List<Counter<Integer>> estimateZ(int [][] datums) {
    List<Counter<Integer>> zs = new ArrayList<Counter<Integer>>();
    for(int [] datum: datums) {
      zs.add(estimateZ(datum));
    }
    return zs;
  }

  private Counter<Integer> estimateZ(int [] datum) {
    Counter<Integer> vector = new ClassicCounter<Integer>();
    for(int d: datum) vector.incrementCount(d);

    Counter<Integer> scores = new ClassicCounter<Integer>();
    for(int label = 0; label < zWeights.length; label ++){
      double score = zWeights[label].dotProduct(vector);
      scores.setCount(label, score);
    }

    return scores;
  }

  private List<Counter<Integer>> estimateZ(int [][] datums, double [][] values) {
    List<Counter<Integer>> zs = new ArrayList<Counter<Integer>>();
    for(int i = 0; i < datums.length; i++) {
      zs.add(estimateZ(datums[i], values[i]));
    }
    return zs;
  }

  private Counter<Integer> estimateZ(int [] datum, double [] value) {
    Counter<Integer> vector = new ClassicCounter<Integer>();
    for(int i = 0; i < datum.length; i++) {
      vector.incrementCount(datum[i], value[i]);
    }

    Counter<Integer> scores = new ClassicCounter<Integer>();
    for(int label = 0; label < zWeights.length; label ++){
      double score = zWeights[label].dotProduct(vector);
      scores.setCount(label, score);
    }

    return scores;
  }

  private int [] generateZPredicted(List<Counter<Integer>> zs) {
    int [] bestZs = new int[zs.size()];

    for(int i = 0; i < zs.size(); i ++) {
      Counter<Integer> cands = zs.get(i);
      int bestZ = nilIndex;
      if(cands.size() > 0){
        bestZ = pickBestLabel(cands);
      }
      bestZs[i] = bestZ;
    }

    return bestZs;
  }

  private static int pickBestLabel(Counter<Integer> scores) {
    assert(scores.size() > 0);
    List<Pair<Integer, Double>> sortedScores = sortIntPredictions(scores);
    return sortedScores.iterator().next().first();
  }

  private static List<Pair<Integer, Double>> sortIntPredictions(Counter<Integer> scores) {
    List<Pair<Integer, Double>> sortedScores = new ArrayList<Pair<Integer,Double>>();
    for(Integer key: scores.keySet()) {
      sortedScores.add(new Pair<Integer, Double>(key, scores.getCount(key)));
    }
    sortIntPredictions(sortedScores);
    return sortedScores;
  }

  private static void sortIntPredictions(List<Pair<Integer, Double>> scores) {
    Collections.sort(scores, new Comparator<Pair<Integer, Double>>() {
      @Override
      public int compare(Pair<Integer, Double> o1, Pair<Integer, Double> o2) {
        if(o1.second() > o2.second()) return -1;
        if(o1.second() < o2.second()) return 1;

        // this is an arbitrary decision to disambiguate ties
        if(o1.first() > o2.first()) return -1;
        if(o1.first() < o2.first()) return 1;
        return 0;
      }
    });
  }

  public static List<Pair<String, Double>> sortStringPredictions(Counter<String> scores) {
    List<Pair<String, Double>> sortedScores = new ArrayList<Pair<String,Double>>();
    for(String key: scores.keySet()) {
      sortedScores.add(new Pair<String, Double>(key, scores.getCount(key)));
    }
    sortStringPredictions(sortedScores);
    return sortedScores;
  }

  private static void sortStringPredictions(List<Pair<String, Double>> scores) {
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

  @Override
  public Counter<String> classifyMentions(List<Collection<String>> mentions) {
    Counter<String> bestZScores = new ClassicCounter<String>();

    // traverse of all mention of this tuple
    for (int i = 0; i < mentions.size(); i++) {
      // get all scores for this mention
      Collection<String> mentionFeatures = mentions.get(i);
      Counter<String> mentionScores = classifyMention(mentionFeatures);

      Pair<String, Double> topPrediction = sortStringPredictions(mentionScores).get(0);
      String l = topPrediction.first();
      double s = topPrediction.second();

      // update the best score for this label if necessary
      // exclude the NIL label from this; it is not propagated in the Y layer
      if(! l.equals(RelationMention.UNRELATED) &&
          (! bestZScores.containsKey(l) || bestZScores.getCount(l) < s)) {
        bestZScores.setCount(l, s);
      }
    }

    // generate the predictions of the Y layer using deterministic OR
    // the score of each Y label is the best mention-level score
    return bestZScores;
  }

  private Counter<String> classifyMention(Collection<String> testDatum) {
    Counter<String> scores = new ClassicCounter<String>();
    for(int labelIdx = 0; labelIdx < zWeights.length; labelIdx ++){
      double score = zWeights[labelIdx].avgDotProduct(testDatum, zFeatureIndex);
      scores.setCount(labelIndex.get(labelIdx), score);
    }
    return scores;
  }

  private Counter<Integer> softmaxInstance(Counter<Integer> inst) {
    Counter<Integer> exp = new ClassicCounter<Integer>();
    Iterator<Map.Entry<Integer, Double>> instEntries = inst.entrySet().iterator();
    while(instEntries.hasNext()) {
      Map.Entry<Integer, Double> kv = instEntries.next();
      Double exponentiated = java.lang.Math.exp(kv.getValue());
      exp.setCount(kv.getKey(), exponentiated);
    }
    Double allE = exp.values().stream().reduce(0.0, (x,y) -> x + y);
    Iterator<Map.Entry<Integer, Double>> expEntries = exp.entrySet().iterator();
    while(expEntries.hasNext()) {
      Map.Entry<Integer, Double> kv = expEntries.next();
      exp.setCount(kv.getKey(), kv.getValue() / allE);
    }
    return exp;
  }

  private List<Counter<Integer>> softmaxInstance(List<Counter<Integer>> inst) {
    ArrayList<Counter<Integer>> exp = new ArrayList<>(inst.size());
    for(int i = 0; i < inst.size(); i++){
      exp.add(softmaxInstance(inst.get(i)));
    }
    return exp;
  }

  private Counter<Integer> softmaxAccount(List<Counter<Integer>> inst, int numKeys) {
    Counter<Integer> sm = new ClassicCounter<>(numKeys);
    for(int i = 0; i < numKeys; i++) {
      double prob = 1.0;
      for(int j = 0; j < inst.size(); j++) {
        prob *= 1 - inst.get(j).getCount(i);
      }
      sm.setCount(i, 1 - prob);
    }
    return sm;
  }

  public List<Counter<Integer>> classifyAccounts(RvfMLDataset<String, String> dataset) {
    List<Counter<Integer>> predictedLabels = new ArrayList<Counter<Integer>>();
    for(int i = 0; i < dataset.size(); i++) {
      int[][] rowFeatures = dataset.getDataArray()[i];
      double[][] rowValues = dataset.getValueArray()[i];
      List<Counter<Integer>> zs = estimateZ(rowFeatures, rowValues);
      Counter<Integer> sm = softmaxAccount(zs, dataset.labelIndex.size());

      Counter<Integer> iLabel = new ClassicCounter<>();
      Map.Entry<Integer, Double> bestLabel = null;
      for(Map.Entry<Integer, Double> entry: sm.entrySet()) {
        if (bestLabel == null || entry.getValue() > bestLabel.getValue())
          bestLabel = entry;
      }

      iLabel.incrementCount(bestLabel.getKey());
      predictedLabels.add(iLabel);
    }
    logger.info(predictedLabels.size() + " labels predicted\n");
    Counter<Integer> allPredicted = new ClassicCounter<Integer>(predictedLabels.size());
    for(int i = 0; i < predictedLabels.size(); i++) {
      allPredicted.incrementCount(predictedLabels.get(i).keySet().iterator().next());
    }
    logger.info("label distribution: " + allPredicted.toString());

    return predictedLabels;
  }

  public Triple<Double, Double, Double> test(RvfMLDataset<String, String> dataset) {
    Set<Integer>[] goldLabels = dataset.getLabelsArray();
    List<Counter<Integer>> predictedLabels = classifyAccounts(dataset);

    return score(goldLabels, predictedLabels);
  }

  public static Triple<Double, Double, Double> score(
      Set<Integer>[] goldLabels,
      List<Counter<Integer>> predictedLabels) {
    assert(goldLabels.length == predictedLabels.size());
    if(goldLabels.length == 0)
      logger.warning("Trying to evaluate on 0 datums!");
    double total = 0.0, predicted = 0.0, correct = 0.0;
    for(int i = 0; i < goldLabels.length; i ++) {
      Set<Integer> gold = goldLabels[i];
      Counter<Integer> preds = predictedLabels.get(i);
      total += gold.size();
      predicted += preds.size();
      for(Integer label: preds.keySet()) {
        if(gold.contains(label)) correct ++;
      }
    }

    double p = (predicted != 0 ? correct / predicted : 0.0);
    double r = (total != 0 ? correct / total : 0.0);
    double f1 = (p != 0 && r != 0 ? 2*p*r/(p+r) : 0);
    System.out.print("p: " + p + ", r: " + r + ", f1: " + f1 + "\n");
    return new Triple<Double, Double, Double>(p, r, f1);
  }

  @Override
  public void save(String modelPath) throws IOException {
    // make sure the modelpath directory exists
    int lastSlash = modelPath.lastIndexOf(File.separator);
    if(lastSlash > 0){
      String path = modelPath.substring(0, lastSlash);
      File f = new File(path);
      if (! f.exists()) {
        f.mkdirs();
      }
    }

    for(LabelWeights zw: zWeights) {
      zw.clear();
    }

    FileOutputStream fos = new FileOutputStream(modelPath);
    ObjectOutputStream out = new ObjectOutputStream(fos);

    assert(zWeights != null);
    out.writeInt(zWeights.length);
    for(LabelWeights zw: zWeights) {
      out.writeObject(zw);
    }

    out.writeObject(labelIndex);
    out.writeObject(zFeatureIndex);

    out.close();
  }

  @Override
  public void load(ObjectInputStream in) throws IOException, ClassNotFoundException {
    int length = in.readInt();
    zWeights = new LabelWeights[length];
    for(int i = 0; i < zWeights.length; i ++){
      zWeights[i] = ErasureUtils.uncheckedCast(in.readObject());
    }

    labelIndex = ErasureUtils.uncheckedCast(in.readObject());
    nilIndex = labelIndex.indexOf(RelationMention.UNRELATED);
    zFeatureIndex = ErasureUtils.uncheckedCast(in.readObject());
  }

  /*
  public static RelationExtractor load(String modelPath, Properties props) throws IOException, ClassNotFoundException {
    InputStream is = new FileInputStream(modelPath);
    ObjectInputStream in = new ObjectInputStream(is);
    HoffmannExtractor ex = new HoffmannExtractor(props);
    ex.load(in);
    in.close();
    is.close();
    return ex;
  }
  */
}
