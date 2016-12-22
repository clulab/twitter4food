package org.clulab.twitter4food.struct;

import org.clulab.twitter4food.miml.Log;
import edu.stanford.nlp.util.HashIndex;
import edu.stanford.nlp.util.Index;

import java.util.*;

public class RvfMLDataset<L, F> extends MultiLabelDataset<L, F> {
  protected double[][][] values;
  protected String [][] tweets;

  public RvfMLDataset() {
    this(10);
  }

  public RvfMLDataset(int sz) {
    initialize(sz);
  }

  public RvfMLDataset(int[][][] data,
                      double[][][] values,
                      String [][] tweets,
                      Index<F> featureIndex,
                      Index<L> labelIndex,
                      Set<Integer>[] labels) {
    this.data = data;
    this.values = values;
    this.tweets = tweets;
    this.featureIndex = featureIndex;
    this.labelIndex = labelIndex;
    this.labels = labels;
    this.size = data.length;
  }

  protected void initialize(int numDatums) {
    labelIndex = new HashIndex<L>();
    featureIndex = new HashIndex<F>();
    labels = new Set[numDatums];
    data = new int[numDatums][][];
    values = new double[numDatums][][];
    size = 0;
  }

  public double[][][] getValueArray() {
    values = trimToSize(values);
    return values;
  }

  public String [][] getTweets() {
    tweets = trimToSize(tweets);
    return tweets;
  }

  protected double[][][] trimToSize(double[][][] i) {
    if(i.length == size) return i;
    double[][][] newI = new double[size][][];
    System.arraycopy(i, 0, newI, 0, size);
    return newI;
  }

  protected String [][] trimToSize(String [][] i) {
    if(i.length == size) return i;
    String [][] newI = new String [size][];
    System.arraycopy(i, 0, newI, 0, size);
    return newI;
  }

  /**
   * Randomizes the data array in place
   * @param randomSeed
   */
  public void randomize(int randomSeed) {
    Random rand = new Random(randomSeed);
    for(int j = size - 1; j > 0; j --){
      int randIndex = rand.nextInt(j);

      int [][] tmp = data[randIndex];
      data[randIndex] = data[j];
      data[j] = tmp;

      double [][] tmpv = values[randIndex];
      values[randIndex] = values[j];
      values[j] = tmpv;

      Set<Integer> tmpl = labels[randIndex];
      labels[randIndex] = labels[j];
      labels[j] = tmpl;

      String [] tmpt = tweets[randIndex];
      tweets[randIndex] = tweets[j];
      tweets[j] = tmpt;
    }
  }

  public void randomize(int [][] zLabels, int randomSeed) {
    Random rand = new Random(randomSeed);
    for(int j = size - 1; j > 0; j --){
      int randIndex = rand.nextInt(j);

      int [][] tmp = data[randIndex];
      data[randIndex] = data[j];
      data[j] = tmp;

      double [][] tmpv = values[randIndex];
      values[randIndex] = values[j];
      values[j] = tmpv;

      Set<Integer> tmpl = labels[randIndex];
      labels[randIndex] = labels[j];
      labels[j] = tmpl;

      String [] tmpt = tweets[randIndex];
      tweets[randIndex] = tweets[j];
      tweets[j] = tmpt;

      int [] tmpz = zLabels[randIndex];
      zLabels[randIndex] = zLabels[j];
      zLabels[j] = tmpz;
    }
  }

  /**
   * Get the total count (over all data instances) of each feature
   *
   * @return an array containing the counts (indexed by index)
   */
  public float[] getFeatureCounts() {
    float[] counts = new float[featureIndex.size()];
    for (int i = 0; i < size; i++) {
      for (int j = 0; j < data[i].length; j++) {
        for(int k = 0; k < data[i][j].length; k ++) {
          counts[data[i][j][k]] += values[i][j][k];
        }
      }
    }
    return counts;
  }

  /**
   * Applies a feature count threshold to the Dataset.
   * All newFeatures that occur fewer than <i>k</i> times are expunged.
   */
  public void applyFeatureCountThreshold(double threshold) {
    float[] counts = getFeatureCounts();

    Log.info("# features before thresholding: " + featureIndex.size());
    //
    // rebuild the feature index
    //
    Index<F> newFeatureIndex = new HashIndex<F>();
    int[] featMap = new int[featureIndex.size()];
    for (int i = 0; i < featMap.length; i++) {
      F feat = featureIndex.get(i);
      if (counts[i] >= threshold) {
        int newIndex = newFeatureIndex.size();
        newFeatureIndex.add(feat);
        featMap[i] = newIndex;
      } else {
        featMap[i] = -1;
      }
    }

    featureIndex = newFeatureIndex;

    //
    // rebuild the data
    //
    for (int i = 0; i < size; i++) {
      for(int j = 0; j < data[i].length; j ++){
        List<Integer> featList = new ArrayList<>(data[i][j].length);
        List<Double> valueList = new ArrayList<>(values[i][j].length);
        for (int k = 0; k < data[i][j].length; k++) {
          if (featMap[data[i][j][k]] >= 0) {
            featList.add(featMap[data[i][j][k]]);
            valueList.add(values[i][j][k]);
          }
        }
        data[i][j] = featList.stream().mapToInt(x -> x).toArray();
        values[i][j] = valueList.stream().mapToDouble(x -> x).toArray();
      }
    }
    Log.info("# features after thresholding: " + featureIndex.size());
  }

  public void add(Set<L> y, List<List<F>> newFeatures, List<List<Double>> newValues, List<String> newTweets) {
    ensureSize();

    addLabels(y);
    addFeatures(newFeatures, newValues, newTweets);
    size ++;
  }

  protected void addFeatures(List<List<F>> newFeatures, List<List<Double>> newValues, List<String> newTweets) {
    int [][] datumFeatures = new int[newFeatures.size()][];
    double [][] datumValues = new double[newValues.size()][];
    String [] datumTweets = new String[newTweets.size()];
    int i = 0;
    int totalDatums = newFeatures.size();
    while(i < totalDatums){
      List<F> ifeats = newFeatures.get(i);
      List<Double> ivals = newValues.get(i);
      int[] iFeatures = new int[ifeats.size()];
      double[] iValues = new double[ivals.size()];
      int j = 0; // source index
      int k = 0; // target index
      while (j < ifeats.size()) {
        F f = ifeats.get(j);
        featureIndex.add(f);
        int index = featureIndex.indexOf(f);
        if (index >= 0) {
          iFeatures[k] = featureIndex.indexOf(f);
          iValues[k] = ivals.get(j);
          k++;
        }
        j++;
      }

      int [] trimmedFeatures = new int[j];
      System.arraycopy(iFeatures, 0, trimmedFeatures, 0, j);
      datumFeatures[i] = trimmedFeatures;
      double [] trimmedValues = new double[j];
      System.arraycopy(iValues, 0, trimmedValues, 0, j);
      datumValues[i] = trimmedValues;

      datumTweets[i] = newTweets.get(i);

      i ++;
    }
    assert(i == newFeatures.size());
    data[size] = datumFeatures;
    values[size] = datumValues;
    tweets[size] = datumTweets;
    // System.out.print("size " + String.valueOf(size) + " of " + String.valueOf(Array.getLength(values)) + "\n");
    ensureSize();
  }

  @SuppressWarnings("unchecked")
  protected void ensureSize() {
    if (labels.length == size) {
      Set<Integer> [] newLabels = new Set[size * 2];
      System.arraycopy(labels, 0, newLabels, 0, size);
      labels = newLabels;

      int[][][] newData = new int[size * 2][][];
      System.arraycopy(data, 0, newData, 0, size);
      data = newData;

      double[][][] newValues = new double[size * 2][][];
      System.arraycopy(values, 0, newValues, 0, size);
      values = newValues;
    }
  }

  public RvfMLDataset<L, F> copy() {
    return new RvfMLDataset<L, F>(this.getDataArray().clone(),
        this.getValueArray().clone(),
        this.getTweets().clone(),
        cloneFIndex(featureIndex),
        cloneLIndex(labelIndex),
        labels.clone());
  }

  private Index<L> cloneLIndex(Index<L> from) {
    return new HashIndex<L>(from);
  }

  private Index<F> cloneFIndex(Index<F> from) {
    return new HashIndex<F>(from);
  }
}
