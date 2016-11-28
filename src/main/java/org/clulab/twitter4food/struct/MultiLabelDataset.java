package org.clulab.twitter4food.struct;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import edu.stanford.nlp.ling.Datum;
import edu.stanford.nlp.util.HashIndex;
import edu.stanford.nlp.util.Index;

public class MultiLabelDataset<L, F> implements Serializable {
  private static final long serialVersionUID = 1L;
  
  public Index<L> labelIndex;
  public Index<F> featureIndex;

  /** Stores the list of known positive labels for each datum group */
  protected Set<Integer> [] labels;
  /** Stores the datum groups, where each group consists of a collection of datums */
  protected int[][][] data;
  protected int size;
  
  public MultiLabelDataset() {
    this(10);
  }
  
  public MultiLabelDataset(int sz) {
    initialize(sz);
  }
  
  public MultiLabelDataset(int[][][] data,
      Index<F> featureIndex,
      Index<L> labelIndex,
      Set<Integer> [] labels) {
    this.data = data;
    this.featureIndex = featureIndex;
    this.labelIndex = labelIndex;
    this.labels = labels;
    this.size = data.length;
  }
  
  @SuppressWarnings("unchecked")
  protected void initialize(int numDatums) {
    labelIndex = new HashIndex<L>();
    featureIndex = new HashIndex<F>();
    labels = new Set[numDatums];
    data = new int[numDatums][][];
    size = 0;
  }
  
  public int size() { return size; }
  
  public Index<L> labelIndex() { return labelIndex; }

  public Index<F> featureIndex() { return featureIndex; }

  public int numFeatures() { return featureIndex.size(); }

  public int numClasses() { return labelIndex.size(); }
  
  public Set<Integer> [] getLabelsArray() {
    labels = trimToSize(labels);
    return labels;
  }
  
  public int[][][] getDataArray() {
    data = trimToSize(data);
    return data;
  }
  
  @SuppressWarnings("unchecked")
  protected Set<Integer> [] trimToSize(Set<Integer> [] i) {
    if(i.length == size) return i;
    Set<Integer> [] newI = new Set[size];
    System.arraycopy(i, 0, newI, 0, size);
    return newI;
  }
  
  protected int[][][] trimToSize(int[][][] i) {
    if(i.length == size) return i;
    int[][][] newI = new int[size][][];
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
      
      Set<Integer> tmpl = labels[randIndex];
      labels[randIndex] = labels[j];
      labels[j] = tmpl;
    }
  }
  
  public void randomize(int [][] zLabels, int randomSeed) {
    Random rand = new Random(randomSeed);
    for(int j = size - 1; j > 0; j --){
      int randIndex = rand.nextInt(j);
      
      int [][] tmp = data[randIndex];
      data[randIndex] = data[j];
      data[j] = tmp;
      
      Set<Integer> tmpl = labels[randIndex];
      labels[randIndex] = labels[j];
      labels[j] = tmpl;
      
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
          counts[data[i][j][k]] += 1.0;
        }
      }
    }
    return counts;
  }

  /**
   * Applies a feature count threshold to the Dataset.
   * All features that occur fewer than <i>k</i> times are expunged.
   */
  public void applyFeatureCountThreshold(int threshold) {
    float[] counts = getFeatureCounts();
    
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
        List<Integer> featList = new ArrayList<Integer>(data[i][j].length);
        for (int k = 0; k < data[i][j].length; k++) {
          if (featMap[data[i][j][k]] >= 0) {
            featList.add(featMap[data[i][j][k]]);
          }
        }
        data[i][j] = new int[featList.size()];
        for(int k = 0; k < data[i][j].length; k ++) {
          data[i][j][k] = featList.get(k);
        }
      }
    }
  }
  
  public void addDatum(Set<L> yLabels, List<Datum<L, F>> group) {
    List<Collection<F>> features = new ArrayList<Collection<F>>();
    for(Datum<L, F> datum: group){
      features.add(datum.asFeatures());
    }
    add(yLabels, features);
  }
  
  public void add(Set<L> yLabels, List<Collection<F>> group) {
    ensureSize();
    
    addLabels(yLabels);
    addFeatures(group);
    
    size ++;
  }

  protected void addLabels(Set<L> lbls) {
    labelIndex.addAll(lbls);
    Set<Integer> newLabels = new HashSet<Integer>();
    for(L l: lbls) {
      newLabels.add(labelIndex.indexOf(l));
    }
    labels[size] = newLabels;
  }
  
  protected void addFeatures(List<Collection<F>> group) {
    int [][] groupFeatures = new int[group.size()][];
    int datumIndex = 0;
    for(Collection<F> features: group){
      int[] intFeatures = new int[features.size()];
      int j = 0;
      for (F feature : features) {
        featureIndex.add(feature);
        int index = featureIndex.indexOf(feature);
        if (index >= 0) {
          intFeatures[j] = featureIndex.indexOf(feature);
          j++;
        }
      }

      int [] trimmedFeatures = new int[j];
      System.arraycopy(intFeatures, 0, trimmedFeatures, 0, j);
      groupFeatures[datumIndex] = trimmedFeatures;
      datumIndex ++;
    }
    assert(datumIndex == group.size());
    data[size] = groupFeatures;
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
    }
  }
}

