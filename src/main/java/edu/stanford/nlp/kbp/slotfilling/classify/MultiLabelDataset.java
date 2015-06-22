package edu.stanford.nlp.kbp.slotfilling.classify;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import edu.stanford.nlp.ling.BasicDatum;
import edu.stanford.nlp.ling.Datum;
import edu.stanford.nlp.ling.RVFDatum;
import edu.stanford.nlp.stats.ClassicCounter;
import edu.stanford.nlp.stats.Counter;
import edu.stanford.nlp.util.HashIndex;
import edu.stanford.nlp.util.Index;

public class MultiLabelDataset<L, F> implements Serializable {
  private static final long serialVersionUID = 1L;
  
  public Index<L> labelIndex;
  public Index<F> featureIndex;

  /** Stores the list of known positive labels for each datum group */
  protected Set<Integer> [] posLabels;
  /** Stores the list of known negative labels for each datum group */
  protected Set<Integer> [] negLabels;
  /** Stores the datum groups, where each group consists of a collection of datums */
  protected Datum<L,F>[][] data;
  protected int size;

  public MultiLabelDataset() {
    this(10);
  }
  
  public MultiLabelDataset(int sz) {
    initialize(sz);
  }
  
  public MultiLabelDataset(Datum<L,F>[][] data,
      Index<F> featureIndex,
      Index<L> labelIndex,
      Set<Integer> [] posLabels,
      Set<Integer> [] negLabels) {
    this.data = data;
    this.featureIndex = featureIndex;
    this.labelIndex = labelIndex;
    this.posLabels = posLabels;
    this.negLabels = negLabels;
    this.size = data.length;
  }
  
  @SuppressWarnings("unchecked")
  protected void initialize(int numDatums) {
    labelIndex = new HashIndex<L>();
    featureIndex = new HashIndex<F>();
    posLabels = new Set[numDatums];
    negLabels = new Set[numDatums];
    data = new Datum[numDatums][];
    size = 0;
  }
  
  public int size() { return size; }
  
  public Index<L> labelIndex() { return labelIndex; }

  public Index<F> featureIndex() { return featureIndex; }

  public int numFeatures() { return featureIndex.size(); }

  public int numClasses() { return labelIndex.size(); }
  
  public Set<Integer> [] getPositiveLabelsArray() {
    posLabels = trimToSize(posLabels);
    return posLabels;
  }
  
  public Set<Integer> [] getNegativeLabelsArray() {
    negLabels = trimToSize(negLabels);
    return negLabels;
  }

  public Datum<L,F>[][] getDataArray() {
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

  @SuppressWarnings("unchecked")
  protected Datum<L,F>[][] trimToSize(Datum<L,F>[][] i) {
    if(i.length == size) return i;
    Datum<L,F>[][] newI = new Datum[size][];
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
      
      Datum<L,F>[] tmp = data[randIndex];
      data[randIndex] = data[j];
      data[j] = tmp;
      
      Set<Integer> tmpl = posLabels[randIndex];
      posLabels[randIndex] = posLabels[j];
      posLabels[j] = tmpl;
      
      tmpl = negLabels[randIndex];
      negLabels[randIndex] = negLabels[j];
      negLabels[j] = tmpl;
      
    }
  }
  
  public void randomize(int [][] zLabels, int randomSeed) {
    Random rand = new Random(randomSeed);
    for(int j = size - 1; j > 0; j --){
      int randIndex = rand.nextInt(j);
      
      Datum<L,F>[] tmp = data[randIndex];
      data[randIndex] = data[j];
      data[j] = tmp;
      
      Set<Integer> tmpl = posLabels[randIndex];
      posLabels[randIndex] = posLabels[j];
      posLabels[j] = tmpl;
      
      tmpl = negLabels[randIndex];
      negLabels[randIndex] = negLabels[j];
      negLabels[j] = tmpl;
      
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
        Datum<L, F> datum = data[i][j];
        for (F feature: datum.asFeatures()) {
          if(datum instanceof RVFDatum<?, ?>)
            counts[featureIndex.indexOf(feature)] += ((RVFDatum<L, F>) datum).getFeatureCount(feature);
          else
            counts[featureIndex.indexOf(feature)] += 1.0;
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
        Datum<L, F> oldDatum = data[i][j];
        Counter<F> featureCounter = new ClassicCounter<F>();
        for (F feature: oldDatum.asFeatures()) {
          if (featMap[featureIndex.indexOf(feature)] >= 0) {
            if (oldDatum instanceof RVFDatum<?, ?>) {
              featureCounter.setCount(feature, ((RVFDatum) oldDatum).getFeatureCount(feature));
            } else {
              featureCounter.setCount(feature, 1.0); // just place it in, we'll dump features later
            }
          }
        }
        Datum<L, F> newDatum;
        if (oldDatum instanceof RVFDatum<?, ?>) {
          newDatum = new RVFDatum<L, F>(featureCounter);
        } else {
          newDatum = new BasicDatum<L, F>(featureCounter.keySet());
        }
        data[i][j] = newDatum;
      }
    }
  }
  
  public void addDatum(Set<L> yPos, Set<L> yNeg, List<Datum<L, F>> group) {
    ensureSize();

    addPosLabels(yPos);
    addNegLabels(yNeg);
    addGroup(group);

    size ++;
  }

  protected void addGroup(List<Datum<L, F>> group) {
    Datum<L,F>[] groupFeatures = new Datum[group.size()];
    int datumIndex = 0;
    for(Datum<?, F> datum: group){
      Counter<F> featureCounter = new ClassicCounter<F>();

      for (F feature: datum.asFeatures()) {
        featureIndex.add(feature);
        if (featureIndex.indexOf(feature) >= 0) {
          if (datum instanceof RVFDatum<?, ?>) {
            featureCounter.setCount(feature, ((RVFDatum) datum).getFeatureCount(feature));
          } else {
            featureCounter.setCount(feature, 1.0); // just place it in, we'll dump features later
          }
        }
      }
      Datum<L, F> newDatum;
      if (datum instanceof RVFDatum<?, ?>) {
        newDatum = new RVFDatum<L, F>(featureCounter);
      } else {
        newDatum = new BasicDatum<L, F>(featureCounter.keySet());
      }

      groupFeatures[datumIndex] = newDatum;
      datumIndex ++;
    }
    assert(datumIndex == group.size());
    data[size] = groupFeatures;
  }
  
  protected void addPosLabels(Set<L> labels) {
    labelIndex.addAll(labels);
    Set<Integer> newLabels = new HashSet<Integer>();
    for(L l: labels) {
      newLabels.add(labelIndex.indexOf(l));
    }
    posLabels[size] = newLabels;
  }
  
  protected void addNegLabels(Set<L> labels) {
    labelIndex.addAll(labels);
    Set<Integer> newLabels = new HashSet<Integer>();
    for(L l: labels) {
      newLabels.add(labelIndex.indexOf(l));
    }
    negLabels[size] = newLabels;
  }
  
  @SuppressWarnings("unchecked")
  protected void ensureSize() {
    if (posLabels.length == size) {
      Set<Integer> [] newLabels = new Set[size * 2];
      System.arraycopy(posLabels, 0, newLabels, 0, size);
      posLabels = newLabels;
      
      newLabels = new Set[size * 2];
      System.arraycopy(negLabels, 0, newLabels, 0, size);
      negLabels = newLabels;
      
      Datum<L,F>[][] newData = new Datum[size * 2][];
      System.arraycopy(data, 0, newData, 0, size);
      data = newData;      
    }
  }
}
