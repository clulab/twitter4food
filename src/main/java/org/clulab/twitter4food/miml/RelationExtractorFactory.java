package org.clulab.twitter4food.miml;

import java.io.IOException;
import java.util.Properties;

public class RelationExtractorFactory {
  ModelType modelType;
  
  public RelationExtractorFactory(String type) {
    this(ModelType.stringToModel(type));
  }
  
  public RelationExtractorFactory(ModelType mt) {
    modelType = mt;
  }
  
  public ModelType modelType() { return modelType; }
  
  public JointlyTrainedRelationExtractor makeJointExtractor(Properties props) throws IOException {
    JointlyTrainedRelationExtractor extractor = null;
    if(modelType == ModelType.LOCAL_BAYES) {
      extractor = new JointBayesRelationExtractor(props, true);
    } else if(modelType == ModelType.JOINT_BAYES) {
      extractor = new JointBayesRelationExtractor(props);
    } else if(modelType == ModelType.AT_LEAST_ONCE) {
      extractor = new HoffmannExtractor(props);
    } else if(modelType == ModelType.AT_LEAST_ONCE_INC ||
        modelType == ModelType.PERCEPTRON ||
        modelType == ModelType.PERCEPTRON_INC) {
      extractor = new PerceptronExtractor(props);
    } else {
      throw new RuntimeException("ERROR: Invalid extractor in the joint framework: " + modelType);
    }
    return extractor;
  }
  
  public RelationExtractor load(String modelPath, Properties props) throws IOException, ClassNotFoundException {
    RelationExtractor relationExtractor = null;
    if (modelType == ModelType.LR_INC) {
      relationExtractor = OneVsAllRelationExtractor.load(modelPath);
    } else if (modelType == ModelType.AT_LEAST_ONCE) {
      relationExtractor = HoffmannExtractor.load(modelPath, props);
    } else if(modelType == ModelType.AT_LEAST_ONCE_INC
        || modelType == ModelType.PERCEPTRON 
        || modelType == ModelType.PERCEPTRON_INC) {
      relationExtractor = PerceptronExtractor.load(modelPath, props);
    } else if(modelType == ModelType.JOINT_BAYES) {
      relationExtractor = JointBayesRelationExtractor.load(modelPath, props);
    } else if(modelType == ModelType.LOCAL_BAYES) {
      relationExtractor = JointBayesRelationExtractor.load(modelPath, props);
    } else {
      throw new RuntimeException("ERROR: Unknown modelType " + modelType);
    }
    return relationExtractor;
  }
}
