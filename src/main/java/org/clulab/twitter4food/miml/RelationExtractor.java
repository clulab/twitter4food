package org.clulab.twitter4food.miml;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import edu.stanford.nlp.stats.Counter;

import org.clulab.twitter4food.struct.TwitterAccount;

public abstract class RelationExtractor implements Serializable {
  private static final long serialVersionUID = 2606577772115897869L;
  
  protected static final Logger logger = Logger.getLogger(RelationExtractor.class.getName());
    
  public RelationExtractor() {
    logger.setLevel(Level.INFO);
  }
  
  /** 
   * Generates all possible labels (and their scores) for a tuple <entity, slot>
   * The NIL label is NOT to be generated by this method. 
   */
  /*
  public Counter<String> classifyAccount(TwitterAccount account) {
    List<Collection<String>> mentions = new ArrayList<Collection<String>>();
    for(int i = 0; i < account.size(); i ++) {
      mentions.add(account.datum(i).asFeatures());
    }
    return classifyMentions(mentions);
  }
  */
  
  public abstract Counter<String> classifyMentions(List<Collection<String>> relation);
  
  // Override for your oracle experiments
  public Counter<String> classifyOracleMentions(
      List<Collection<String>> relation,
      Set<String> goldLabels) {
    return null;
  }
  
  public void setLoggerLevel(Level level) {
    logger.setLevel(level);
  }
}
