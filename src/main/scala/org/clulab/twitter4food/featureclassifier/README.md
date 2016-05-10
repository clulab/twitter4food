# What is it?

This package contains feature classifiers that detect individual features, such as gender, race, and age.
Each custom classifier will extend ClassifierImpl with configuration parameters that denote usage of
unigrams, bigrams, cosine similarity, embeddings, topics, and follower features for training. 

ClassifierImpl contains implementation of train, test and evaluate methods for tuning each specific 
classifier. Each classifier is instantiated as the following:
    val classifier = new GenderClassifier(useUnigrams, useBigrams,
      useTopics, useDictionaries, useEmbeddings, useFollowers)