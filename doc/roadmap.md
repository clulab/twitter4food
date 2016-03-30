# Roadmap

The twitter4food framework is expanding from its current state of naive
SVM classifier to a more intricate mesh of health-related data mapped with 
twitter account metadata, and linguistic annotations of user tweets.

## Current system

Presently, the classifier is fed a concotion of annotated features such as
n-grams, word embeddings, counts from lexicons, and domain adaptation 
techniques. Also, the implementations of these are primitive, but functional. 
Literature review of contemporary papers suggest that mixtures of these 
features generally augment the accuracy of the prediction mechanism. At the 
higher level, demographic sub-features such as gender, age, race, income, 
education, and location are identified with the same classifier framework and 
later fed as meta features to the overweight classifier. In turn, it attempts 
to partition the set of accounts with the help of these subfeatures and 
crucially, its own set of features.

We propose additions to this system based on some of the approaches used in the
set of papers listed [below](#references). 

## Related works

[1] suggested an interesting feature of average caloric value per tweet based 
on the food-related words mentioned in the tweet. Intuitively, this is a good 
indicator for the calories consumed when matched against available health-data
mentioned even in restaurant menus. Unfortunately, the authors claim this was 
not the most predictive feature. [2] mentioned that the social networks of
consumers are reflective of their characteristics. They refer to this with the
idea of 'homophily' to mean 'birds of a feather flock together', which can be 
extended to other subfeatures. [1] and [2] contain the idea of (un)directed
graphs between twitter followers. [1] states that accounts with 4+ active users
who have high probability of being obese are also likely to be obese. 
[2] filters active users based on a threshold of interaction between two users 
within a two-week period. 

[3] approaches the problem with a 'lightly supervised' training on soft 
constraints rather than individual labels. Label regularization is subsequently 
the process of using those constraints as prior knowledge to fit a model to 
unlabeled instances. While they use nearly hundreds of constraints, they are 
broadly catergorised as *cnt* (county), *fol* (followers), and *nam* (name).
The math behind label regularization attempts to label the test set such that 
the probability distribution of each constraint is as expected from prior 
knowledge. Constraint selection of various forms is used to reduce the number 
of constraints (improved-greedy being the best), though they found that using 
all constraints does well despite the overlap and noise among the constraints.
The baselines are simple predictions for predicting race and age based on name 
statistics and predicting political classification based on number of 
politicians they follow of a particular party affiliation.

[2], [4], and [6] look at sociolinguistic patterns to etch out common language
usage between target users. [4] considers features such as online engagement
(volume of tweets, mentions, and replies), ego-network (number of followers on
a given day), emotion, and linguistic style (word count in catergories such as
auxilary verbs, pronouns, and swear words). [6] uses parts of speech such as 
pronouns, conjunction, nominative verbs etc. to affix patterns for person name
mention and person nominal mention to predict the gender of a named entity in
a paragraph of text. The training for the fixed pattern was done offline and
used as model for annotating gender information based on the confidence score.

[5] is an implementation of the gender classification with the same feature 
collection as our current system. The ensemble of screen name, tweets, and
description gave a prediction of 92%. [4] was set up on finding the point where
a mother gives birth to her child and the effects of postpartum changes can be 
observed in the context of her tweets. Consequently, [4] is relevant for 
identifying a breakpoint/change in the sentiment of a user's tweets.

## Proposed features

* For gender classification and perhaps other features, it is useful to search
the mentions of the concerned user because others are likely to use of pronouns
such as ‘he/she’ to refer to the current user.

* For income/education, to estimate what race the current user is, it will be 
prudent to look at the friends of the users. Follows from the ‘homophily’ of 
above paper. The same could be said of similarities among overweight people.

* Lexical/POS tags can throw signal at the education level of the user. Use of 
contracted ‘textese’ is a good indicator that the user is one of the following: 
not of sound education level, or (racism, stereotyping aside) low income racial 
background, or a teenager (which can be useful to guess the age). On the same 
lines, usage of erudite words commonly found in major literature and GRE word 
lists (words such as ‘erudite’) suggests stronger educational background.

* In general, classification of an account need not be at the mercy of the svm
classifier and a set of training accounts that we shove into it. As in [6], we 
can train some features offline from realtime CDC, NHS data about the features 
of overweight people and use in conjunction with the classifier.

* [1] and other related papers use LIWC (Linguistic Inquiry and Word Count) and 
Food-Demographic information in conjunction, and separately.

* As in [4], using linguistic and ego-network was useful in detecting change.
The change could be as simple as change in eating pattern or increase in tweet
volume, and other behavioural features.

* Continue with the ensemble of lexicon to spot gender-specific names, words 
in description as per [5]. In this case, fetch census information about 
multi-cultural name lists, and similar words describing gender.

* The aforementioned points consider features among a group of accounts. To add
more information, the fundamental step is to apply the same linguistic, lexical,
and word count emeddings for all food-related tweets of a user.



## References

[1] Sofiane Abbar, Yelena Mejova, and Ingmar Weber. You tweet what you eat: 
Studying food consumption through twitter. In Proc. CHI, 2015.

[2] David Bamman, Jacob Eisenstein, and Tyler Schnoebelen. Gender identity 
and lexical variation in social media. Journal of Sociolinguistics
Volume 18, Issue 2, p.135–160, April 2014.

[3] Ehsan Mohammady Ardehaly, and Aron Culotta. Inferring Latent Attributes of
Twitter Users with Label Regularization. NAACL/HLT, 2015.

[4] Munmun De Choudhury, Scott Counts, and Eric Horvitz. Predicting postpartum
changes in emotion and behavior via social media. Proc. of the 2013 ACM Annual 
Conference on Human Factors in Computing Systems. ACM New York, p.3267-3276, 2013.

[5] John D. Burger, John Henderson, George Kim, and Guido Zarrella. Discriminating
gender on Twitter. In Proceedings of EMNLP’11, p.1301–1309, 2011. 

[6] Heng Ji, and Dekang Lin. Gender and Animacy Knowledge Discovery from 
Web-Scale N-Grams for Unsupervised Person Mention Detection. 23rd Pacific 
Asia Conference on Language, Information and Computation, p. 220–229, 2009.

