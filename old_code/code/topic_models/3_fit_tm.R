## Fits 5, 10, 20 topic topic-models on entire nodeset
library(topicmodels)
library(tm)

load("data/TOPIC_MODEL/clean_corpus_20150610.Rdata")

tm_stem_5 = NULL; tm_stem_10 = NULL; tm_stem_20 = NULL

print(date())
print("Working on stem 5")
tm_stem_5 = LDA(x = stem_dtm, k = 5)
save(tm_stem_5, tm_stem_10, tm_stem_20, file = "data/TOPIC_MODEL/fitted_topic_models_20150614.Rdata")

print(date())
print("Working on stem 10")
tm_stem_10 = LDA(x = stem_dtm, k = 10)
save(tm_stem_5, tm_stem_10, tm_stem_20, file = "data/TOPIC_MODEL/fitted_topic_models_20150614.Rdata")

print(date())
print("Working on stem 10")
tm_stem_20 = LDA(x = stem_dtm, k = 20)
save(tm_stem_5, tm_stem_10, tm_stem_20, file = "data/TOPIC_MODEL/fitted_topic_models_20150614.Rdata")
