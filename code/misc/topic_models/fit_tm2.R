## Fits 5, 10, 20 topic topic-models on smaller dataset (6000 nodes)
library(topicmodels)
library(tm)

load("data/TOPIC_MODEL/clean_corpus_20150608.Rdata")

tm_stem_5 = NULL; tm_stem_10 = NULL; tm_stem_20 = NULL

print(date())
print("Working on stem 5")
tm_stem_5 = LDA(x = sl_dtm, k = 5)
save(tm_stem_5, tm_stem_10, tm_stem_20, file = "data/TOPIC_MODEL/tms2.Rdata")

print(date())
print("Working on stem 10")
tm_stem_10 = LDA(x = sl_dtm, k = 10)
save(tm_stem_5, tm_stem_10, tm_stem_20, file = "data/TOPIC_MODEL/tms2.Rdata")

print(date())
print("Working on stem 10")
tm_stem_20 = LDA(x = sl_dtm, k = 20)
save(tm_stem_5, tm_stem_10, tm_stem_20, file = "data/TOPIC_MODEL/tms2.Rdata")


