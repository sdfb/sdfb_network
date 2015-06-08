## Fits 5-topic topic models on fuller data

library(topicmodels)
library(tm)

load("data/TOPIC_MODEL/clean_corpus_20150608.Rdata")
print("Working on raw 5")
tm_raw_5 <- LDA(x=dtm, k = 5)
save(tm_raw_5, file = "data/TOPIC_MODEL/tms1.Rdata")

print(date())
print("Working on stem 5")
tm_stem_5 = LDA(x = stem_dtm, k = 5)
save(tm_raw_5, tm_stem_5, file = "data/TOPIC_MODEL/tms1.Rdata")
