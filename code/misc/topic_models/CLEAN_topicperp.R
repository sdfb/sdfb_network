library(tm)
library(topicmodels)

load("tm_corpus.Rdata")
load("tms.cv.Rdata")


dt.matrix = DocumentTermMatrix(corpus)

perps = matrix(nrow = 10, ncol = 3)

for(K in c(3:10,15)) {
for(S in 1:3) {
perps[K,S] = perplexity(topic.models[[K]][[S]], dt.matrix[sub.samples[[S]],])

}}

save(perps, file = "tmcv.Rdata")