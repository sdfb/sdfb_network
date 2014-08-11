library(tm)
library(topicmodels)

load("tm_corpus.Rdata")
load("tms.cv.Rdata")

for (K in c(15,20,30)) {
  topic.models[[K]] = list()
  for(S in 1:3) {
    print(date())
    print(paste("Starting", S, "of 3: K = ", K))
    topic.models[[K]][[S]] = LDA(x = dt.matrix[-sub.samples[[S]],], k=K)
  }
}
save(topic.models, sub.samples, file = "tms.cv.Rdata")
