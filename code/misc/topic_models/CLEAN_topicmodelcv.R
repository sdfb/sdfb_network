
load("tm_corpus.Rdata")

sub.samples = list()
sub.samples[[1]] = sample(1:6289, size = 2000)
sub.samples[[2]] = sample(setdiff(1:6289, sub.samples[[1]]), size = 2000)
sub.samples[[3]] = sample(setdiff(1:6289, c(sub.samples[[1]], sub.samples[[2]])), size = 2000)

topic.models = list()
for (K in 3:10) {
  topic.models[[K]] = list()
  for(S in 1:3) {
    print(date())
    print(paste("Starting", S, "of 3: K = ", K))
    topic.models[[K]][[S]] = LDA(x = dt.matrix[-sub.samples[[S]],], k=K)
  }
}
save(topic.models, sub.samples, file = "tms.cv.Rdata")
