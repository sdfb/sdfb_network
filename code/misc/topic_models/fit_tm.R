
library(topicmodels)
library(tm)

load("data/TOPIC_MODEL/clean_corpus_20150608.Rdata")
print(date())
print("Working on raw 5")
tm_raw_5 <- LDA(x=dtm, k = 5)
save(tm_raw_5, file = "data/TOPIC_MODEL/tms1.Rdata")

print(date())
date()
print("Working on stem 5")
tm_stem_5 = LDA(x = stem_dtm, k = 5)
save(tm_raw_5, tm_stem_5, file = "data/TOPIC_MODEL/tms1.Rdata")

print(date())
date()
print("Working on raw 10")
tm_raw_10 <- LDA(x=dtm, k = 10)
save(tm_raw_5, tm_stem_5, tm_raw_10, file = "data/TOPIC_MODEL/tms1.Rdata")

print(date())
date()
print("Working on stem 10")
tm_stem_10 = LDA(x = stem_dtm, k = 10)
save(tm_raw_5, tm_stem_5, tm_raw_10, tm_stem_10, file = "data/TOPIC_MODEL/tms1.Rdata")


print(date())
date()
print("Working on stem 20")
tm_stem_20 <- LDA(x=stem_dtm, k = 20)
save(tm_raw_5, tm_stem_5, tm_raw_10, tm_stem_10, tm_stem_20, file = "data/TOPIC_MODEL/tms1.Rdata")

print(date())
date()
print("Working on raw 20")
tm_raw_20 <- LDA(x=dtm, k = 20)
save(tm_raw_5, tm_stem_5, tm_raw_10, tm_stem_10, tm_stem_20, tm_raw_20, file = "data/TOPIC_MODEL/tms1.Rdata")











# 
# 
# library(lda)
# convert_dtm_into_proper_form = function(dtm, maxrows = NULL) { 
#   if (is.null(maxrows)) { maxrows = dim(dtm)[1]}
#   reslist = list()
#   for(r in 1:maxrows) {
#     inds = which(dtm$i == r)
#     reslist[[r]] = rbind(as.integer(dtm$j[inds] - 1), as.integer(dtm$v[inds]))
#     cat(r, " ")
#   }
#   return(reslist)
# }
# 
# 
# 
# 
# 
# 
# newdat = convert_dtm_into_proper_form(dtm, maxrows = 100)
# system.time((test = lda.collapsed.gibbs.sampler(newdat, vocab = dtm$v, K = 10, num.iterations = 30, alpha = 0.1, eta = 0.1, compute.log.likelihood = TRUE, trace = as.integer(5))
# ))


