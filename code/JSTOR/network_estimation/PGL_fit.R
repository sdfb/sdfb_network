library(glmnet)
library(parallel)

load("jstor_docCount.Rdata")

fit.cv <- function(y, X) {
  fit = glmnet(X, y,
       family="poisson",
       thres = 10^(-6))
  return(fit)
}


persons = colnames(data.mat)
docs = rownames(data.mat)

results = mclapply(1:length(persons), function(i) {
  y = data.mat[,i]
  X = data.mat[,-i]
  result = fit.cv(y, X)
  return(result)
}, mc.cores=4)

save(list=c("persons", "docs", "results"), file="jstor_PGLFit.Rdata")

