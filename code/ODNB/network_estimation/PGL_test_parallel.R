y = results[[k]]$results

cbind(nodeset$full_name[as.numeric(y$name)], y$finalBeta)


library(parallel)

mclapply(seq_len(4)), 

z = mclapply(X = 6:25, FUN = function(x) {rnorm(3) * rnorm(3)}, mc.cores = 4)


