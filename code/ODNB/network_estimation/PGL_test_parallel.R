y = results[[k]]$results

cbind(nodeset$full_name[as.numeric(y$name)], y$finalBeta)


library(parallel)

mclapply(seq_len(4)), 

z = mclapply(X = 6:25, FUN = function(x) {rnorm(3) * rnorm(3)}, mc.cores = 4)



cd4.rg <- function(data, mle) MASS::mvrnorm(nrow(data), mle$m, mle$v)
> cd4.mle <- list(m = colMeans(cd4), v = var(cd4))
> run1 <- function(...) boot(cd4, corr, R = 500, sim = "parametric",
                             + ran.gen = cd4.rg, mle = cd4.mle)
> mc <- 2 # set as appropriate for your hardware
> ## To make this reproducible:
  > set.seed(123, "L
'
Ecuyer")
> cd4.boot <- do.call(c, mclapply(seq_len(mc), run1) )
> boot.ci(cd4.boot, type = c("norm", "basic", "perc"),
          + conf = 0.9, h = atanh, hinv = tanh
          
          





