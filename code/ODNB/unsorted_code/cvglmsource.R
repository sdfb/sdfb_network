## Parameters?
run.model.true = TRUE
run.fits.true = TRUE
model = "A"

lambda1 = c(seq(0.8, 0.5, by = -0.1), seq(0.4, 0.3, by = -0.02)) #trialA
lambda2 = seq(0.28, 0.1, by = -0.02)                              #trialB

NN = 10
min.thres = 3 # use only rows that have more than this number of nonzero entries

## Code
load("update2.datamat.Rdata")

library(huge)
source("llgm.R")
library(huge)
source("cvglmfunct.R")

if(model == "A") {
  #Run models (for network)
  if(run.model.true) { 
    setup.cv.pgl(data = upd.data.matrix, lambda = lambda1, base.filename = "trialA") 
  }
  
  #Run fits
  if(run.fits.true) {
    run.kcv.pgl(k = 10, NN = 1:10, data = upd.data.matrix, base.filename = "trialA",
                lambda = lambda1, result.filename = "CVglm.trialA.Rdata")
  }
} else if (model == "B") {
  if(run.model.true) { 
    setup.cv.pgl(data = upd.data.matrix, lambda = lambda2, base.filename = "trialB",
                 use.cur.groups = TRUE, old.filename = "trialA") 
  }
  
  #Run fits
  if(run.fits.true) {
    run.kcv.pgl(k = 10, NN = 1:10, data = upd.data.matrix, base.filename = "trialB",
                lambda = lambda2, result.filename = "CVglm.trialB.Rdata")
  }
}
  
  
  
  
