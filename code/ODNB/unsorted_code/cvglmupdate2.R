load("scovmat.Rdata")
load("scovmat.label.Rdata")

library(huge)
source("llgm.R")
library(huge)

long.names = grep(" ", test)

main.data = as.data.frame(dat.mat[,long.names])
main.data = main.data[,-(2122:2223)]

lambda =  seq(0.2, 0.02, by = -0.004)
use.lambda.ind = 1:20
NN.todo = 1:5
NN = 10
num.datapts = 2773 - length(2122:2223)

train.RD = array(0.0, dim = c(num.datapts, length(lambda), NN))
test.RD = array(0.0, dim = c(num.datapts, length(lambda), NN))

options(warn = 2)

#save(training, testing, test.pois, file = paste("trial",n,".Rdata", sep = ""))
for (n in NN.todo) {
  print(date())
  cat("Loading main data: TRIAL =",n, "\n")
  load(paste("trial",n,".Rdata", sep = ""))
  
  trainset = data.frame(main.data[training,])
  testset = data.frame(main.data[testing,])
  colnames(trainset) = 1:num.datapts
  colnames(testset) = 1:num.datapts
  
  for(i in 1:num.datapts) {
    i.touse = i
    cat("Trial", i, "--- Time:", date(), "\n")
    
    true.vals = testset[,i.touse]
    wn = which(true.vals > 0)
    
    for(j in use.lambda.ind) {
      useful.inds = which(test.pois$path[[j]][i.touse,] > 0)
      if(length(useful.inds) == 0) {
        mod.fit = glm(trainset[,i.touse] ~ 1, family = "poisson")
      } else if (length(useful.inds) == 1) {
        mod.fit = glm(trainset[,i.touse] ~ trainset[,useful.inds],
                      family = "poisson")
      } else {
        mod.fit = try(glm(trainset[,i.touse] ~ as.matrix(trainset[,useful.inds, drop = FALSE]),
                          family = "poisson"), silent = TRUE)
      }
      if (is(mod.fit, "try-error")) {
        train.RD[i,j,n] = -1
        test.RD[i,j,n] = -1
      } else {
        pred = mod.fit$coef %*% t(as.matrix(cbind(1, testset[,useful.inds])))
        pred = exp(pred)
        
        train.RD[i,j,n] = mod.fit$deviance
        
        
        true.times.log = true.vals
        true.times.log[wn] = true.vals[wn] * (log(true.vals[wn]) - log(pred[wn]))
        test.RD[i,j,n] = 2 * sum(true.times.log + pred - true.vals)
      }
    }}
  print("computations from refitting glms has been done")
  
  save(train.RD, test.RD, file = "CVresultsGLM3.Rdata")
  
}