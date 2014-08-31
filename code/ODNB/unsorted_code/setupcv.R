load("scovmat.Rdata")
load("scovmat.label.Rdata")

library(huge)
source("llgm.R")
library(huge)

long.names = grep(" ", test)

main.data = as.data.frame(dat.mat[,long.names])
main.data = main.data[,-(2122:2223)]

lambda =  seq(0.2, 0.02, by = -0.004)
NN = 10
num.datapts = 2773 - length(2122:2223)

for(n in 1:NN) {
  print(date())
  training = sort(sample(1:14107, size = 12700))
  testing = setdiff(1:14107, training)
  
  test.pois = my.huge.mb(x = as.matrix(main.data[training,]), 
                         lambda = lambda, 
                         link = "Poisson")
  
  print(paste("Poisson model fit... Trial",n))
  save(training, testing, test.pois, file = paste("trial",n,".Rdata", sep = "")) 
}
