# generate fake data

library(huge)
library(multicore)
source("llgm.R")


runif(15000) -> scaling

ini.data = mp.generator(lambda = 0.01, lambda.c = 0.02, n = 15000, p = 2500, type = "random")
test.data1 = t(ini.data$X) 
test.data2 = test.data1*scaling
test.data3 = round(test.data2)
test.pois = glmpois(X = t(test.data3), lambda = c(0.5, 0.2, 0.1, 0.05, 0.02))


for(j in 1:10000) {
  sample(1:10000, size = 50) -> rs
  sample(1:500, size = 2) -> cs
  test.data2[rs,cs] = runif(1) * test.data2[rs,cs]
  print(j)
}


for(i in 1:30000) {
  sample(1:10000, size = 20) -> rs
  sample(1:500, size = 2) -> cs
  test.data3[rs,cs] = 0
  if (i %% 10000 == 0) {
    print(i)
  }
}
sum(test.data3 > 0) / length(test.data3)
test.pois = glmpois(X = t(test.data3), lambda = c(0.5, 0.2, 0.1, 0.05, 0.02))


test.huge = huge(test.data1, lambda = c(0.5, 0.2, 0.1, 0.08, 0.065, 0.05, 0.01))

huge.predict = test.huge$path[[3]]





### Old stuff

load("scovmat.Rdata")
load("scovmat.label.Rdata")
mult.words = grep(" ", test)
s.cov.subset = s.cov[mult.words, mult.words]
rm(s.cov)

a.poisson = my.huge.mb(x = s.cov.subset, lambda = c(0.5, 0.25, 0.1, 0.05, 0.02, 0.015, 0.01), link = "Poisson")
save(a.poisson, file = "apois.Rdata")

a.pois3 = my.huge.mb(x = s.cov.subset, lambda = c(0.1, 0.05, (49:20 * 0.001)[1:14 * 2] ), link = "Poisson")
save(a.pois3, file = "apois3.Rdata")
##



load("meinbuhl.Rdata")
load("apois3.Rdata")

a.poisson$sparsity[3:12]
sum(a.poisson$path[[3]])
sum(a$path[[2]])

a.pois3$sparsity
a.pois3$lambda[5]

gg2 = a.pois3$path[[5]]
gg1 = a$path[[2]]
sum(gg2 + gg1 == 2)
total.edgess = dim(gg2)[1] * (dim(gg2)[1]-1)

sum(gg2)
sum(gg1)

a.pois3$sparsity

gg3 = a.pois3$path[[1]]
sum(gg3 + gg1 == 2) / sum(gg3)

## Function to give all connections given node number (can be vector of numbers).
find.names.from.nodenum.both <- function(edge.mat.mb = gg1, edge.mat.pois = gg2, names = reordered.names, nodenum) {
  result = list()
  nodenum = as.numeric(nodenum)
  for(j in 1:length(nodenum)) {
    result[[j]] = list()
    result[[j]][[1]] = c("Person is:", names[nodenum])
    matches = which(edge.mat.mb[nodenum[j],] + edge.mat.pois[nodenum[j],] == 2) 
    result[[j]][[2]] = cbind(matches, reordered.names[matches])
    colnames(result[[j]][[2]]) = c("Node Number", "Person [BOTH LISTS]")
    
    matrix2 = edge.mat.mb - edge.mat.pois
    matches = which(matrix2[nodenum[j],] == 1)
    result[[j]][[3]] = cbind(matches, reordered.names[matches])
    colnames(result[[j]][[3]]) = c("Node Number", "Person [MB Only]")
    
    matrix3 = edge.mat.pois - edge.mat.mb
    matches = which(matrix3[nodenum[j],] == 1)
    result[[j]][[4]] = cbind(matches, reordered.names[matches])
    colnames(result[[j]][[4]]) = c("Node Number", "Person [Poisson Only]")
    
  }
  return(result)
}


find.names.from.nodenum.both(nodenum = 1743)
find.names.from.nodenum.both(nodenum = 1275)
find.names.from.nodenum.both(nodenum = 1966)


hist(apply(gg2, 1, sum), main = "Distribution of number of edges/node (Pois, lambda = 0.044)", breaks = 20,
     xlab = "Number of Edges in Node")



