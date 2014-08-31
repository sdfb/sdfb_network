# Old graphical lasso model and poisson glm models

##### GRAPHICAL LASSO
# - Model fitting from old covariance matrices
# - Processing names of edges
# - Some preliminary attempts at graphing
library(huge)

load("scovmat.Rdata")
load("scovmat.label.Rdata")
mult.words = grep(" ", test)
s.cov.subset = s.cov[mult.words, mult.words]
rm(s.cov)

a = huge(s.cov.subset, lambda = c(0.5, 0.25, 0.1, 0.05, 0.02))

load("countss.Rdata")

word.mentions = rep(0, times = length(mult.words))
for(j in 1:length(mult.words)) {
  word.mentions[j] = total.counts[names(total.counts) == test[mult.words[j]]]
  print(j)
}
bb = data.frame(test[mult.words], word.mentions, 1:2773)

head(bb)
names(bb) = c("Name", "Mentions", "Index")
bb = bb[rev(order(bb$Mentions)),]
head(bb, 50)
top.ment = bb[1:50,3]

#lambda = 0.1
sp1 = as(a1$path[[3]], Class = "matrix")
sp1.small = sp1[top.ment, top.ment]

gr1 = graph.adjacency( adjmatrix = sp1.small, mode = "undirected")
gr1 = set.vertex.attribute(gr1, name= "Person", index=V(gr1), value = bb[1:50,1])

plot.igraph(gr1, vertex.label = as.character(bb[1:50,1]), vertex.label.cex = 1)

#lambda = 0.02
sp2 = as(a1$path[[5]], Class = "matrix")
sp2.small = sp2[top.ment, top.ment]

gr2 = graph.adjacency( adjmatrix = sp2.small, mode = "undirected")
plot.igraph(gr2, vertex.label = bb[1:50,1], vertex.label.cex = 1)

gr2 <- set.vertex.attribute(gr2, "label", value=bb[1:50,1])

b.gr2 = cohesive.blocks(gr2)
b.gr2 <- set.vertex.attribute(b.gr2, "Label", value=bb[1:50,1])
write.pajek.bgraph(b.gr2, filename = "test.paj")

a -> a1

load("meinbuhl.Rdata")

a$lambda

sp3 = as(a$path[[7]], Class = "matrix")
sp3.small = sp3[top.ment, top.ment]

gr3 = graph.adjacency( adjmatrix = sp3.small, mode = "undirected")
plot.igraph(gr3, vertex.label = bb[1:50,1], vertex.label.cex = 1)

## Testing glasso
# ## glasso test
library(MASS)
fake.dat = matrix(NA, nrow = 100, ncol = 8)
fake.dat[,1] = rnorm(100, 0, 1)
fake.dat[,2] = rnorm(100,fake.dat[,1] * .2,4)
fake.dat[,3] = rnorm(100,fake.dat[,1] * .2 + fake.dat[,2],2)
fake.dat[,4] = rnorm(100,20,3.6)
fake.dat[,5] = rnorm(100,fake.dat[,1],7)
fake.dat[,6] = rnorm(100, fake.dat[,1] + fake.dat[,5]*.7,12)
fake.dat[,7] = rnorm(100,fake.dat[,4] * 2,.3)
fake.dat[,8] = rnorm(100,fake.dat[,4] * fake.dat[,7],1)
true.cov = cov(fake.dat)
xx = mvrnorm(n = 1000, mu = rep(0, times = 8), Sigma = true.cov)

samp.cov = t(xx) %*% xx
huge(samp.cov, lambda = 100)


glasso(samp.cov, rho = 100000, approx = 1)
# 
# 
# 
# test.mat = matrix(rnorm(1000000), nrow = 1000, ncol = 5000)
# 
# system.time(t(test.mat) %*% test.mat)
# 
# d = matrix(nrow = 5, ncol = 3)
# d[1,] = c(.03, 1000, 100)
# d[2,] = c(.37, 10000, 100)
# d[3,] = c(32.97, 10000, 1000)
# d[4,] = c(3.26, 1000, 1000)
# d[5,] = c(82.90, 1000, 5000)

# Functions for processing results of model fitting
# - obtain all edges from a specific model, place in csv format

all.edges.from.mat <- function(mat, nam, num.out, num.match) {
  NN = dim(mat)[1]
  res = data.frame(P1 ="Person1", P2 ="Person2", C = num.out)
  for(i in 1:(NN-1)) {
    for (j in (i+1):NN) {
      if(mat[i,j] == num.match) {
        aa1 = c(nam[i], nam[j])
        aa2 = c(nam[j], nam[i])
        aa3 = c(num.out, num.out)
        bbb = data.frame(aa1, aa2, aa3)
        names(bbb) = c("P1", "P2", "C")
        res = rbind(res,bbb)
      }
    }
  }
  return(res[-1,])
}

test1 = all.edges.from.mat(sp1.small, nam = as.character(bb[1:50,1]), num.out = 1, num.match = 1)
test2 = all.edges.from.mat(sp2.small-sp1.small, nam = as.character(bb[1:50,1]), num.out = 2, num.match = 1)
tosave = rbind(test1,test2)
write.csv(tosave, file = "namesedges.csv")




reordered.names = as.character(bb[order(bb[,3]),1])

# This searches all names for exact matches. 
find.nodenum.from.name <- function(query, names = reordered.names) {
  test = grep(query, reordered.names)
  result = cbind(test, reordered.names[test])
  colnames(result) = c("Node Number", "Person")
  return(result)
}

find.nodenum.from.name(query = "Marlow")


## Function to give all connections given node number (can be vector of numbers).
find.names.from.nodenum <- function(edge.mat.small = sp1, edge.mat.mid = sp2, names = reordered.names, nodenum, two.lists = FALSE) {
  result = list()
  nodenum = as.numeric(nodenum)
  for(j in 1:length(nodenum)) {
    result[[j]] = list()
    result[[j]][[1]] = c("Person is:", names[nodenum])
    matches = which(edge.mat.small[nodenum[j],] == 1) 
    result[[j]][[2]] = cbind(matches, reordered.names[matches])
    colnames(result[[j]][[2]]) = c("Node Number", "Person (more likely)")
    
    if(two.lists) {
      matrix2 = edge.mat.mid - edge.mat.small
      matches = which(matrix2[nodenum[j],] == 1)
      result[[j]][[3]] = cbind(matches, reordered.names[matches])
      colnames(result[[j]][[3]]) = c("Node Number", "Person (less likely)")
    }
  }
  return(result)
}

find.names.from.nodenum(nodenum = 1743, two.lists = TRUE)
find.names.from.nodenum(nodenum = 1966, two.lists = TRUE)

## Function to sample a number of connections of each type (1.highly conf/2.low conf/3(or 0).no edge)
#    given specific node number
#2112 to 2223 are saints

sample.for.nodenum <- function(nodenum, num.high = 8, num.med = 6, num.none = 6, names = reordered.names, no.saint = TRUE) {
  temp = find.names.from.nodenum(nodenum = nodenum, two.lists = TRUE)[[1]]
  high.dat = temp[[2]]
  med.dat = temp[[3]]
  high.ind = as.numeric(high.dat[,1])
  med.ind = as.numeric(med.dat[,1])
  no.ind = setdiff(1:2773, union(med.ind, high.ind))
  if (no.saint) {
    saints = 2112:2223
    high.ind = setdiff(high.ind, saints)
    med.ind = setdiff(med.ind, saints)
    no.ind = setdiff(no.ind, saints)
  }
  if (num.high > length(high.ind)) { 
    print("Not enough high-confidence connections, so all are used")  
    num.high = length(high.ind)
  } 
  if (num.med > length(med.ind)) {
    print("Not enough med-confidence connections, so all are used")
    num.med = length(med.ind)
  }
  results = matrix("",ncol = 3, nrow = num.high+num.med+num.none)
  t1 = sample(high.ind, size = num.high)
  results[1:num.high,] = cbind(t1,names[t1],1)
  t1 = sample(med.ind, size = num.med)
  results[(1:num.med)+num.high,] = cbind(t1,names[t1],2)
  t1 = sample(no.ind, size = num.none)
  results[(1:num.none)+num.med+num.high,] = cbind(t1,names[t1],3)
  
  DIM = num.none + num.med + num.high
  aaa = sample(10000:50000, size = DIM) * 3 + as.numeric(results[,3])
  results[,3] = aaa
  
  rearrangement = sample(1:DIM, size = DIM)
  colnames(results) = c("Node Number", "Name", "Code")
  return(results[rearrangement,])
}

bbb = sample.for.nodenum(1743)

sample.for.nodenum(1743)

hist(apply(a$path[[2]], 1, sum), main = "Distribution of number of edges/node (lambda = 0.1)", breaks = 20,
     xlab = "Number of Edges in Node")

