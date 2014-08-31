#GLM Processing
lambda =  seq(0.2, 0.02, by = -0.004)
load("CVresultsGLM3.Rdata") #1 to 20
test.RD3 = test.RD
train.RD3 = train.RD
load("CVresultsGLM4.Rdata") # 22, 24, 26
test.RD4 = test.RD
train.RD4 = train.RD

which(test.RD4[1,,1] > 0)
which(test.RD3[1,,1] > 0)
test.RD = test.RD3
train.RD = train.RD3
test.RD[,c(22,24,26),] = test.RD4[,c(22,24,26),]
train.RD[,c(22,24,26),] = train.RD4[,c(22,24,26),]

dim(test.RD)
which(test.RD < 0)
which(test.RD[,,] > 10000)
which(apply(test.RD[,c(1:20,22,24,26),1:5], 1, max) > 10000)
#
#> which(apply(test.RD[,c(1:20,22,24,26),1:5], 1, max) > 10000)
#[1]   10   21  126  202  351  364  646  750  791  876 1132 1203
#[13] 1290 1317 1440 1864 1926 2045 2124 2209 2267 2336

## From train2.Rdata

# Case 1: Always large deviance 
## 10 is a result of: doc 5247 having a count of 7 for word 2600, but no other document
# has a count for this of more than two. 
## 21 is result of same doc. 
# &&& Distribution of total counts in test set matrix: Only 1 entry > 2, the one above. 
# &&& In training set, there are 10 occurrences > 2. 

test.RD[10,c(1:20,22,24,26),1:5]
test.RD[21,c(1:20,22,24,26),1:5]

# Case 2: Small to huge deviance
 ## 351: In this case, more and more covariates were added but then eventually 
  # too many matches => log(mean) of ~ 10 => prediction ~ 50000. 
  # This is possibly a problem with a long document accumulating a large number of names. 
  # Test doc number 680. (true doc number 6980)
  # This doc has 37 different other names appear (only once or twice)
  # seemingly, other docs only have usually fewer than 10 names. 
 ## 646: This happens here too; predictions do not get too large though (~ 10000)

 ## Out of 14107 documents, 94 have at least 40 different names mentioned.
 ##                         252 have at least 30
 ##                         769 have at least 20

 ## 1317: This can also be caused by jump for word 2600 as for many other cases. 
 ## 2336: Due to same reason. 

# Case 3: Randomly large deviance in middle
 ## 364: This case is due to same problem as docs 10/21; word 2600 was given a large
  # weight in a few cases, then skewed largely for count of 7. 
 ## 1203: Due to same problem. 


# Run this to get fit... (after loading appropriate trial #)
i = 2336
j = 24
i.touse = i
true.vals = testset[,i.touse]
wn = which(true.vals > 0)
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
pred.b = mod.fit$coef %*% t(as.matrix(cbind(1, testset[,useful.inds])))
pred = exp(pred.b)
true.times.log = true.vals
true.times.log[wn] = true.vals[wn] * (log(true.vals[wn]) - log(pred[wn]))
test.deviance = 2 * sum(true.times.log + pred - true.vals)

## ~~ Look at 
which(pred.b > 0)
testset[which(pred.b > 0),useful.inds]



#



k = c(100, 500, 1000,2000, 3500, 5000, 10000, 10^100)
ls.touse = which(test.RD[1,,1] > 0)
usable = test.RD[,ls.touse,1:5]

par(mfrow = c(2,4))
for(n in 1:8) {
    
  badrows = 0
  for(i in 1:dim(usable)[1]) {
    if (sum(usable[i,,] < 0) > 0) { badrows = c(badrows, i) }
    if (sum(usable[i,,] > k[n]) > 0) { badrows = c(badrows, i) }
  }
  badrows = unique(setdiff(badrows, 0))
  
  usable.new = usable[-badrows,,]
  ms = apply(usable.new, 2, mean)
  plot(lambda[ls.touse], ms, xlab = "Lambda", ylab = "Mean Deviance",
       main = paste("Cross-validation: Throw out rows >",k[n]) , type = "b", pch = 19,
       ylim = c(min(ms)-2, max(ms)+2) )
  sds = apply(usable.new, 2, mean) / sqrt(2*dim(usable.new)[1])
  
#   for(m in ls.touse) {
#     
#     arrows(lambda[m], ms[m], y1 = (ms[m] + 2 * sds[m]), angle = 90, length = 0.03,
#            col = "green")
#     arrows(lambda[m], ms[m], y1 = (ms[m] - 2 * sds[m]), angle = 90, length = 0.03,
#            col = "green")
#     
#     
#   }
}




dev.new()
hist(log(test.RD[,1:20,1:2][test.RD[,1:20,1:2] > 0], base = 10), breaks = 50 , xlab = "Log Deviances", 
     main = "Log Deviances")





usable = test.RD[,1:20,1:2]
badrows = 0
for(i in 1:dim(usable)[1]) {
  if (sum(usable[i,,] < 0) > 0) { badrows = c(badrows, i) }
  if (sum(usable[i,,] > k[n]) > 0) { badrows = c(badrows, i) }
}
badrows = unique(setdiff(badrows, 0))

usable.new = usable[-badrows,,]
ms = apply(usable.new, 2, mean)
plot(lambda[1:20], ms, xlab = "Lambda", ylab = "Mean Deviance",
     main = paste("Cross-validation: Throw out rows >",k[n]) , type = "b", pch = 19,
     ylim = c(200, 800) )
sds = apply(usable.new, 2, mean) / sqrt(2*dim(usable.new)[1])

for(m in 1:20) {
  
  arrows(lambda[m], ms[m], y1 = (ms[m] + 2 * sds[m]), angle = 90, length = 0.03,
         col = "green")
  arrows(lambda[m], ms[m], y1 = (ms[m] - 2 * sds[m]), angle = 90, length = 0.03,
         col = "green")
  
  
}



#


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

# 
# 
# for(n in 1:NN) {
#   print(date())
#   training = sort(sample(1:14107, size = 12700))
#   testing = setdiff(1:14107, training)
#   
#   test.pois = my.huge.mb(x = as.matrix(main.data[training,]), 
#                          lambda = lambda, 
#                          link = "Poisson")
#   
#   print(paste("Poisson model fit... Trial",n))
#   save(training, testing, test.pois, file = paste("trial",n,".Rdata", sep = "")) 
# }

##################################


lambda =  seq(0.2, 0.02, by = -0.004)
use.lambda.ind = c(1,6,11,16,21,26,31,36)
NN.todo = 1:5
NN = 10
num.datapts = 2773 - length(2122:2223)  # 2671

train.RD = array(0.0, dim = c(num.datapts, length(lambda), NN))
test.RD = array(0.0, dim = c(num.datapts, length(lambda), NN))

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
  
  save(train.RD, test.RD, file = "CVresultsGLM.Rdata")
  
}
