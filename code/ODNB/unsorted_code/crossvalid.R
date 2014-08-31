load("scovmat.Rdata")
load("scovmat.label.Rdata")

library(huge)
source("llgm.R")
library(huge)

long.names = grep(" ", test)

main.data = as.data.frame(dat.mat[,long.names])

lambda =  seq(0.2, 0.02, by = -0.02)

NN = 10
num.datapts = 2773

train.RD = array(0.0, dim = c(2773, length(lambda), NN))
test.RD = array(0.0, dim = c(2773, length(lambda), NN))

test.RD.fm = array(0.0, dim = c(2773, length(lambda), NN))
test.RD.adj = array(0.0, dim = c(2773, length(lambda), NN))


for(n in 1:NN) {
  print(date())
load(paste("trial",n,".Rdata", sep = ""))
print(paste("Poisson model loaded... Trial",n))

trainset = data.frame(main.data[training,])
testset = data.frame(main.data[testing,])
colnames(trainset) = 1:2773
colnames(testset) = 1:2773

print(date())
## Using known betas
for(i in 1:num.datapts) {
  if (i %% 100 == 1) {print(i)}
  for(j in 1:length(lambda)) {
    pred = test.pois$beta[[j]][i,] %*% t(testset)
    pred = exp(pred)
  
  true.vals = testset[,i]
  true.times.log = true.vals
  true.times.log[which(true.vals > 0)] = true.vals[which(true.vals > 0)] * log(true.vals[which(true.vals > 0)]/pred[which(true.vals > 0)])
  test.RD.fm[i,j,n] = sum(2 * abs(true.times.log - (true.vals - pred)))
    
    pred = pred - 1
    pred[pred < 0] = 0.0000001
    
    true.times.log = true.vals
    true.times.log[which(true.vals > 0)] = true.vals[which(true.vals > 0)] * 
      ( log(true.vals[which(true.vals > 0)]) - 
        log(pred[which(true.vals > 0)]) )
    test.RD.adj[i,j,n] = sum(2 * abs(true.times.log - (true.vals - pred)))
    
  }
}

  save(test.RD.fm, test.RD.adj, file = "CVresults1.Rdata")
print("computations from graph have been done.")
  print(date())
#Runtime: 1hour


## Using refitting poissons

for(i in 1:num.datapts) {
  i.touse = i
  print(i)
for(j in 1:8) {
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
  
  true.vals = testset[,i.touse]
  true.times.log = true.vals
  true.times.log[which(true.vals > 0)] = true.vals[which(true.vals > 0)] * log(true.vals[which(true.vals > 0)]/pred[which(true.vals > 0)])
  test.RD[i,j,n] = sum(2 * abs(true.times.log - (true.vals - pred)))
  }
}}
print("computations from refitting glms has been done")

save(train.RD, test.RD, file = "CVresults2.Rdata")
}



# 
# predict(mod.fit, newdata = main.data[testing,useful.inds])
# 
# rest = c(2,5,240,429,519,566,771,1029, 1159, 1216, 1245, 1339, 1591, 1605, 1650, 1697,
#          1704, 1811, 1957, 1986, 2073, 2229, 2246, 2266, 2398)
# datad = main.data[,c(1,rest)]
# 
# mod.fit = glm(datad[,1] ~ datad[,2:(length(rest) - 1)], family = "poisson")
# 
# 
# 
# counts <- c(18,17,15,20,10,20,25,13,12)
# outcome <- gl(3,1,9)
# treatment <- gl(3,3)
# print(d.AD <- data.frame(treatment, outcome, counts))
# glm.D93 <- glm(counts ~ outcome + treatment, family=poisson())
# anova(glm.D93)
# summary(glm.D93)
# 
# awer = resid(glm.D93, type = "deviance")
# 
# L = huge.generator(d = 200, graph="hub")
# out.mb = huge(L$data)
# out.ct = huge(L$data, method = "ct")
# out.glasso = huge(L$data, method = "glasso")
