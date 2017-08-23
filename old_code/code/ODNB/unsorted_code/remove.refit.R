library(glmnet)
library(Matrix)

load("sim.lambs.Rdata")
load("1230sp.datamat.sparse.Rdata")

sparse.dm = sparse.dm[,-c(488, 545 ,881, 1360, 1487)]
dm.names = dm.names[-c(488, 545 ,881, 1360, 1487)]
dim(sparse.dm)

# Milton ID = 2914
# Worst match: S Lewis => 4659 - NEED TO DROP
# Next: R Chambers => 3952 - ignore; no edge match
# Thomas Moore - 5292 - NEED TO DROP
# Elizabeth Gaskell - not in pool
# George I - 1356 - no edge match
# WM Thackeray - not in pool
# Anthony Gilby - 237 - no match
# Joseph Browne - 3331 - 
grep("George I", dm.names)
dm.names[grep("Gilby", dm.names)]
sim.lambs[2914,c()]
which(sim.lambs[2914, c(4659, 3952, 5292, 1356, 1357, 237, 331)] > 0)
dm.names[c(4659, 3952, 5292, 1356, 1357, 237, 331)]

valid.data = read.csv("milton.valid.csv", header = 0, stringsAsFactors = 0)
valid.data[33,1] = "Charles I"
valid.data[16,1] = "William Makepeace Thackeray"

match(valid.data[,1], dm.names)

valid.data = cbind(match(valid.data[,1], dm.names), valid.data[,1:2])

valid.data = valid.data[!is.na(valid.data[,1]),]
names(valid.data)[1] = "ID"
valid.data = cbind(valid.data,lambda=sim.lambs[2914,valid.data$ID])

min(valid.data[valid.data[,4] > 0,4])
# min = 0.00017

valid.data[order(valid.data[,4]),]
# Drop Giles Fletcher, 1526
# Drop John Toland, 3211
# Drop Christopher Potter, 590
# Drop Algernon Sidney, 130
# Drop Joseph Browne, 3331

lambda.list = list()
lambda.list[[1]] = unique(c(seq(from=100, to=10, by = -5),
                            seq(from=10, to=6, by = -.5),
                            seq(from=6, to=4, by = -.25),
                            seq(from=4, to=2, by = -.1),
                            seq(from=2, to=.9, by = -.05),
                            seq(from=.9, to=.3, by = -.04)))
lambda.list[[2]] = unique(c(seq(from=.3, to=.15, by = -.005),
                            seq(from=.15, to=.1, by = -.0025),
                            seq(from=.1, to=.05, by = -.002)))
lambda.list[[3]] = unique(c(seq(from=.05, to = .02, by = -.001),
                            seq(from=.02, to = .01, by = -.0002),
                            seq(from=.01, to = .005, by = -.0002),
                            seq(from=.005, to = .003, by = -.0001),
                            seq(from=.003, to = .002, by = -.00005),
                            seq(from=.002, to = .001, by = -.00002),
                            seq(from=.001, to = .0005, by = -.00001),
                            seq(from=.0005, to = .0002, by = -.000002),
                            seq(from=.0002, to = .0001, by = -.000001)))
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))

# orig fit

fit0 = glmnet(sparse.dm[,-2914], sparse.dm[,2914], family = 'poisson', lambda = lambda.touse,
maxit = 15000, thres = 10^(-6))

temp = rep(0, times = nrow(valid.data))
for(j in 1:nrow(valid.data)) {
  ind = valid.data[j,1]
  if (length(intersect(2914, ind)) == 0) {
    
    adj = sum(ind > 2914)
    ind = ind - adj
    z = min(which(fit0$beta[ind,] > 0))
    if (z > 50000) {
      temp[j] = 0
    } else {
      temp[j] = fit0$lambda[z]
    }
  } else {
    temp[j] = 0
  }
}
valid.data$lambda = temp

# Drop giles fletcher 1526
bad1 = c(1526,2914)
colnames(sparse.dm) = 1:6289
refit1 = glmnet(sparse.dm[,-bad1], sparse.dm[,2914], family = 'poisson', lambda = lambda.touse,
                maxit = 15000, thres = 10^(-6))
names(refit1)
dim(refit1$beta)

temp = rep(0, times = nrow(valid.data))
for(j in 1:nrow(valid.data)) {
  ind = valid.data[j,1]
  if (length(intersect(bad1, ind)) == 0) {
  
  adj = sum(ind > bad1)
  ind = ind - adj
  z = min(which(refit1$beta[ind,] > 0))
  if (z > 50000) {
    temp[j] = 0
  } else {
    temp[j] = refit1$lambda[z]
  }
  } else {
    temp[j] = 0
  }
}

valid.data = cbind(valid.data, d1=temp)
valid.data[order(valid.data[,5]),]

# drop john toland, 3211
bad2 = c(1526,2914, 3211)
refit2 = glmnet(sparse.dm[,-bad2], sparse.dm[,2914], family = 'poisson', lambda = lambda.touse,
                maxit = 15000, thres = 10^(-6))
names(refit2)
dim(refit2$beta)

temp = rep(0, times = nrow(valid.data))
for(j in 1:nrow(valid.data)) {
  ind = valid.data[j,1]
  if (length(intersect(bad2, ind)) == 0) {
    
    adj = sum(ind > bad2)
    ind = ind - adj
    z = min(which(refit2$beta[ind,] > 0))
    if (z > 50000) {
      temp[j] = 0
    } else {
      temp[j] = refit2$lambda[z]
    }
  } else {
    temp[j] = 0
  }
}

valid.data = cbind(valid.data, d2=temp)
valid.data[order(valid.data[,6]),]

# drop lady arabella, 3399
bad3 = c(1526,2914, 3211, 3399)
refit3 = glmnet(sparse.dm[,-bad3], sparse.dm[,2914], family = 'poisson', lambda = lambda.touse,
                maxit = 15000, thres = 10^(-6))

temp = rep(0, times = nrow(valid.data))
for(j in 1:nrow(valid.data)) {
  ind = valid.data[j,1]
  if (length(intersect(bad3, ind)) == 0) {
    
    adj = sum(ind > bad3)
    ind = ind - adj
    z = min(which(refit3$beta[ind,] > 0))
    if (z > 50000) {
      temp[j] = 0
    } else {
      temp[j] = refit3$lambda[z]
    }
  } else {
    temp[j] = 0
  }
}

valid.data = cbind(valid.data, d3=temp)
valid.data[order(valid.data[,7]),]


# drop k charles, 3382
bad4 = c(1526,2914, 3211, 3399, 3382)
refit4 = glmnet(sparse.dm[,-bad4], sparse.dm[,2914], family = 'poisson', lambda = lambda.touse,
                maxit = 15000, thres = 10^(-6))

temp = rep(0, times = nrow(valid.data))
for(j in 1:nrow(valid.data)) {
  ind = valid.data[j,1]
  if (length(intersect(bad4, ind)) == 0) {
    
    adj = sum(ind > bad4)
    ind = ind - adj
    z = min(which(refit4$beta[ind,] > 0))
    if (z > 50000) {
      temp[j] = 0
    } else {
      temp[j] = refit4$lambda[z]
    }
  } else {
    temp[j] = 0
  }
}

valid.data = cbind(valid.data, d4=temp)
valid.data[order(valid.data[,8]),]


# drop christopher potter, 590
bad5 = c(1526,2914, 3211, 3399, 3382, 590)
refit5 = glmnet(sparse.dm[,-bad5], sparse.dm[,2914], family = 'poisson', lambda = lambda.touse,
                maxit = 15000, thres = 10^(-6))

temp = rep(0, times = nrow(valid.data))
for(j in 1:nrow(valid.data)) {
  ind = valid.data[j,1]
  if (length(intersect(bad5, ind)) == 0) {
    
    adj = sum(ind > bad5)
    ind = ind - adj
    z = min(which(refit5$beta[ind,] > 0))
    if (z > 50000) {
      temp[j] = 0
    } else {
      temp[j] = refit5$lambda[z]
    }
  } else {
    temp[j] = 0
  }
}

valid.data = cbind(valid.data, d5=temp)
valid.data[order(valid.data[,9]),]




# drop algernon sidney, 130
bad6 = c(1526,2914, 3211, 3399, 3382, 590, 130)
refit6 = glmnet(sparse.dm[,-bad6], sparse.dm[,2914], family = 'poisson', lambda = lambda.touse,
                maxit = 15000, thres = 10^(-6))

temp = rep(0, times = nrow(valid.data))
for(j in 1:nrow(valid.data)) {
  ind = valid.data[j,1]
  if (length(intersect(bad6, ind)) == 0) {
    
    adj = sum(ind > bad6)
    ind = ind - adj
    z = min(which(refit6$beta[ind,] > 0))
    if (z > 50000) {
      temp[j] = 0
    } else {
      temp[j] = refit6$lambda[z]
    }
  } else {
    temp[j] = 0
  }
}

valid.data = cbind(valid.data, d6=temp)
valid.data[order(valid.data[,10]),]




# drop john tomkins, 3213
bad7 = c(1526,2914, 3211, 3399, 3382, 590, 130, 3213)
refit7 = glmnet(sparse.dm[,-bad7], sparse.dm[,2914], family = 'poisson', lambda = lambda.touse,
                maxit = 15000, thres = 10^(-6))

temp = rep(0, times = nrow(valid.data))
for(j in 1:nrow(valid.data)) {
  ind = valid.data[j,1]
  if (length(intersect(bad7, ind)) == 0) {
    
    adj = sum(ind > bad7)
    ind = ind - adj
    z = min(which(refit7$beta[ind,] > 0))
    if (z > 50000) {
      temp[j] = 0
    } else {
      temp[j] = refit7$lambda[z]
    }
  } else {
    temp[j] = 0
  }
}

valid.data = cbind(valid.data, d7=temp)
valid.data[order(valid.data[,11]),]



which(valid.data[,3] == "10")


write.csv(valid.data, file = "remove.refit.result.csv")
