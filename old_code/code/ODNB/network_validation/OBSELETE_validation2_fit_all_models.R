## This file contains very old code that fits all the different models needed for the validation stage. I don't plan to clean this file up... Don't want to simply delete it, but I don't think we'll need it any longer. All the network estimation algorithms had already been improved (in terms of better written code that's far easier to use/re-use), and are many iterations away from this code. 










########### Fitting full model for no split, without indicator

## run on banshee
library(snow)
library(glmnet)

clust1 = makeSOCKcluster(c("node68","node69","node70", "node71", "node72",
                           "node73", "node74", "node75", "node76", "node77",
                           "node78","node79"))

# pw ...


# stopCluster(clust1)

clusterEvalQ(clust1, library(glmnet))
clusterEvalQ(clust1, load("cbnospi1.dm.Rdata"))
load("cbnospi1.dm.Rdata")

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
                            seq(from=.002, to = .001, by = -.00002)))
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))

glm.x = function(x) {
  return(try(glmnet(dm[,-x], dm[,x],
                    family="poisson", lambda = lambda.touse, 
                    maxit = 2000 , thres = 10^(-6))))
}


find.done = function(x) {
  done = rep(-1, times = length(x))
  for(i in 1:length(x)) {
    if (length(x[[i]]) == 12) {
      done[i] = (x[[i]]$lambda[length(x[[i]]$df)] == 0.001)
    }
  }
  return(done)
}

glm.x.re <- function(x, prog, lamb.adj = 11) {
  start = prog[[x]]$lambda[length(prog[[x]]$lambda) - 1]
  matches = match(prog[[x]]$lambda,lambda.touse)
  end = lambda.touse[max(matches, na.rm = TRUE) + 1]
  
  to.add = seq(from=start, to=end, length.out = lamb.adj)
  new.lambda = unique(sort(c(to.add, prog[[x]]$lambda, lambda.touse), decreasing = TRUE))
  maxit2 = 2000 * (prog[[x]]$iters+1)
  #return(list(new.lambda, maxit2))
  return(try(glmnet(dm[,-x], dm[,x],
                    family="poisson", lambda = new.lambda, 
                    maxit = maxit2 , thres = 10^(-6))))
}

save(lambda.touse, glm.x, find.done, glm.x.re, file = "helper.Rdata")
clusterEvalQ(clust1, load("helper.Rdata"))


snow.time(parLapply(clust1, x = 1:30, fun = glm.x))


results = list()
N.vertex = dim(dm)[2]

# First attempt at fit
## 6767 to do, do 24 at a time. 
for(k in 1:300) {
  j = intersect((length(results) + 1 ): (length(results) + 24), 1:N.vertex)
  if (length(j) > 0) {
    print(c(min(j), max(j)))
    print(date())
    print(snow.time(( results[j] = parLapply(clust1, x = j, fun = glm.x) )) )
    if (k %% 10 == 0) {
      print("Saving....")
      print(date())
      save(results, file = "cbnosp.fit.Rdata")
    }
  }
}
save(results, file = "cbnosp.fit.Rdata")

######### banshee ran up to here:

# re-running non-finished trials
nodes=12

for(run in 2:10) {
  is.done = find.done(results)
  to.do = which(is.done == 0)
  prog = list()
  
  if (length(to.do) > 0) {
    
    
    for(j in 1:N.vertex) {
      if (is.done[j] == 0) {
        prog[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
      }
    }
    
    to.run = NULL
    for(i in 1:500) {
      prog[to.run] = paste(run ,"<- iteration done")
      to.run = to.do[intersect((nodes*(i-1)+1):(nodes*(i)), 1:length(to.do))]
      to.run = to.run[!is.na(to.run)]
      if (length(to.run) > 0) {
        print(paste("Re-iter:", run))
        print(to.run)
        print(date())
        print(snow.time((results[to.run] = parLapply(clust1, x = to.run, 
                                                     fun = glm.x.re, 
                                                     prog = prog))) )
      }
    }
    save(results, file = "cbnosp.fit.Rdata")
    
    
  }
}

for(run in 11:25) {
  is.done = find.done(results)
  to.do = which(is.done == 0)
  prog = list()
  
  if (length(to.do) > 0) {
    
    
    for(j in 1:N.vertex) {
      if (is.done[j] == 0) {
        prog[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
      }
    }
    
    to.run = NULL
    for(i in 1:500) {
      prog[to.run] = paste(run ,"<- iteration done")
      to.run = to.do[intersect((nodes*(i-1)+1):(nodes*(i)), 1:length(to.do))]
      to.run = to.run[!is.na(to.run)]
      if (length(to.run) > 0) {
        print(paste("Re-iter:", run))
        print(to.run)
        print(date())
        print(snow.time((results[to.run] = parLapply(clust1, x = to.run, 
                                                     fun = glm.x.re, 
                                                     prog = prog,
                                                     lamb.adj = (run*2) + 1) )) )
      }
    }
    
  }
}
save(results, file = "cbnosp.fit.Rdata")

# need to fix: 1305, 2325
x = 1305
new.lambda = 0.5*(results[[x]]$lambda[-1] + results[[x]]$lambda[-length(results[[x]]$lambda)])
new.lambda = sort(unique(c(new.lambda, results[[x]]$lambda, lambda.touse)), decreasing = TRUE)
test = try(glmnet(dm[,-x], dm[,x],
                  family="poisson", lambda = new.lambda, 
                  maxit = 20000 , thres = 10^(-6)))



########### Fitting full model for split, full docs, without indicator
## updated 12/14 for running with smaller lambdas
## run on banshee
library(snow)
library(glmnet)

clust1 = makeSOCKcluster(c("node68","node69","node70", "node71", "node72",
                           "node73", "node74", "node75", "node76", "node77",
                           "node78","node79"))

# pw ...

# stopCluster(clust1)

clusterEvalQ(clust1, library(glmnet))
clusterEvalQ(clust1, load("cb500i1.dm.Rdata"))
load("cb500i1.dm.Rdata")

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

glm.x = function(x) {
  return(try(glmnet(dm[,-x], dm[,x],
                    family="poisson", lambda = lambda.touse, 
                    maxit = 2000 , thres = 10^(-6))))
}


find.done = function(x) {
  done = rep(-1, times = length(x))
  for(i in 1:length(x)) {
    if (length(x[[i]]) == 12) {
      done[i] = (x[[i]]$lambda[length(x[[i]]$df)] == 0.0001)
    }
  }
  return(done)
}

glm.x.re <- function(x, prog, lamb.adj = 11) {
  start = prog[[x]]$lambda[length(prog[[x]]$lambda) - 1]
  matches = match(prog[[x]]$lambda,lambda.touse)
  end = lambda.touse[max(matches, na.rm = TRUE) + 1]
  
  to.add = seq(from=start, to=end, length.out = lamb.adj)
  new.lambda = unique(sort(c(to.add, prog[[x]]$lambda, lambda.touse), decreasing = TRUE))
  maxit2 = 2000 * (prog[[x]]$iters+1)
  #return(list(new.lambda, maxit2))
  return(try(glmnet(dm[,-x], dm[,x],
                    family="poisson", lambda = new.lambda, 
                    maxit = maxit2 , thres = 10^(-6))))
}

save(lambda.touse, glm.x, find.done, glm.x.re, file = "helper.Rdata")
clusterEvalQ(clust1, load("helper.Rdata"))


snow.time(parLapply(clust1, x = 1:30, fun = glm.x))


results = list()
N.vertex = dim(dm)[2]

# First attempt at fit
## 6767 to do, do 24 at a time. 
for(k in 1:300) {
  j = intersect((length(results) + 1 ): (length(results) + 24), 1:N.vertex)
  if (length(j) > 0) {
    print(c(min(j), max(j)))
    print(date())
    print(snow.time(( results[j] = parLapply(clust1, x = j, fun = glm.x) )) )
    if (k %% 10 == 0) {
      print("Saving....")
      print(date())
      save(results, file = "cb500.fitnew.Rdata")
    }
  }
}
save(results, file = "cb500.fitnew.Rdata")

# re-running non-finished trials
nodes=12

for(run in 2:10) {
  is.done = find.done(results)
  to.do = which(is.done == 0)
  prog = list()
  
  if (length(to.do) > 0) {
    
    
    for(j in 1:N.vertex) {
      if (is.done[j] == 0) {
        prog[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
      }
    }
    
    to.run = NULL
    for(i in 1:500) {
      prog[to.run] = paste(run ,"<- iteration done")
      to.run = to.do[intersect((nodes*(i-1)+1):(nodes*(i)), 1:length(to.do))]
      to.run = to.run[!is.na(to.run)]
      if (length(to.run) > 0) {
        print(paste("Re-iter:", run))
        print(to.run)
        print(date())
        print(snow.time((results[to.run] = parLapply(clust1, x = to.run, 
                                                     fun = glm.x.re, 
                                                     prog = prog))) )
      }
    }
    save(results, file = "cb500.fitnew.Rdata")
    
    
  }
}

for(run in 11:25) {
  is.done = find.done(results)
  to.do = which(is.done == 0)
  prog = list()
  
  if (length(to.do) > 0) {
    
    
    for(j in 1:N.vertex) {
      if (is.done[j] == 0) {
        prog[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
      }
    }
    
    to.run = NULL
    for(i in 1:500) {
      prog[to.run] = paste(run ,"<- iteration done")
      to.run = to.do[intersect((nodes*(i-1)+1):(nodes*(i)), 1:length(to.do))]
      to.run = to.run[!is.na(to.run)]
      if (length(to.run) > 0) {
        print(paste("Re-iter:", run))
        print(to.run)
        print(date())
        print(snow.time((results[to.run] = parLapply(clust1, x = to.run, 
                                                     fun = glm.x.re, 
                                                     prog = prog,
                                                     lamb.adj = (run*2) + 1) )) )
      }
    }
    
  }
}
save(results, file = "cb500.fitnew.Rdata")


load("refits.Rdata")
results = list()
N.vertex = dim(dm)[2]

# First attempt at fit
## 6767 to do, do 24 at a time. 
j = iggy
print(j)
print(date())
print(snow.time(( results[j] = parLapply(clust1, x = j, fun = glm.x) )) )

save(results, iggy, file = "refits.Rdata")




nodes = 12

for(run in 2:10) {
  is.done = find.done(results[iggy])
  to.do = iggy[which(is.done == 0)]
  prog = list()
  
  if (length(to.do) > 0) {
    
    
    for(j in to.do) {
      prog[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
    }
    
    to.run = NULL
    for(i in 1:500) {
      prog[to.run] = paste(run ,"<- iteration done")
      to.run = to.do[intersect((nodes*(i-1)+1):(nodes*(i)), 1:length(to.do))]
      to.run = to.run[!is.na(to.run)]
      if (length(to.run) > 0) {
        print(paste("Re-iter:", run))
        print(to.run)
        print(date())
        print(snow.time((results[to.run] = parLapply(clust1, x = to.run, 
                                                     fun = glm.x.re, 
                                                     prog = prog))) )
      }
    }
    
  }
}

save(results, iggy, file = "refits.Rdata")



for(run in 11:25) {
  is.done = find.done(results[iggy])
  to.do = iggy[which(is.done == 0)]
  prog = list()
  
  if (length(to.do) > 0) {
    
    
    for(j in to.do) {
      prog[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
    }
    
    to.run = NULL
    for(i in 1:500) {
      prog[to.run] = paste(run ,"<- iteration done")
      to.run = to.do[intersect((nodes*(i-1)+1):(nodes*(i)), 1:length(to.do))]
      to.run = to.run[!is.na(to.run)]
      if (length(to.run) > 0) {
        print(paste("Re-iter:", run))
        print(to.run)
        print(date())
        print(snow.time((results[to.run] = parLapply(clust1, x = to.run, 
                                                     fun = glm.x.re, 
                                                     prog = prog,
                                                     lamb.adj = (run*2) + 1) )) )
      }
    }
    
  }
}

save(results, iggy, file = "refits.Rdata")




###### fitting for biography id'd model
## NOTE THAT biom.matrix is not updated for the removal of 16 names: 

# to.remove = c(2234, 148, 70, 965, 1856, 2126, 850, 2017, 2116, 847, 1745, 181, 490)
# to.combine = c(217, 256, 1401, 2862)
# fin.toremove = sort(c(to.remove, to.combine[2:4]))



## run on banshee
library(snow)
library(glmnet)

clust1 = makeSOCKcluster(c("node68","node69","node70", "node71", "node72",
                           "node73", "node74", "node75", "node76", "node77",
                           "node78","node79"))

# pw ...


# stopCluster(clust1)

savefile.name = "cb500i1.bio.fit.Rdata"
clusterEvalQ(clust1, library(glmnet))
clusterEvalQ(clust1, load("cb500i1.dm.Rdata"))
clusterEvalQ(clust1, load("comb.sp500.bio.Rdata"))
load("cb500i1.dm.Rdata")
load("comb.sp500.bio.Rdata")

to.rm.bm = c(70, 148, 181, 256, 490, 847, 850, 965, 1401, 1745, 1856, 
             2017, 2116, 2126, 2234, 2862)
biom.matrix = biom.matrix[,-to.rm.bm]
clusterEvalQ(clust1, (to.rm.bm = c(70, 148, 181, 256, 490, 847, 850, 965, 1401, 1745, 1856, 
                                   2017, 2116, 2126, 2234, 2862 ) ))
clusterEvalQ(clust1, biom.matrix <- biom.matrix[,-to.rm.bm])
clusterEvalQ(clust1, dim(biom.matrix))

to.fit = which(colSums(biom.matrix) > 0)

lambda.list = list()
lambda.list[[1]] = unique(c(seq(from=1000, to=200, by = -50),
                            seq(from=200, to=10, by = -5),
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
                            seq(from=.002, to = .001, by = -.00002)))
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))


glm.x = function(x) {
  return(try(glmnet(cBind(dm[,-x], biom.matrix[,x]), dm[,x],
                    family="poisson", lambda = lambda.touse, 
                    penalty.factor = c(rep(1, times = 6750), 0.02), 
                    maxit = 2000 , thres = 10^(-6))))
}


find.done = function(x) {
  done = rep(-1, times = length(x))
  for(i in 1:length(x)) {
    if (length(x[[i]]) == 12) {
      done[i] = (x[[i]]$lambda[length(x[[i]]$df)] == 0.001)
    }
  }
  return(done)
}

glm.x.re <- function(x, prog, lamb.adj = 11) {
  start = prog[[x]]$lambda[length(prog[[x]]$lambda) - 1]
  matches = match(prog[[x]]$lambda,lambda.touse)
  end = lambda.touse[max(matches, na.rm = TRUE) + 1]
  
  to.add = seq(from=start, to=end, length.out = lamb.adj)
  new.lambda = unique(sort(c(to.add, prog[[x]]$lambda, lambda.touse), decreasing = TRUE))
  maxit2 = 2000 * (prog[[x]]$iters+1)
  #return(list(new.lambda, maxit2))
  return(try(glmnet(cBind(dm[,-x], biom.matrix[,x]), dm[,x],
                    family="poisson", lambda = new.lambda, 
                    penalty.factor = c(rep(1, times = 6750), 0.02),
                    maxit = maxit2 , thres = 10^(-6))))
}

save(lambda.touse, glm.x, find.done, glm.x.re, file = "helper.Rdata")
clusterEvalQ(clust1, load("helper.Rdata"))


snow.time(( test = parLapply(clust1, x = 1:20, fun = glm.x) ))


results = list()
N.vertex = length(to.fit)

# First attempt at fit
## 6767 to do, do 24 at a time. 
for(k in 1:300) {
  j = to.fit[intersect((1:24) + 24 *(k-1), 1:N.vertex)]
  if (length(j) > 0) {
    print(c(min(j), max(j)))
    print(date())
    print(snow.time(( results[j] = parLapply(clust1, x = j, fun = glm.x) )) )
    if (k %% 10 == 0) {
      print("Saving....")
      print(date())
      save(results, file = savefile.name)
    }
  }
}
save(results, file = savefile.name)

# re-running non-finished trials
nodes=12

for(run in 6:15) {
  is.done = find.done(results)
  to.do = intersect(which(is.done == 0), to.fit)
  prog = list()
  
  if (run > 10) {
    nn = 51
  } else {
    nn = 21
  }
  
  if (length(to.do) > 0) {
    
    
    for(j in to.fit) {
      if (is.done[j] == 0) {
        prog[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
      }
    }
    
    to.run = NULL
    for(i in 1:500) {
      prog[to.run] = paste(run ,"<- iteration done")
      to.run = to.do[intersect((nodes*(i-1)+1):(nodes*(i)), 1:length(to.do))]
      to.run = to.run[!is.na(to.run)]
      if (length(to.run) > 0) {
        print(paste("Re-iter:", run))
        print(to.run)
        print(date())
        print(snow.time((results[to.run] = parLapply(clust1, x = to.run, 
                                                     fun = glm.x.re, 
                                                     prog = prog,
                                                     lamb.adj = nn) )) )
      }
    }
    save(results, file = "test.fit.Rdata")
    
  }
}


## 500 g2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## fixing unfinished fits for whatever reason. 
load("cb500g2.fit.test.Rdata")
load("cb500g2i1.dm.Rdata")

find.done = function(x) {
  done = rep(-1, times = length(x))
  for(i in 1:length(x)) {
    if (length(x[[i]]) == 12) {
      done[i] = (x[[i]]$lambda[length(x[[i]]$df)] == 0.001)
    }
  }
  return(done)
}

to.fix = which(find.done(results) != 1) ## cant fix. respose == 0
# thus, ignore to.fix

## grabbing results: 

# want to store biggest lambda where edge is added
# row1 = own.edges
# row2 = other.edges
# row3 = own.final.edge.wt
# row4 = other.final.edge.wt


nam = c("John Milton", "James Harrington", "Thomas Hobbes", "John Wilmot", "James II", 
        "Peter Paul Rubens", "Algernon Sidney", "Andrew Marvell", "Michael Drayton", 
        "William Morgan", "John Drummond", "George Herbert", "Philip Sidney",
        "Charles I", "Margaret Cavendish", "John Cook", "Henry Killigrew", 
        "John Kennedy", "William Herbert")
ind = c(1204,          886,                2101,            4691,          895,        
        5106,                54,                66,               1616,              
        2409,             1057,            588,              1699,
        214,         4834,                 1032,        741,
        1158,           2363) 

res.mat.500g2 = matrix(NA, nrow = 4*length(ind), ncol = 6751)

smallest.nonzero.ind <- function(x) {
  return(which(x != 0)[1])
}


for(j in 1:length(ind)) {
  print(j)
  
  temp = apply(results[[ind[j]]]$beta, 1, smallest.nonzero.ind)
  res.mat.500g2[4*(j-1) + 1,-ind[j]] = results[[ind[j]]]$lambda[temp]
  
  res.mat.500g2[4*(j-1) + 2, -ind[j]] = results[[ind[j]]]$beta[,length(results[[ind[j]]]$df)]
  
}

for(k in setdiff(1:6751, to.fix)) {
  if (k %% 50 == 0) { print(k) }
  for(j in 1:length(ind)) {
    f = length(results[[k]]$df)
    if (ind[j] > k) {
      res.mat.500g2[4*(j-1) + 3, k] = results[[k]]$lambda[smallest.nonzero.ind(results[[k]]$beta[ind[j] - 1,])]
      res.mat.500g2[4*(j-1) + 4, k] = results[[k]]$beta[ind[j] - 1,f]
    } else if (ind[j] < k) {
      res.mat.500g2[4*(j-1) + 3, k] = results[[k]]$lambda[smallest.nonzero.ind(results[[k]]$beta[ind[j],])]
      res.mat.500g2[4*(j-1) + 4, k] = results[[k]]$beta[ind[j],f]
    }
  }
}
save(res.mat.500g2, file = "resmat.500g2.Rdata")

##################################
## 500 ~~~~~~~~~~~~~~~~~~~~~~~~~
load("cb500.fit.Rdata")
load("cb500i1.dm.Rdata")
library(glmnet)

## fixing unfinished fits for whatever reason. 

find.done = function(x) {
  done = rep(-1, times = length(x))
  for(i in 1:length(x)) {
    if (length(x[[i]]) == 12) {
      done[i] = (x[[i]]$lambda[length(x[[i]]$df)] == 0.001)
    }
  }
  return(done)
}

to.fix = which(find.done(results) != 1) 

x = 4819

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
lambda.list[[3]] = unique(c(seq(from=.05, to = .021, by = -.001),
                            seq(from=.021, to = .0204, by = -.00001),
                            seq(from=.0204, to = .01, by = -.0002),
                            seq(from=.01, to = .005, by = -.0001),
                            seq(from=.005, to = .003, by = -.0001),
                            seq(from=.003, to = .002, by = -.00005),
                            seq(from=.002, to = .001, by = -.00002)))
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))

a = try(glmnet(dm[,-x], dm[,x],
               family="poisson", lambda = lambda.touse, 
               maxit = 10000 , thres = 10^(-6)))
results[[4819]] = a

save(results, file = "cb500.fit.Rdata")  ## update fit. 

## grabbing results: 

# want to store biggest lambda where edge is added
# row1 = own.edges
# row2 = other.edges
# row3 = own.final.edge.wt
# row4 = other.final.edge.wt
load("cb500.fit.Rdata")
nam = c("John Milton", "James Harrington", "Thomas Hobbes", "John Wilmot", "James II", 
        "Peter Paul Rubens", "Algernon Sidney", "Andrew Marvell", "Michael Drayton", 
        "William Morgan", "John Drummond", "George Herbert", "Philip Sidney",
        "Charles I", "Margaret Cavendish", "John Cook", "Henry Killigrew", 
        "John Kennedy", "William Herbert")
ind = c(1204,          886,                2101,            4691,          895,        
        5106,                54,                66,               1616,              
        2409,             1057,            588,              1699,
        214,         4834,                 1032,        741,
        1158,           2363) 

res.mat.500 = matrix(NA, nrow = 4*length(ind), ncol = 6751)

smallest.nonzero.ind <- function(x) {
  return(which(x != 0)[1])
}

for(j in 1:length(ind)) {
  print(j)
  
  temp = apply(results[[ind[j]]]$beta, 1, smallest.nonzero.ind)
  res.mat.500[4*(j-1) + 1,-ind[j]] = results[[ind[j]]]$lambda[temp]
  
  res.mat.500[4*(j-1) + 2, -ind[j]] = results[[ind[j]]]$beta[,length(results[[ind[j]]]$df)]
  
}

for(k in 1:6751) {
  if (k %% 50 == 0) { print(k) }
  for(j in 1:length(ind)) {
    f = length(results[[k]]$df)
    if (ind[j] > k) {
      res.mat.500[4*(j-1) + 3, k] = results[[k]]$lambda[smallest.nonzero.ind(results[[k]]$beta[ind[j] - 1,])]
      res.mat.500[4*(j-1) + 4, k] = results[[k]]$beta[ind[j] - 1,f]
    } else if (ind[j] < k) {
      res.mat.500[4*(j-1) + 3, k] = results[[k]]$lambda[smallest.nonzero.ind(results[[k]]$beta[ind[j],])]
      res.mat.500[4*(j-1) + 4, k] = results[[k]]$beta[ind[j],f]
    }
  }
}
save(res.mat.500, file = "resmat.500.Rdata")

#####
## orig ~~~~~~~~~~~~~~~~~~~~~~~~~
load("cbnosp.fit.Rdata")
load("cbnospi1.dm.Rdata")
library(glmnet)

## fixing unfinished fits for whatever reason. 

find.done = function(x) {
  done = rep(-1, times = length(x))
  for(i in 1:length(x)) {
    if (length(x[[i]]) == 12) {
      done[i] = (x[[i]]$lambda[length(x[[i]]$df)] == 0.001)
    }
  }
  return(done)
}

to.fix = which(find.done(results) != 1) 

x = 1305

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
                            seq(from=.002, to = .001, by = -.00002)))
lambda.list[[4]] = unique(c(seq(from=.075, to = .07, by = -.0005),
                            seq(from=.055, to = .05, by = -.0005),
                            seq(from=.0026, to = .002, by = -.00002),
                            seq(from=.0024, to = .0023, by = -.000001),
                            seq(from=.011, to = .01, by = -.000005),
                            seq(from=.029, to = .027, by = -.0002) ))
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))

a = try(glmnet(dm[,-x], dm[,x],
               family="poisson", lambda = lambda.touse, 
               maxit = 25000 , thres = 10^(-7)))

results[[x]] = a

x = 2325

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
                            seq(from=.002, to = .001, by = -.00002)))
lambda.list[[4]] = unique(c(seq(from=.36, to = .32, by = -.002),
                            seq(from=.0025, to = .002, by = -.000005) ))
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))

a = try(glmnet(dm[,-x], dm[,x],
               family="poisson", lambda = lambda.touse, 
               maxit = 20000 , thres = 10^(-6)))

results[[x]] = a


save(results, file = "cbnosp.fit.Rdata")  ## update fit. 

## grabbing results: 

# want to store biggest lambda where edge is added
# row1 = own.edges
# row2 = other.edges
# row3 = own.final.edge.wt
# row4 = other.final.edge.wt

nam = c("John Milton", "James Harrington", "Thomas Hobbes", "John Wilmot", "James II", 
        "Peter Paul Rubens", "Algernon Sidney", "Andrew Marvell", "Michael Drayton", 
        "William Morgan", "John Drummond", "George Herbert", "Philip Sidney",
        "Charles I", "Margaret Cavendish", "John Cook", "Henry Killigrew", 
        "John Kennedy", "William Herbert")
ind = c(1204,          886,                2101,            4691,          895,        
        5106,                54,                66,               1616,              
        2409,             1057,            588,              1699,
        214,         4834,                 1032,        741,
        1158,           2363) 

res.mat.nosp = matrix(NA, nrow = 4*length(ind), ncol = 6751)

smallest.nonzero.ind <- function(x) {
  return(which(x != 0)[1])
}

for(j in 1:length(ind)) {
  print(j)
  
  temp = apply(results[[ind[j]]]$beta, 1, smallest.nonzero.ind)
  res.mat.nosp[4*(j-1) + 1,-ind[j]] = results[[ind[j]]]$lambda[temp]
  
  res.mat.nosp[4*(j-1) + 2, -ind[j]] = results[[ind[j]]]$beta[,length(results[[ind[j]]]$df)]
  
}

for(k in 1:6751) { 
  if (k %% 50 == 0) { print(k) }
  for(j in 1:length(ind)) {
    f = length(results[[k]]$df)
    if (ind[j] > k) {
      res.mat.nosp[4*(j-1) + 3, k] = results[[k]]$lambda[smallest.nonzero.ind(results[[k]]$beta[ind[j] - 1,])]
      res.mat.nosp[4*(j-1) + 4, k] = results[[k]]$beta[ind[j] - 1,f]
    } else if (ind[j] < k) {
      res.mat.nosp[4*(j-1) + 3, k] = results[[k]]$lambda[smallest.nonzero.ind(results[[k]]$beta[ind[j],])]
      res.mat.nosp[4*(j-1) + 4, k] = results[[k]]$beta[ind[j],f]
    }
  }
}
save(res.mat.nosp, file = "resmat.nosp.Rdata")








#####
## 500sp + bio ~~~~~~~~~~~~~~~~~~~~~~~~~

## fixing unfinished fits for whatever reason.
load("test.fit.Rdata")
load("cb500i1.dm.Rdata")
load("comb.sp500.bio.Rdata")
library(glmnet)

to.rm.bm = c(70, 148, 181, 256, 490, 847, 850, 965, 1401, 1745, 1856, 
             2017, 2116, 2126, 2234, 2862)
biom.matrix = biom.matrix[,-to.rm.bm]
to.fit = which(colSums(biom.matrix) > 0)

find.done = function(x) {
  done = rep(-1, times = length(x))
  for(i in 1:length(x)) {
    if (length(x[[i]]) == 12) {
      done[i] = (x[[i]]$lambda[length(x[[i]]$df)] == 0.001)
    }
  }
  return(done)
}

to.fix = which(find.done(results) != 1) 
intersect(to.fit, to.fix)

x = 3079

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
                            seq(from=.002, to = .001, by = -.00002)))
lambda.list[[4]] = unique(c(seq(from=.00702, to = .0069, by = -.000001),
                            seq(from=.0025, to = .0023, by = -.000001) ))
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))

a = try(glmnet(cBind(dm[,-x], biom.matrix[,x]), dm[,x],
               family="poisson", lambda = lambda.touse, 
               penalty.factor = c(rep(1, times = 6750), 0.02), 
               maxit = 30000 , thres = 15*10^(-5)))

results[[x]] = a # DID NOT FIX; whatever. 

save(results, file = "test.fit.Rdata")  ## update fit. 

## grabbing results: 

# want to store biggest lambda where edge is added
# row1 = own.edges
# row2 = other.edges
# row3 = own.final.edge.wt
# row4 = other.final.edge.wt

load("resmat.500.Rdata")

nam = c("John Milton", "James Harrington", "Thomas Hobbes", "John Wilmot", "James II", 
        "Peter Paul Rubens", "Algernon Sidney", "Andrew Marvell", "Michael Drayton", 
        "William Morgan", "John Drummond", "George Herbert", "Philip Sidney",
        "Charles I", "Margaret Cavendish", "John Cook", "Henry Killigrew", 
        "John Kennedy", "William Herbert")
ind = c(1204,          886,                2101,            4691,          895,        
        5106,                54,                66,               1616,              
        2409,             1057,            588,              1699,
        214,         4834,                 1032,        741,
        1158,           2363) 

res.mat.500i = matrix(NA, nrow = 4*length(ind), ncol = 6751)

smallest.nonzero.ind <- function(x) {
  return(which(x != 0)[1])
}

for(j in 1:length(ind)) {
  print(j)
  
  if (length(intersect(to.fit, ind[j]) == 1)) {
    temp = apply(results[[ind[j]]]$beta[-6751,], 1, smallest.nonzero.ind)
    res.mat.500i[4*(j-1) + 1,-ind[j]] = results[[ind[j]]]$lambda[temp]
    
    res.mat.500i[4*(j-1) + 2, -ind[j]] = results[[ind[j]]]$beta[-6751,length(results[[ind[j]]]$df)]
  } else {
    res.mat.500i[4*(j-1) + 1,] = res.mat.500[4*(j-1) + 1,]
    res.mat.500i[4*(j-1) + 2,] = res.mat.500[4*(j-1) + 2,]
  }
  
}

for(k in 1:6751) {
  if (k %% 50 == 0) { print(k) }
  if (length(intersect(k, to.fit)) == 1) {
    for(j in 1:length(ind)) {
      f = length(results[[k]]$df)
      if (ind[j] > k) {
        res.mat.500i[4*(j-1) + 3, k] = results[[k]]$lambda[smallest.nonzero.ind(results[[k]]$beta[ind[j] - 1,])]
        res.mat.500i[4*(j-1) + 4, k] = results[[k]]$beta[ind[j] - 1,f]
      } else if (ind[j] < k) {
        res.mat.500i[4*(j-1) + 3, k] = results[[k]]$lambda[smallest.nonzero.ind(results[[k]]$beta[ind[j],])]
        res.mat.500i[4*(j-1) + 4, k] = results[[k]]$beta[ind[j],f]
      }
    }
  } else {
    res.mat.500i[4 * ((1:length(ind)) - 1) + 3, k] = res.mat.500[4 * ((1:length(ind)) - 1) + 3, k]
    res.mat.500i[4 * ((1:length(ind)) - 1) + 4, k] = res.mat.500[4 * ((1:length(ind)) - 1) + 4, k]
  }
}
save(res.mat.500i, file = "resmat.500i.Rdata")


## finding cors
load("comb500.datamat.Rdata")


to.remove = c(2234, 148, 70, 965, 1856, 2126, 850, 2017, 2116, 847, 1745, 181, 490)
to.combine = c(217, 256, 1401, 2862)

# combine cols 1st
s = apply(cbn.datamat[,to.combine], 1, sum)
cbn.datamat[,217] = s
cbn.names[217] = paste(cbn.names[to.combine], collapse = "|")

fin.toremove = sort(c(to.remove, to.combine[2:4]))
cbn.datamat = cbn.datamat[,-fin.toremove]
cbn.names = cbn.names[-fin.toremove]

ind = c(1204,          886,                2101,            4691,          895,        
        5106,                54,                66,               1616,              
        2409,             1057,            588,              1699,
        214,         4834,                 1032,        741,
        1158,           2363)

system.time(( rescor = cor(cbn.datamat[,ind], cbn.datamat) ))
save(rescor, file = "corrs.Rdata")

## combining:
load("resmat.nosp.Rdata")
load("resmat.500.Rdata")
load("resmat.500i.Rdata")
load("resmat.500g2.Rdata")
load("cb500i1.dm.Rdata")

nam = c("John Milton", "James Harrington", "Thomas Hobbes", "John Wilmot", "James II", 
        "Peter Paul Rubens", "Algernon Sidney", "Andrew Marvell", "Michael Drayton", 
        "William Morgan", "John Drummond", "George Herbert", "Philip Sidney",
        "Charles I", "Margaret Cavendish", "John Cook", "Henry Killigrew", 
        "John Kennedy", "William Herbert")
ind = c(1204,          886,                2101,            4691,          895,        
        5106,                54,                66,               1616,              
        2409,             1057,            588,              1699,
        214,         4834,                 1032,        741,
        1158,           2363)

library(Matrix)

# want to store biggest lambda where edge is added
# row1 = own.edges
# row3 = other.edges
# row2 = own.final.edge.wt
# row4 = other.final.edge.wt
j = 1
ind[j]

#cor
left.mat = t(dm[,ind])
mult.res = left.mat %*% dm
t(dm) %*% dm -> all.mult

xsq = apply(left.mat^2, 1, sum)
ysq = apply(all.mult, 1, sum)
xsum = apply(left.mat, 1, sum)
ysum = colSums(dm)

corrs = matrix(0, ncol = 19, nrow = 6751)

for(j in 1:6751) {
  for(k in 1:19) {
    corrs[j, k] = (95206 * mult.res[k,j] - xsum[k]*ysum[j]) / 
      sqrt( (95206 * xsq[k] - (xsum[k])^2 ) * (95206 * ysq[j] -(ysum[j])^2 ))
  }
  print(j)
}
save(corrs, file = "corrs.Rdata")

#listing by index
# 1-3: by lambda (or), by lambda(fit.self), by lambda (other.fits)
# 4: by positive magnitude edges 
# 5: by overall magnitude edges
# 6: by correlations
fin.list = list()
for(j in 1:19) {
  fin.list[[j]] = 0
}

name.lists = list()
val.lists = list()
resm = res.mat.500i
for(j in 1:19) {
  print(j)
  print(nam[j])
  name.lists[[j]] = list()
  val.lists[[j]] = list()
  
  uses = sort(union(which(!is.na(resm[4*j - 3, ])), 
                    which(!is.na(resm[4*j - 1, ])) ))
  rs = apply(resm[4*j - c(1,3),uses],  2, max, na.rm = 1)
  lambdas = rs[rev(order(rs))]
  inds = uses[rev(order(rs))]
  names = cbn.names[uses][rev(order(rs))]
  
  uses1 = sort(union(which(resm[4*j - 2, ] != 0), 
                     which(resm[4*j - 0, ] != 0) ))
  rs1 = apply(resm[4*j - c(0, 2),uses1],  2, mean, na.rm = 1)
  lambdas1 = rs1[rev(order(rs1))]
  inds1 = uses1[rev(order(rs1))]
  names1 = cbn.names[uses1][rev(order(rs1))]
  
  name.lists[[j]][[1]] = inds
  val.lists[[j]][[1]] = lambdas
  
  u = which(!is.na(resm[4*j - 3, ]))
  l = res.mat.500g2[4*j-3, u]
  name.lists[[j]][[2]] = u[rev(order(l))]
  val.lists[[j]][[2]] = sort(l, decreasing = TRUE)
  
  u = which(!is.na(resm[4*j - 1, ]))
  l = resm[4*j-2, u]
  name.lists[[j]][[3]] = u[rev(order(l))]
  val.lists[[j]][[3]] = sort(l, decreasing = TRUE)
  
  name.lists[[j]][[4]] = inds1
  val.lists[[j]][[4]] = lambdas1
  
  r = abs(rs1)
  name.lists[[j]][[5]] = uses1[rev(order(r))]
  val.lists[[j]][[5]] = sort(r, decreasing = TRUE)
  
  i = which(corrs[,j] > 0.0001) 
  name.lists[[j]][[6]] = i[rev(order(corrs[i,j]))]
  val.lists[[j]][[6]] = corrs[name.lists[[j]][[6]],j]
}


b = matrix(NA, nrow = 6751, ncol = 6)
for(j in 1:6) {
  t = length(name.lists[[1]][[j]])
  b[name.lists[[1]][[j]],j] = rank(val.lists[[1]][[j]])
}
cor(b, use = "pairwise.complete.obs", method="spearman" )

how.many.tokeep = 30

for(j in 1:19) {
  res = NULL
  for(k in 1:6) {
    a = min(how.many.tokeep, length(name.lists[[j]][[k]]))
    res = union(res, name.lists[[j]][[k]][1:a])
  }
  fin.list[[j]] = c(fin.list[[j]],setdiff(res, ind[j]))
}

save(fin.list, name.lists, val.lists, file = "modelres.Rdata")

for(j in 1:19) {
  fin.list[[j]] = unique(setdiff(fin.list[[j]], 0))
  fin.list[[j]] = fin.list[[j]][sample(1:length(fin.list[[j]]), size = length(fin.list[[j]]))]
}
res.n = rep("", times = 2064)
res.ind = matrix(NA, ncol = 7, nrow = 2064)

cur.ind = 1
for(j in 1:19) {
  cur.ind = cur.ind + 1
  res.n[cur.ind] = nam[j]
  cur.ind = cur.ind + 1
  res.n[cur.ind + (1:length(fin.list[[j]]))] = cbn.names[fin.list[[j]]]
  res.ind[cur.ind + (1:length(fin.list[[j]])),1] = fin.list[[j]]
  for(k in 1:6) {
    a = match(fin.list[[j]], name.lists[[j]][[k]])
    b = which(!is.na(a))
    res.ind[(cur.ind + (1:length(fin.list[[j]])) )[b], k+1] = val.lists[[j]][[k]][a[b]]
  }
  cur.ind = cur.ind + 1 + length(fin.list[[j]])
}

toprint = cbind(res.n, res.ind)
write.csv(toprint, file = "test.csv")

# in resmats 
# row  1 = own.edges
# row  3 = other.edges
# row  2 = own.final.edge.wt
# row  4 = other.final.edge.wt


all.res.mat = array(NA, dim = c(6751, 24, 19))
# 1 *6 - nosp
# 2 *6 - 500
# 3 *6 - 500g2
# 4 *6 - 500i

#1-6: 
# 1-3: by lambda (or), by lambda(fit.self), by lambda (other.fits)
# 4: by abs magnitude edges 
# 5: by pos magnitude edges
# 6: by correlations (so far only for 2/4)
load("resmat.nosp.Rdata")
for(p in 1:19) {
  all.res.mat[,2,p] = res.mat.nosp[4*p - 3,]
  all.res.mat[,3,p] = res.mat.nosp[4*p - 1,]
  all.res.mat[,5,p] = apply(res.mat.nosp[4*p - c(0,2),], 2, mean, na.rm = TRUE)
}

load("resmat.500.Rdata")
for(p in 1:19) {
  all.res.mat[,2+6,p] = res.mat.500[4*p - 3,]
  all.res.mat[,3+6,p] = res.mat.500[4*p - 1,]
  all.res.mat[,5+6,p] = apply(res.mat.500[4*p - c(0,2),], 2, mean, na.rm = TRUE)
}

load("resmat.500g2.Rdata")
for(p in 1:19) {
  all.res.mat[,2+12,p] = res.mat.500g2[4*p - 3,]
  all.res.mat[,3+12,p] = res.mat.500g2[4*p - 1,]
  all.res.mat[,5+12,p] = apply(res.mat.500g2[4*p - c(0,2),], 2, mean, na.rm = TRUE)
}

load("resmat.500i.Rdata")
for(p in 1:19) {
  all.res.mat[,2+18,p] = res.mat.500i[4*p - 3,]
  all.res.mat[,3+18,p] = res.mat.500i[4*p - 1,]
  all.res.mat[,5+18,p] = apply(res.mat.500i[4*p - c(0,2),], 2, mean, na.rm = TRUE)
}

load("corrs.Rdata")
for(p in 1:19) {
  all.res.mat[,12,p] = corrs[,p]
}

# generating rows 1, 4
for(j in 0:3) {
  for(p in 1:19) {
    all.res.mat[,4 + (j*6),p] = abs(all.res.mat[,5 + (j*6),p])
    
    d = apply(all.res.mat[,2:3 + (j*6),p], 1, function(x) {
      if (all(is.na(x))) { return(NA) } else {
        return(max(x, na.rm = 1)) } } )
    all.res.mat[,1 + (j*6),p] = d
  }
}

save(all.res.mat, file = "all.res.mat.Rdata")

find.ranks = function(x, n) {
  a = which(x > 0)
  N = min(n, length(a))
  b = order(x, decreasing = TRUE)
  return(which(x >= x[b[N]]))
  #return(b[1:N])  
}

new.finlist = list()
for( p in 1:19) {
  a = NULL
  for(j in 1:24) {
    b = find.ranks(x = all.res.mat[,j,p], n = 30)
    if (length(b) > 1) {
      a = c(a, b)
    }
  }
  new.finlist[[p]] = setdiff(unique(a), ind[p])
  print(p)
}


## James harrington adjustment: for prior mistake
new.finlist[[2]] = c(new.finlist[[2]], 1065, 2664, 650, 3263, 6218, 2459, 6223)

save(new.finlist, file = "new.finlist.Rdata")


m = read.csv("test.csv")
q = c(which(is.na(m[,3])), 2065)

for(z in 1:19){
  r = m[q[3*z]:q[3*z+1],3]
  for(j in 1:24) {
    print(
      c(length(find.ranks(x = all.res.mat[,j,z], n = 30)),
        length(intersect(find.ranks(x = all.res.mat[,j,z],n = 30), r) )
      ))
  }
}

intersect(find.ranks(x = all.res.mat[,1,1],n = 25), fin.list[[1]])



resample = function(x) {
  return(sample(x, size = length(x)))
}

tot.lens = sapply(new.finlist, length)
tot.len = sum(tot.lens)

fin.res.nam = matrix("", ncol = 2, nrow = tot.len)
fin.res.ver = rep(0, times = tot.len)
val.mat = matrix(NA, ncol = 24, nrow = tot.len)

cur.pos = 1
for(p in 1:19) {
  a = resample(new.finlist[[p]])
  cur.use = cur.pos:(cur.pos + tot.lens[p] - 1)
  fin.res.nam[cur.use,1] = nam[p]
  fin.res.nam[cur.use,2] = cbn.names[a]
  fin.res.ver[cur.use] = a
  val.mat[cur.use,] = all.res.mat[a,,p]
  cur.pos = cur.pos + tot.lens[p]
}

tosave = data.frame(fin.res.nam, fin.res.ver, val.mat)
colnames(tosave) = c("Base Person", "Link", "ID",
                     "nosp.lor", "nosp.lself", "nosp.lother", "nosp.absfin", "nosp.posfin", "na",
                     "500.lor", "500.lself", "500.lother", "500.absfin", "500.posfin", "500.corr",
                     "500g2.lor", "500g2.lself", "500g2.lother", "500g2.absfin", "500g2.posfin", "na",
                     "500i.lor", "500i.lself", "500i.lother", "500i.absfin", "500i.posfin", "na")

write.csv(tosave, file = "test2.csv")
