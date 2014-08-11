#@S Old Code: fits Poisson Graphical Lasso

# TODO: Clear this file

###################################################################
###################################################################
# fixing datamatrix
###################################################################
# Fitting only Milton
# on banshee
# 2/13
load("1230sp.datamat.sparse.Rdata")
iggy = c(488, 545 ,881, 1360, 1487)
sparse.dm = sparse.dm[,-iggy]
dm.names = dm.names[-iggy]

save(dm.names, sparse.dm, dm.docnums, file = "0213sp.datamat.sparse.Rdata")
###################################################################
###################################################################





# TODO: In middle of updating this code: 

########### Fitting full model for split, full docs, without indicator
## updated 12/31 for running with smaller lambdas
## run on banshee

ZZ.run.poisglasso = function() {
  library(snow)
  library(glmnet)
  
  clust1 = makeSOCKcluster(c("node68","node69","node70", "node71", "node72",
                             "node73", "node74", "node75", "node76", "node77",
                             "node78","node79"))
  

  
  samp = list()
  for(j in 1:100) {
    samp[[j]] = sample.docs()
  }
  
  
  # stopCluster(clust1)
  
  clusterEvalQ(clust1, library(glmnet))
  clusterEvalQ(clust1, load("0213sp.datamat.sparse.Rdata"))
  load("0213sp.datamat.sparse.Rdata")
  

  
  save(lambda.touse, glm.x, find.done, glm.x.re, samp, file = "helper.Rdata")
  clusterEvalQ(clust1, load("helper.Rdata"))
  load('helper.Rdata')
  
  results = list()
  N.trials = 100

  for(k in 1:30) {
    j = intersect((length(results) + 1 ): (length(results) + 24), 1:N.trials)
    if (length(j) > 0) {
      print(c(min(j), max(j)))
      print(date())
      print(snow.time(( results[j] = parLapply(clust1, x = j, fun = glm.x) )) )
    }
  }
  print("Saving....")
  save(results, file = "stabsel.Rdata")
  
  # re-running non-finished trials
  nodes=12
  
  for(run in 2:10) {
    is.done = find.done(results)
    to.do = which(is.done == 0)
    prog = list()
    
    if (length(to.do) > 0) {
      
      
      for(j in 1:N.trials) {
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
      if (run %% 3 == 1) {
        save(results, file = "stabsel.Rdata")
      }
      
    }
  }
  
  run = 9
  to.do = which(is.done == 0)
  prog = list()
  for(j in 1:N.trials) {
    if (is.done[j] == 0) {
      prog[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
    }
  }
  test = glm.x.re(x = 47, prog = prog)
  
  results[[47]] = test
  save(results, file = "stabsel.Rdata")
  
  
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
  save(results, file = "stabsel.Rdata")
  
}




######

library(glmnet)
load("stabsel.Rdata")
load("helper.Rdata")
load("0213sp.datamat.sparse.Rdata")

results[[1]]

# appearance count matrix. 452 columns (lambdas), 6288 rows (peoples, ignoring JM)

app.ct.mat = matrix(0, nrow = 6288, ncol = 912)

for(j in 1:100) {
  app.ct.mat = app.ct.mat + 
                (results[[j]]$beta[,match(lambda.touse, results[[j]]$lambda)] > 0)
  print(j)
}

smax = apply(app.ct.mat, 1, max)
hist(smax[smax > 0]/100, breaks = 20, xlab = "Fraction of Time Node is Selected",
     main = "Stability Selection Proportions (Milton)",
     sub = "Note: 0% dropped (5801 0%'s)")


dm.names[-2914][which(smax > 69)]
milton.res = data.frame(smax, dm.names[-2914])[which(smax > 29),]

milton.true = read.csv("milton.valid.csv", stringsAsFactors = FALSE, header = FALSE)[,-3]
milton.true[33,1] = "Charles I"

milton.true[,]
milton.res$expert = NA

expert = rep(NA, times = dim(milton.res)[1])
for(j in 1:length(expert)) {
  if (!is.na(toadd[j])) {
    expert[j] = milton.true[toadd[j],2]
  }
}
toadd = match(milton.true[,1], milton.res[,2])
milton.res$expert = expert

colnames(milton.res)[2] = "Name"
head(milton.res)
milton.res$lamb0.00005 = app.ct.mat[match(milton.res$Name, dm.names[-2914]),912]
milton.res$lamb0.0001 = app.ct.mat[match(milton.res$Name, dm.names[-2914]),662]
milton.res$lamb0.0005 = app.ct.mat[match(milton.res$Name, dm.names[-2914]),412]

write.csv(milton.res[order(milton.res[,1], decreasing = 1),], file = "milton.res.csv")

order(milton.res[,1], decreasing = 1)[1:20]

# plot like in paper
lambdas.toplot = 112:452
inds.toplot = order(milton.res[,1], decreasing = 1)[1:20]
inds.toplot = match(milton.res[inds.toplot,2], dm.names[-2914])

plot(-1835, -19843, xlim = c(0.2, 0) , ylim = c(0, 110), xlab = "Lambda", ylab = "Proportion",
     main = "Stability paths of Top-20")
for(j in 1:20) {
lines(lambda.touse[lambdas.toplot], app.ct.mat[inds.toplot[j],lambdas.toplot])
}


inds.toplot = order(milton.res[,1], decreasing = 1)[51:70]
inds.toplot = match(milton.res[inds.toplot,2], dm.names[-2914])

plot(-1835, -19843, xlim = c(0.2, 0) , ylim = c(0, 110), xlab = "Lambda", ylab = "Proportion",
     main = "Stability paths of 51-70")
for(j in 1:20) {
  lines(lambda.touse[lambdas.toplot], app.ct.mat[inds.toplot[j],lambdas.toplot])
}


# random sample
inds.toplot = sample(which(apply(app.ct.mat, 1, sum) > 0), size = 20)

plot(-1835, -19843, xlim = c(0.2, 0) , ylim = c(0, 110), xlab = "Lambda", ylab = "Proportion",
     main = "Stability paths of random sample (20)")
for(j in 1:20) {
  lines(lambda.touse[lambdas.toplot], app.ct.mat[inds.toplot[j],lambdas.toplot])
}








########################3
# andrew marvell

library(snow)
library(glmnet)

clust1 = makeSOCKcluster(c("node68","node69","node70", "node71", "node72",
                           "node73", "node74", "node75", "node76", "node77",
                           "node78","node79"))

# stopCluster(clust1)

clusterEvalQ(clust1, library(glmnet))
clusterEvalQ(clust1, load("0213sp.datamat.sparse.Rdata"))
load("0213sp.datamat.sparse.Rdata")



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
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 8))

glm.x = function(x) {
  return(try(glmnet(sparse.dm[samp[[x]],-169], sparse.dm[samp[[x]],169],
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
  return(try(glmnet(sparse.dm[samp[[x]],-169], sparse.dm[samp[[x]],169],
                    family="poisson", lambda = new.lambda, 
                    maxit = maxit2 , thres = 10^(-6))))
}


sample.docs = function() {
  return(sample(1:72574, size = 36287))
}

samp = list()
for(j in 1:100) {
  samp[[j]] = sample.docs()
}

save(lambda.touse, glm.x, find.done, glm.x.re, samp, file = "helperAM.Rdata")
clusterEvalQ(clust1, load("helperAM.Rdata"))
load('helperAM.Rdata')

results = list()
N.trials = 100

for(k in 1:30) {
  j = intersect((length(results) + 1 ): (length(results) + 24), 1:N.trials)
  if (length(j) > 0) {
    print(c(min(j), max(j)))
    print(date())
    print(snow.time(( results[j] = parLapply(clust1, x = j, fun = glm.x) )) )
  }
}
print("Saving....")
save(results, file = "stabselAM.Rdata")

# re-running non-finished trials
nodes=12

for(run in 2:10) {
  is.done = find.done(results)
  to.do = which(is.done == 0)
  prog = list()
  
  if (length(to.do) > 0) {
    
    
    for(j in 1:N.trials) {
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
    if (run %% 3 == 1) {
      save(results, file = "stabselAM.Rdata")
    }
    
  }
}

run = 9
to.do = which(is.done == 0)
prog = list()
for(j in 1:N.trials) {
  if (is.done[j] == 0) {
    prog[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
  }
}
test = glm.x.re(x = 47, prog = prog)

results[[47]] = test
save(results, file = "stabselAM.Rdata")


#######################################3
################ george herbert

library(snow)
library(glmnet)

clust1 = makeSOCKcluster(c("node68","node69","node70", "node71", "node72",
                           "node73", "node74", "node75", "node76", "node77",
                           "node78","node79"))

# pw ..

# stopCluster(clust1)

clusterEvalQ(clust1, library(glmnet))
clusterEvalQ(clust1, load("0213sp.datamat.sparse.Rdata"))
load("0213sp.datamat.sparse.Rdata")

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
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 8))

glm.x = function(x) {
  return(try(glmnet(sparse.dm[samp[[x]],-1345], sparse.dm[samp[[x]],1345],
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
  return(try(glmnet(sparse.dm[samp[[x]],-1345], sparse.dm[samp[[x]],1345],
                    family="poisson", lambda = new.lambda, 
                    maxit = maxit2 , thres = 10^(-6))))
}


sample.docs = function() {
  return(sample(1:72574, size = 36287))
}

samp = list()
for(j in 1:100) {
  samp[[j]] = sample.docs()
}

save(lambda.touse, glm.x, find.done, glm.x.re, samp, file = "helperGH.Rdata")
clusterEvalQ(clust1, load("helperGH.Rdata"))
load('helperGH.Rdata')

results = list()
N.trials = 100

for(k in 1:30) {
  j = intersect((length(results) + 1 ): (length(results) + 24), 1:N.trials)
  if (length(j) > 0) {
    print(c(min(j), max(j)))
    print(date())
    print(snow.time(( results[j] = parLapply(clust1, x = j, fun = glm.x) )) )
  }
}
print("Saving....")
save(results, file = "stabselGH.Rdata")

# re-running non-finished trials
nodes=12

for(run in 2:10) {
  is.done = find.done(results)
  to.do = which(is.done == 0)
  prog = list()
  
  if (length(to.do) > 0) {
    
    
    for(j in 1:N.trials) {
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
    if (run %% 3 == 1) {
      save(results, file = "stabselGH.Rdata")
    }
    
  }
}

run = 9
to.do = which(is.done == 0)
prog = list()
for(j in 1:N.trials) {
  if (is.done[j] == 0) {
    prog[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
  }
}
test = glm.x.re(x = 47, prog = prog)

results[[47]] = test
save(results, file = "stabselGH.Rdata")




#######################################3
################ james harrington

library(snow)
library(glmnet)

clust1 = makeSOCKcluster(c("node68","node69","node70", "node71", "node72",
                           "node73", "node74", "node75", "node76", "node77",
                           "node78","node79"))

# pw ..

# stopCluster(clust1)

clusterEvalQ(clust1, library(glmnet))
clusterEvalQ(clust1, load("0213sp.datamat.sparse.Rdata"))
load("0213sp.datamat.sparse.Rdata")

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
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 8))

glm.x = function(x) {
  return(try(glmnet(sparse.dm[samp[[x]],-2054], sparse.dm[samp[[x]],2054],
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
  return(try(glmnet(sparse.dm[samp[[x]],-2054], sparse.dm[samp[[x]],2054],
                    family="poisson", lambda = new.lambda, 
                    maxit = maxit2 , thres = 10^(-6))))
}


sample.docs = function() {
  return(sample(1:72574, size = 36287))
}

samp = list()
for(j in 1:100) {
  samp[[j]] = sample.docs()
}

save(lambda.touse, glm.x, find.done, glm.x.re, samp, file = "helperJH.Rdata")
clusterEvalQ(clust1, load("helperJH.Rdata"))
load('helperJH.Rdata')

results = list()
N.trials = 100

for(k in 1:30) {
  j = intersect((length(results) + 1 ): (length(results) + 24), 1:N.trials)
  if (length(j) > 0) {
    print(c(min(j), max(j)))
    print(date())
    print(snow.time(( results[j] = parLapply(clust1, x = j, fun = glm.x) )) )
  }
}
print("Saving....")
save(results, file = "stabselJH.Rdata")

# re-running non-finished trials
nodes=12

for(run in 2:10) {
  is.done = find.done(results)
  to.do = which(is.done == 0)
  prog = list()
  
  if (length(to.do) > 0) {
    
    
    for(j in 1:N.trials) {
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
    if (run %% 3 == 1) {
      save(results, file = "stabselJH.Rdata")
    }
    
  }
}

run = 9
to.do = which(is.done == 0)
prog = list()
for(j in 1:N.trials) {
  if (is.done[j] == 0) {
    prog[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
  }
}
test = glm.x.re(x = 47, prog = prog)

results[[47]] = test
save(results, file = "stabselJH.Rdata")





#######################################3
################ margaret cavendish

library(snow)
library(glmnet)

clust1 = makeSOCKcluster(c("node68","node69","node70", "node71", "node72",
                           "node73", "node74", "node75", "node76", "node77",
                           "node78","node79"))

# pw ..

# stopCluster(clust1)

clusterEvalQ(clust1, library(glmnet))
clusterEvalQ(clust1, load("0213sp.datamat.sparse.Rdata"))
load("0213sp.datamat.sparse.Rdata")

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
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 8))

glm.x = function(x) {
  return(try(glmnet(sparse.dm[samp[[x]],-3500], sparse.dm[samp[[x]],3500],
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
  return(try(glmnet(sparse.dm[samp[[x]],-3500], sparse.dm[samp[[x]],3500],
                    family="poisson", lambda = new.lambda, 
                    maxit = maxit2 , thres = 10^(-6))))
}


sample.docs = function() {
  return(sample(1:72574, size = 36287))
}

samp = list()
for(j in 1:100) {
  samp[[j]] = sample.docs()
}

save(lambda.touse, glm.x, find.done, glm.x.re, samp, file = "helperMC.Rdata")
clusterEvalQ(clust1, load("helperMC.Rdata"))
load('helperMC.Rdata')

results = list()
N.trials = 100

for(k in 1:30) {
  j = intersect((length(results) + 1 ): (length(results) + 24), 1:N.trials)
  if (length(j) > 0) {
    print(c(min(j), max(j)))
    print(date())
    print(snow.time(( results[j] = parLapply(clust1, x = j, fun = glm.x) )) )
  }
}
print("Saving....")
save(results, file = "stabselMC.Rdata")

# re-running non-finished trials
nodes=12

for(run in 2:10) {
  is.done = find.done(results)
  to.do = which(is.done == 0)
  prog = list()
  
  if (length(to.do) > 0) {
    
    
    for(j in 1:N.trials) {
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
    if (run %% 3 == 1) {
      save(results, file = "stabselMC.Rdata")
    }
    
  }
}

run = 9
to.do = which(is.done == 0)
prog = list()
for(j in 1:N.trials) {
  if (is.done[j] == 0) {
    prog[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
  }
}
test = glm.x.re(x = 47, prog = prog)

results[[47]] = test
save(results, file = "stabselMC.Rdata")




###### andrew marvell, pulling links


library(glmnet)
load("stabselAM.Rdata")
load("helperAM.Rdata")
load("0213sp.datamat.sparse.Rdata")

# appearance count matrix. 452 columns (lambdas), 6288 rows (peoples, ignoring JM)

AM.app.ct.mat = matrix(0, nrow = 6288, ncol = 662)

for(j in 1:100) {
  AM.app.ct.mat = AM.app.ct.mat + 
    (results[[j]]$beta[,match(lambda.touse, results[[j]]$lambda)] > 0)
  print(j)
}

smax = apply(AM.app.ct.mat, 1, max)
hist(smax[smax > 0]/100, breaks = 20, xlab = "Fraction of Time Node is Selected",
     main = "Stability Selection Proportions (Milton)",
     sub = "Note: 0% dropped (5801 0%'s)")


dm.names[-169][which(smax > 69)]
AM.res = data.frame(smax, dm.names[-169])[which(smax > 29),]
names(AM.res)[2] = "Name"

AM.res$lamb0.0001 = AM.app.ct.mat[match(AM.res$Name, dm.names[-169]),662]
AM.res$lamb0.0005 = AM.app.ct.mat[match(AM.res$Name, dm.names[-169]),412]

write.csv(AM.res, file = "AM.res.csv")

## GH

library(glmnet)
load("stabselGH.Rdata")
load("helperGH.Rdata")
load("0213sp.datamat.sparse.Rdata")

# appearance count matrix. 452 columns (lambdas), 6288 rows (peoples, ignoring JM)

GH.app.ct.mat = matrix(0, nrow = 6288, ncol = 662)

for(j in 1:100) {
  GH.app.ct.mat = GH.app.ct.mat + 
    (results[[j]]$beta[,match(lambda.touse, results[[j]]$lambda)] > 0)
  print(j)
}

smax = apply(GH.app.ct.mat, 1, max)
hist(smax[smax > 0]/100, breaks = 20, xlab = "Fraction of Time Node is Selected",
     main = "Stability Selection Proportions (Milton)",
     sub = "Note: 0% dropped (5801 0%'s)")


dm.names[-1345][which(smax > 69)]
GH.res = data.frame(smax, dm.names[-1345])[which(smax > 29),]
names(GH.res)[2] = "Name"

GH.res$lamb0.0001 = GH.app.ct.mat[match(GH.res$Name, dm.names[-1345]),662]
GH.res$lamb0.0005 = GH.app.ct.mat[match(GH.res$Name, dm.names[-1345]),412]

write.csv(GH.res, file = "GH.res.csv")

## JH

library(glmnet)
load("stabselJH.Rdata")
load("helperJH.Rdata")
load("0213sp.datamat.sparse.Rdata")

# appearance count matrix. 452 columns (lambdas), 6288 rows (peoples, ignoring JM)

JH.app.ct.mat = matrix(0, nrow = 6288, ncol = 662)

for(j in 1:100) {
  JH.app.ct.mat = JH.app.ct.mat + 
    (results[[j]]$beta[,match(lambda.touse, results[[j]]$lambda)] > 0)
  print(j)
}

smax = apply(JH.app.ct.mat, 1, max)
hist(smax[smax > 0]/100, breaks = 20, xlab = "Fraction of Time Node is Selected",
     main = "Stability Selection Proportions (Milton)",
     sub = "Note: 0% dropped (5801 0%'s)")


dm.names[-2054][which(smax > 69)]
JH.res = data.frame(smax, dm.names[-2054])[which(smax > 29),]
names(JH.res)[2] = "Name"

JH.res$lamb0.0001 = JH.app.ct.mat[match(JH.res$Name, dm.names[-2054]),662]
JH.res$lamb0.0005 = JH.app.ct.mat[match(JH.res$Name, dm.names[-2054]),412]

write.csv(JH.res, file = "JH.res.csv")



## MC

library(glmnet)
load("stabselMC.Rdata")
load("helperMC.Rdata")
load("0213sp.datamat.sparse.Rdata")

# appearance count matrix. 452 columns (lambdas), 6288 rows (peoples, ignoring JM)

MC.app.ct.mat = matrix(0, nrow = 6288, ncol = 662)

for(j in 1:100) {
  MC.app.ct.mat = MC.app.ct.mat + 
    (results[[j]]$beta[,match(lambda.touse, results[[j]]$lambda)] > 0)
  print(j)
}

smax = apply(MC.app.ct.mat, 1, max)
hist(smax[smax > 0]/100, breaks = 20, xlab = "Fraction of Time Node is Selected",
     main = "Stability Selection Proportions (Milton)",
     sub = "Note: 0% dropped (5801 0%'s)")


dm.names[-3500][which(smax > 69)]
MC.res = data.frame(smax, dm.names[-3500])[which(smax > 9),]
names(MC.res)[2] = "Name"

which(MC.app.ct.mat[,662] > 0)

MC.res$lamb0.0001 = MC.app.ct.mat[match(MC.res$Name, dm.names[-3500]),662]
MC.res$lamb0.0005 = MC.app.ct.mat[match(MC.res$Name, dm.names[-3500]),412]
dm.names[-3500]
MC.app.ct.mat[,662]

write.csv(MC.res, file = "MC.res.csv")

sim.lambs[3500,]
which(sim.lambs[,3500] > 0)



