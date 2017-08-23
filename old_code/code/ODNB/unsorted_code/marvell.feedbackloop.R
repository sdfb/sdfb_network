read.csv("AMarvell.valid.csv", stringsAsFactors = FALSE) -> amValid
amValid = amValid[,1:3]
amValid[145,3] = ""

amValid[,3] = as.numeric(amValid[,3])

find.error = function(valid, conf.est) {
  #validation = original scores, so need to be inverted
  valid[valid <= 20] = 0
  inv.valid = 100 - valid
  return(abs(conf.est - inv.valid))
}
amValid$Diff = find.error(valid = amValid[,3], conf.est = amValid[,2])

amValid[order(amValid$Diff),]
amValid[order(amValid$Confidence.Estimate),]
################################################
load("0213sp.datamat.sparse.Rdata")
colnames(sparse.dm) = dm.names
rownames(sparse.dm) = dm.docnums
save(sparse.dm, file = "0325sp.datamat.sparse.Rdata")

########################3
# andrew marvell

library(snow)
library(glmnet)

clust1 = makeSOCKcluster(c("node68","node69","node70", "node71", "node72",
                           "node73", "node74", "node75", "node76", "node77",
                           "node78","node79"))

# pw fOgvo61C6Yw9H6h

# stopCluster(clust1)

clusterEvalQ(clust1, library(glmnet))
clusterEvalQ(clust1, load("0325sp.datamat.sparse.Rdata"))
load("0325sp.datamat.sparse.Rdata")

################################################
# Refit original
SSfile = "stabRE0.AM.Rdata"
Finfile = "AM.conf0.Rdata"
lambda.scale = rep(1, times = dim(sparse.dm)[2])

# MODIFICATIONS
# -- First Step
#Merge Charles Stuart, Charles I, King Charles
# First, drop William Mason / Edward Coke
SSfile = "stabRE1.AM.Rdata"
Finfile = "AM.conf1.Rdata"

load("0325sp.datamat.sparse.Rdata")
temp = match(c("Charles I", "Charles Stuart","King Charles"), colnames(sparse.dm))
sparse.dm[,temp[1]] = apply(sparse.dm[,temp], 1, sum)
sparse.dm = sparse.dm[,-temp[2:3]]

temp = match(c("William Mason", "Edward Coke"), colnames(sparse.dm))
sparse.dm = sparse.dm[,-temp]
lambda.scale = rep(1, times = dim(sparse.dm)[2])

save(sparse.dm, file = "datatemp.Rdata")
load("datatemp.Rdata")
clusterEvalQ(clust1, load("datatemp.Rdata"))

# -- Second Step
# Drop Anthony Wood, Robert Holmes
SSfile = "stabRE2.AM.Rdata"
Finfile = "AM.conf2.Rdata"

load("datatemp.Rdata")

temp = match(c("Robert Holmes", "Anthony Wood"), colnames(sparse.dm))
sparse.dm = sparse.dm[,-temp]
lambda.scale = rep(1, times = dim(sparse.dm)[2])

save(sparse.dm, file = "datatemp.Rdata")
load("datatemp.Rdata")
clusterEvalQ(clust1, load("datatemp.Rdata"))

# -- Third Step
# Enforce John Thurloe, Thomas Blood
SSfile = "stabRE3.AM.Rdata"
Finfile = "AM.conf3.Rdata"

load("datatemp.Rdata")

no.penalty = match(c("John Thurloe", "Thomas Blood"), colnames(sparse.dm))
no.penalty[no.penalty > which(colnames(sparse.dm) == "Andrew Marvell")] = no.penalty[no.penalty > which(colnames(sparse.dm) == "Andrew Marvell")] - 1
lambda.scale = rep(1, times = (dim(sparse.dm)[2] - 1))
lambda.scale[no.penalty] = 0.001


clusterEvalQ(clust1, load("datatemp.Rdata"))

# -- Fourth Step
# Enforce Thomas Fairfax, Bulstrode Whitelocke
SSfile = "stabRE4.AM.Rdata"
Finfile = "AM.conf4.Rdata"

load("datatemp.Rdata")

no.penalty = match(c("Bulstrode Whitelocke","Thomas Fairfax",  "John Thurloe", "Thomas Blood"), colnames(sparse.dm))
no.penalty[no.penalty > which(colnames(sparse.dm) == "Andrew Marvell")] = no.penalty[no.penalty > which(colnames(sparse.dm) == "Andrew Marvell")] - 1
lambda.scale = rep(1, times = (dim(sparse.dm)[2] - 1))
lambda.scale[no.penalty] = 0.001



clusterEvalQ(clust1, load("datatemp.Rdata"))

# -- Step 5
# Drop c("Algernon Sidney", "John Donne", "Martin Marprelate", "John Toland")

SSfile = "stabRE5.AM.Rdata"
Finfile = "AM.conf5.Rdata"

load("datatemp.Rdata")

temp = match(c("Algernon Sidney", "John Donne", "Martin Marprelate", "John Toland"),  
             colnames(sparse.dm))
sparse.dm = sparse.dm[,-temp]
lambda.scale = rep(1, times = dim(sparse.dm)[2])

save(sparse.dm, file = "datatemp.Rdata")
load("datatemp.Rdata")
clusterEvalQ(clust1, load("datatemp.Rdata"))

no.penalty = match(c("Bulstrode Whitelocke","Thomas Fairfax",  "John Thurloe", "Thomas Blood"), colnames(sparse.dm))
no.penalty[no.penalty > which(colnames(sparse.dm) == "Andrew Marvell")] = no.penalty[no.penalty > which(colnames(sparse.dm) == "Andrew Marvell")] - 1
lambda.scale = rep(1, times = (dim(sparse.dm)[2] - 1))
lambda.scale[no.penalty] = 0.0001

               
               
               
##########################


node.ID.for.fits = which(colnames(sparse.dm) == "Andrew Marvell")
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
min.lamb = min(lambda.touse)

glm.x = function(x) {
  test = try(glmnet(sparse.dm[samp[[x]],-node.ID.for.fits], sparse.dm[samp[[x]],node.ID.for.fits],
                    family="poisson", lambda = lambda.touse, penalty.factor = lambda.scale,
                    maxit = 2000 , thres = 10^(-6)))
  return(process.glm(test))
}

process.glm = function(x) {
  # Figures out what the appropriate return value is... 
  #  input is output from glmnet
  if (length(x) == 12) {
    if (x$lambda[length(x$df)] == min.lamb) { #done fitting: 
      locs = apply(x$beta, 1, function(x) { return(which(x != 0)[1]) })
      coords = which(!is.na(locs))
      results = cbind(rownames(x$beta)[coords], x$lambda[locs[coords]], x$beta[coords,dim(x$beta)[2]])
      return(list("Done",results,x))
    } else { # need to continue fitting, save lambda sequence
      return(list("InProg", x$lambda))
    }
  } else { #ERROR
    return(list("Error", "FAILED"))
  }
}

glm.x.re <- function(x, prog, lamb.adj = 11) {
  start = prog[[x]]$lambda[length(prog[[x]]$lambda) - 1]
  matches = match(prog[[x]]$lambda,lambda.touse)
  end = lambda.touse[max(matches, na.rm = TRUE) + 1]
  
  to.add = seq(from=start, to=end, length.out = lamb.adj)
  new.lambda = unique(sort(c(to.add, prog[[x]]$lambda, lambda.touse), decreasing = TRUE))
  maxit2 = 2000 * round((prog[[x]]$iters+1)^1.2)
  #return(list(new.lambda, maxit2))
  test = try(glmnet(sparse.dm[samp[[x]],-node.ID.for.fits], sparse.dm[samp[[x]],node.ID.for.fits],
                    family="poisson", lambda = new.lambda, penalty.factor = lambda.scale,
                    maxit = maxit2 , thres = 10^(-6)))
  return(process.glm(test))
}


sample.docs = function() {
  return(sample(1:72574, size = 36287))
}

samp = list()
N.trials = 100
for(j in 1:N.trials) {
  samp[[j]] = sample.docs()
}

save(node.ID.for.fits, lambda.touse, glm.x, glm.x.re, lambda.scale,
     samp, min.lamb, process.glm, file = "helptemp.Rdata")
clusterEvalQ(clust1, load("helptemp.Rdata"))
load('helptemp.Rdata')



results = list()
for(k in 1:300) {
  j = intersect((length(results) + 1 ): (length(results) + 36), 1:N.trials)
  if (length(j) > 0) {
    print(c(min(j), max(j)))
    print(date())
    print(snow.time(( results[j] = parLapply(clust1, x = j, fun = glm.x) )) )
  }
}
print("Saving....")
save(results, file = SSfile)

# re-running non-finished trials
nodes=12

for(run in 2:10) {
  prog = list()
  not.done = rep(TRUE, times = N.trials)
  
  for(j in 1:N.trials) {
    if(results[[j]][[1]] == "InProg") {
      prog[[j]] = list(lambda = results[[j]][[2]], iters = (run - 1))  
    } else {
      not.done[j] = FALSE
    }
  }
  to.do = which(not.done)

  if (length(to.do) > 0) {
    to.run = NULL
    for(i in 1:500) {
      prog[to.run] = paste(run ,"<- iteration done")
      to.run = to.do[intersect((nodes*(i-1)+1):(nodes*(i)), 1:length(to.do))]
      to.run = to.run[!is.na(to.run)]
      if (length(to.run) > 1) {
        print(paste("Re-iter:", run))
        print(to.run)
        print(date())
        print(snow.time((results[to.run] = parLapply(clust1, x = to.run, 
                                                     fun = glm.x.re, 
                                                     prog = prog,
                                                     lamb.adj = max(11,(run*2) + 1)) )) )
      } else if (length(to.run) == 1) {
        print(paste("Re-iter:", run))
        print(to.run)
        print(date())
        results[[to.run]] = glm.x.re(x = to.run, prog = prog)
      }
    }
  }
  if (run %% 2 == 1) {
    save(results, file = SSfile)
  }
}
save(results, file = SSfile)



load(SSfile)
accum = NULL
for (j in 1:N.trials) {
  accum = c(accum, results[[j]][[2]][as.numeric(results[[j]][[2]][,3])>0,1])
}
conf = table(accum)
rm(accum)
save(conf, file = Finfile)


#################################
# cout amount > 0
all.names = NULL
for(j in 1:N.trials) {
  all.names = c(all.names, results[[j]][[2]][,1])
}
all.names = unique(all.names)


result.matrix = matrix(nrow = length(all.names), ncol = 10)
# 1. number times lambda > 0
# 2. number times lambda < 0
# 3. 1+2
# 4. mean when lambda != 0
# 5. sd when lambda != 0

lambda.matrix = matrix(NA, nrow = length(all.names), ncol = 100)
coef.matrix = matrix(NA, nrow = length(all.names), ncol = 100)
for(j in 1:N.trials) {
  temp = results[[j]][[2]]
  temp.ID = match(temp[,1],all.names)
  lambda.matrix[temp.ID,j] = as.numeric(temp[,2])
  coef.matrix[temp.ID,j] = as.numeric(temp[,3])
}

result.matrix[,1] = apply(coef.matrix, 1, function(x) {sum(x > 0, na.rm = TRUE)})
result.matrix[,2] = apply(coef.matrix, 1, function(x) {sum(x < 0, na.rm = TRUE)})
result.matrix[,3] = result.matrix[,1] + result.matrix[,2]
result.matrix[,4] = apply(coef.matrix, 1, mean, na.rm = TRUE)
result.matrix[,5] = apply(coef.matrix, 1, sd, na.rm = TRUE)
rownames(result.matrix) = all.names

save(lambda.matrix, coef.matrix, result.matrix, file = "analysis.SS.AM0.Rdata")

##################################
## Run on this system

manual.changes = data.frame(Names = c("Charles Stuart", "King Charles",
                                      "William Mason", "Edward Coke"),
                            Step = rep(1, times = 4),
                            stringsAsFactors = FALSE)
manual.changes = rbind(manual.changes, data.frame(Names = c("Robert Holmes", "Anthony Wood",
                                                            "John Thurloe", "Thomas Blood",
                                                            "Bulstrode Whitelocke", "Thomas Fairfax",
                                                            "Algernon Sidney", "John Donne",
                                                            "Martin Marprelate", "John Toland"),
                                                  Step = c(2,2,3,3,4,4,5,5,5,5),
                                                  stringsAsFactors = FALSE))


load("analysis.SS.AM0.Rdata")
load("analysis.SS.AM5.Rdata")



a = combine.tables(amValid, result.matrix[which(result.matrix[,1] > 0),1], new.colname = "RC5")
a[a[,2] < 0,2] = 0
a[a[,5] < 0,5] = 0
ig = match(manual.changes[,1],a$Name)
ig = ig[!is.na(ig)]
t1 = rep(0, times = dim(a)[1])
t1[ig] = 1
a = cbind(a,  t1)
write.csv(a, file = "temp.csv")

b = combine.tables(a, result.matrix[which(result.matrix[,2] > 50),2], new.colname = "negEdges")
write.csv(b, file = "marvell2.csv")

res.colors = rep("gray", times = dim(result.matrix)[1])
res.colors[match(a$Name[a$Valid < 21 & a$Valid > 0 & !is.na(a$Valid)], rownames(result.matrix))] = "green"
res.colors[match(a$Name[a$Valid == 50 & !is.na(a$Valid)], rownames(result.matrix))] = "darkgreen"
res.colors[match(a$Name[a$Valid > 50 & !is.na(a$Valid)], rownames(result.matrix))] = "red"
res.pch = rep(19, times = dim(result.matrix)[1])
res.pch[match(a$Name[ig], rownames(result.matrix))] = 22
plot(result.matrix[,1], result.matrix[,2], col = res.colors, pch = res.pch, cex = 0.7,
     xlab = "Number of tries with positive coefficient (Confidence)",
     ylab = "Number of negative coef", 
     main = "After Iteration")
plot(result.matrix[,4], result.matrix[,5], col = res.colors, 
     pch = res.pch, cex = result.matrix[,3]/100,
     xlim = c(-10, 10), ylim = c(0, 10),
     xlab = "Mean of coefficients over all trials with nonzero coefficient",
     ylab = "SD of coefficients",
     main = "After Iteration")

a = a[-ig,]
hist(a[,5] - a[,2], xlab = "Change in Confidence Estimate", main = "Histogram of [newConf - oldConf]")

d = which(abs(a[,5] - a[,2]) > 10)
a[d,]


# k = 1
# load(paste("AM.conf",k,".Rdata", sep = ""))

combine.tables = function(old.table, conf, new.colname) {
  # old.table = amValid, need 'Name' column
  new.names = setdiff(names(conf), old.table$Name)
  if (length(new.names) > 0) {
    to.append = data.frame(Name = new.names, 
                         matrix(-1, nrow = length(new.names), 
                                ncol =(dim(old.table)[2] - 1)),
                         stringsAsFactors = FALSE)
    colnames(to.append) = colnames(old.table)
    new.tab = rbind(old.table, to.append)
  } else {
    new.tab = old.table
  }
  newcol = rep(-1, times = dim(new.tab)[1])
  newcol[match(names(conf), new.tab$Name)] = conf
  new.tab = cbind(new.tab, newcol)
  colnames(new.tab)[length(colnames(new.tab))] = new.colname
  return(new.tab)
}

NT = amValid
for(k in 0:4) {
  load(paste("AM.conf",k,".Rdata", sep = ""))  
  NT = combine.tables(NT, conf/500, paste("NC",k, sep = ""))
}

plot.changes = function(NT, K, lower.lim, upper.lim) {
  toplot = which(NT$Validation <= upper.lim & NT$Validation >= lower.lim)
  plot(-1,-1, xlim = c(0,K), ylim = c(-0.1, 1.1))
  for(j in toplot) {
    for(k in 1:K) {
      diff = NT[j,5+k] - NT[j,4+k]
      col.touse = "gray18"
      lwd.touse = 1
      if (diff > 0.062) { # 2SE = 0.062
        col.touse = "lightblue"
        lwd.touse = 3
      } else if (diff < -0.062) {
        col.touse = "red"
        lwd.touse = 3
      }
      lines(c(k-1, k), NT[j,k+c(4,5)], col = col.touse, lwd = lwd.touse)
    }
  }
}

plot.changes(NT, K= 4, lower.lim = 0, upper.lim = 10)
plot.changes(NT, K= 4, lower.lim = 15, upper.lim = 25)

plot.changes(NT, K= 4, lower.lim = 50, upper.lim = 50)
plot.changes(NT, K= 4, lower.lim = 80, upper.lim = 80)
plot.changes(NT, K= 4, lower.lim = 100, upper.lim = 100)

colnames(NT)[2] = "Conf"
a = NT$NC4 - NT$NC0
b = which(NT$NC4 > 0 & NT$NC0 > 0)
d = data.frame(NT$Name[b], round(a[b],3))
d[order(d[,2]),]



