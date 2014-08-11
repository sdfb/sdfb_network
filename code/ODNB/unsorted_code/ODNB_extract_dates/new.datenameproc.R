# 12/30 3:30 pm

###########################################################
# generic function: grabs all matches in gregexpr
###  Updated 12/30 3:30 pm
get.all.match = function(gregexpr.out, text) {
  res = list()
  for(j in 1:length(gregexpr.out)) {
    how.many = sum(gregexpr.out[[j]] > 0)
    if (how.many > 0) {
      res[[j]] = rep("", times = how.many)
      for(k in 1:how.many) {
        s = gregexpr.out[[j]][k]
        e = s + attr(gregexpr.out[[j]], "match.length")[k] - 1
        res[[j]][k] = substr(text[j], start = s, stop = e)
      }
    } 
  }
  return(res)
}
############################################################


################
## Dates Processing 12/30
##   Takes ODNB.data, tries to extract date information
## -> dates.matrix
################
ZZ.datesproc = function() {
  # Moved to datesproc.R
  
}


################
## Names Processing 12/30
##   Takes ODNB.data raw name data, compare 
## -> newnames.Rdata (contains names that should be kept, names to combine)
################
ZZ.namesproc = function() {
  
  
  load("ODNBdates.Rdata")
  load("names.raw.Rdata")
  
  load("finaldata2.Rdata")
  #already.used = read.csv("words.edit.csv")
  table.names = table(final.data[,1])
  #load("doc.totals.Rdata")
  
  find.match <- function(target) {
    # relies on existence of 'names' from names.raw.Rdata
    split = strsplit(target, split = " ")[[1]]
    res = 1:58265
    for(j in 1:length(split)) {
      res = intersect(res, grep(gsub("[.]", "", split[j]), names))
    }
    return(res)
  }
  count.proper.date <- function(matches) {
    # returns TRUE if entry should be kept
    # keep if any extracted date fits. If NA, ignore for now...
    mat = dates.matrix[matches,1:2,drop = FALSE]
    if (any(is.na(mat))) {
      is.nas = apply(is.na(mat), 1, any)
    } else {
      is.nas = rep(FALSE, times = dim(mat)[1])
    }
    if (any(mat[!is.nas,1] <= 1700 & mat[!is.nas,2] >= 1550)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  which.proper.date <- function(matches) {
    # returns matching documents if entry should be kept
    # keep if any extracted date fits. If NA, ignore for now...
    mat = dates.matrix[matches,1:2,drop = FALSE]
    if (any(is.na(mat))) {
      is.nas = apply(is.na(mat), 1, any)
    } else {
      is.nas = rep(FALSE, times = dim(mat)[1])
    }
    if (any(mat[!is.nas,1] <= 1700 & mat[!is.nas,2] >= 1550)) {
      return(which(mat[!is.nas,1] <= 1700 & mat[!is.nas,2] >= 1550))
    } else {
      return(FALSE)
    }
  }
  
  
  # >= 20 names, not processed
  geq = names(table.names)[table.names > 4]
  length(grep( " ", geq))
  
  # m = find.match(rem[1])
  # dates.matrix[m,]
  # count.proper.date(find.match(rem[6]))
  
  maybe.add = grep(" ", geq)
  keep = rep(FALSE, times = length(maybe.add))
  
  for(j in 1:length(maybe.add)) {
    match = find.match(geq[maybe.add[j]])
    if (length(match) > 0) {
      keep[j] = count.proper.date(match)
    }
    if (j %% 10 == 0) { print(j) }
  }
  m = find.match("Winston Churchill")
  count.proper.date(m)
  dates.matrix[m,]
  
  a = geq[maybe.add][keep]
  a[grep("Primrose", a)]
  
  model.names = geq[maybe.add][keep]
  
  # remove things with no lower case letters
  model.names = model.names[grep("[:lower:]", model.names)]
  
  model.names = model.names[-c(1:3,4847:4891)]
  
  b = grep("Edward", model.names)
  cbind(b, model.names[b])
  
  # names to combine: uses first index as main name
  names.tocombine = list(c(544, 545), #Charles V
                         c(487, 488), #Charles I
                         c(1486, 1487), #George V
                         c(1359, 1360), #George I
                         c(880, 881)) #Edward I
  
  save(model.names, names.tocombine, file = "newnames.Rdata")
}


################
## New data matrix  12/30
##   Creates new upd.datamat
################

ZZ.generatenewdatamat = function () {
  # Making new data matrix (on hydra, vector alloc problems)
  load("end2.Rdata")
  load("finaldata2.Rdata")
  
  load("newnames.Rdata")
  
  
  
  
  # 
  # load("comb500.datamat.Rdata")
  # match(model.names, cbn.names)
  # 
  # 
  # ## construction of new columns from this
  # 
  # 
  # 
  # load("names.testing.Rdata")
  
  for(k in 1:10) {
    print("-------------------------------------------------")  
  }
  
  print(paste("---- THERE ARE A TOTAL OF :",length(model.names), "NAMES TO PROCESS ----"))
  
  
  result.matrix = cbind(DocNum=0, Person=0, Location=0)
  for(j in 1:length(model.names)) {
    row.ind = which(final.data$MatchName == model.names[j])
    for (k in row.ind) {
      doc = final.data$DocNum[k]
      id = final.data$ID[k]
      mat = end.result[[doc]][[2]]
      locs = mat$Position[mat$ID == id]
      temp = cbind(DocNum = doc, Person = j, Location=locs)
      result.matrix = rbind(result.matrix, temp)
    }
    print(j)
  }
  save(model.names, result.matrix, file = "1230new.resmatrix.Rdata")
  
  
  ######## make new datamat file with splits
  
  find.splits = function(x, max=500) {
    num.splits = 1 + floor(x / max)
    return(floor((1:num.splits-1) /num.splits * x + 1))
  }
  
  load("doclengths.Rdata")
  load("1230new.resmatrix.Rdata")
  
  result.matrix = result.matrix[-1,]
  
  head(result.matrix)
  inds.to.proc = sort(unique(result.matrix[,1]))
  new.result.matrix = result.matrix
  
  
  for(j in inds.to.proc) {
    splits = find.splits(doc.total.words[j])
    if(length(splits) > 1) {
      inds = which(result.matrix[,1] == j)
      to.add = sapply(result.matrix[inds,3], function(x){sum(x >= splits)})
      new.docnums = j + to.add * 0.01
      new.result.matrix[inds,1] = new.docnums
    }
    print(j)
  }
  
  dm.names = model.names
  dm.docnums = sort(unique(new.result.matrix[,1]))
  dm = matrix(0, nrow = length(dm.docnums), ncol = length(dm.names))
  
  factor.docs = as.factor(new.result.matrix[,1])
  
  for(p in 1:length(dm.names)) {
    sub = which(new.result.matrix[,2] == p)
    ones = rep(1, times = length(sub))
    
    tapply(ones, INDEX = factor.docs[sub], sum) -> test
    test[is.na(test)] = 0
    dm[,p] = test
    print(p)
  }
  
  
  # combining names
  
  load("newnames.Rdata") 
  rm = NULL
  for(j in 1:length(names.tocombine)) {
    main = names.tocombine[[j]][1]
    others = names.tocombine[[j]][-1]
    rm = c(rm, others)
    for(k in 1:length(others)) {
      dm[,main] = dm[,main] + dm[,others[k]]
      dm[,others[k]] = 0
    }
  }
  
  # Forgot to run. 
  # dm = dm[,-rm]
  # dm.names = dm.names[-rm]
  
  save(new.result.matrix, 
       dm, dm.names, dm.docnums,
       file = "1230sp.datamat.Rdata")
  
  library(Matrix)
  sparse.dm = Matrix(dm, sparse = TRUE)
  save(new.result.matrix, sparse.dm, dm.names, dm.docnums, 
       file = "1230sp.datamat.sparse.Rdata")
  
}




########### Fitting full model for split, full docs, without indicator
## updated 12/31 for running with smaller lambdas
## run on banshee

ZZ.run.poisglasso = function() {
  library(snow)
  library(glmnet)
  
  clust1 = makeSOCKcluster(c("node68","node69","node70", "node71", "node72",
                             "node73", "node74", "node75", "node76", "node77",
                             "node78","node79"))
  
  # pw fOgvo61C6Yw9H6h
  
  # stopCluster(clust1)
  
  clusterEvalQ(clust1, library(glmnet))
  clusterEvalQ(clust1, load("1230sp.datamat.sparse.Rdata"))
  load("1230sp.datamat.sparse.Rdata")
  
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
    return(try(glmnet(sparse.dm[,-x], sparse.dm[,x],
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
    return(try(glmnet(sparse.dm[,-x], sparse.dm[,x],
                      family="poisson", lambda = new.lambda, 
                      maxit = maxit2 , thres = 10^(-6))))
  }
  
  save(lambda.touse, glm.x, find.done, glm.x.re, file = "helper.Rdata")
  clusterEvalQ(clust1, load("helper.Rdata"))
  
  
  snow.time(parLapply(clust1, x = 1:30, fun = glm.x))
  
  
  results = list()
  N.vertex = dim(sparse.dm)[2]
  
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
        save(results, file = "1231cb500.fit.Rdata")
      }
    }
  }
  save(results, file = "1231cb500.fit.Rdata")
  
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
      if (run %% 3 == 1) {
        save(results, file = "1231cb500.test.fit.Rdata")
      }
      
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
  save(results, file = "1231cb500.test.fit.Rdata")
  
}



################## Creating lambda matrix (old file: createnewmat.R on desktop)
load("1231cb500.test.fit.Rdata")
load("1230sp.datamat.sparse.Rdata")
library(Matrix)

# iggy = which(!find.done(results))
# save(iggy, file = "refits.Rdata")

# forgot to remove
iggy = c(488, 545 ,881, 1360, 1487)

#sparse.dm = sparse.dm[,-to.remove]
#dm.names = dm.names[-to.remove]
#results = results[-to.remove]


n = length(dm.names)

res.lambs = matrix(0, nrow = n, ncol = n)




spi <- function(x) {
  return(which(x > 0)[1])
}

# spi(results[[1]]$)
todo = setdiff(1:n, iggy)

for(j in todo) {
  locs = apply(results[[j]]$beta, 1, spi)
  coords = which(!is.na(locs))
  adj.coords = coords
  adj.coords[adj.coords >= j] = adj.coords[adj.coords >= j] + 1
  res.lambs[j,coords] = results[[j]]$lambda[locs[coords]]
  print(j)
}

save(res.lambs, file = "res.lambs.Rdata")

# load("refits.Rdata")
# for(j in iggy) {
#   locs = apply(results[[j]]$beta, 1, spi)
#   coords = which(!is.na(locs))
#   res.lambs[j,coords] = results[[j]]$lambda[locs[coords]]
#   print(j)
#   
# }
# save(res.lambs, file = "res.lambs.Rdata")

sim.lambs = res.lambs

for(j in 1:n) {
  for(k in j:n) {
    if(sim.lambs[j,k] > sim.lambs[k,j]) {
      sim.lambs[k,j] = sim.lambs[j,k]
    } else if (sim.lambs[k,j] > sim.lambs[j,k]) {
      sim.lambs[j,k] = sim.lambs[k,j]
    }
  }
  print(j)
}



dm.names = dm.names[-iggy]
sparse.dm = sparse.dm[,-iggy]

sim.lambs = sim.lambs[-iggy,-iggy]


save(sim.lambs, file = "sim.lambs.Rdata")
# reran on 3/6; bugged :(

write.csv(sim.lambs, file = "lambda.matrix.csv")
nameid.matrix = matrix("", nrow = 6289, ncol = 3)
nameid.matrix[,1] = 1:6289
nameid.matrix[,2] = dm.names
nameid.matrix[,3] = colSums(sparse.dm)
colnames(nameid.matrix) = c("ID", "Entity Name", "Total Appearance Count")
write.csv(nameid.matrix, file = "idnamecount.csv")


topk = function(dat, k) {
  k = min(k, sum(!is.na(dat)))
  return(order(dat,decreasing= TRUE)[1:k])
}
topk(sim.lambs[,2054], k = 30)






























################## 
## This does not belong here
## Update ODNBdatav1.Rdata
##
###################################

dates.text = rep("", times = length(ODNB.names))
for (j in 1:length(ODNB.names)) {
  dates.text[j] = get.dates(ODNB.data[[j]]$text)
  ODNB.data[[j]]$dates = dates.text[j]
  print(j)
}

save(ODNB.cosubject, ODNB.data, ODNB.names, ODNB.nums, ODNB.nums.inv,
     file = "ODNBdatav2.Rdata")
##############################################















## 12/31: perhaps need to adjust/run this?
######## make new datamat file without splits

load("doclengths.Rdata")
load("new.names.resmatrix.Rdata")

result.matrix = result.matrix[-1,]

inds.to.proc = sort(unique(result.matrix[,1]))
new.result.matrix = result.matrix

dmN.names = upd.two.word.names
dmN.docnums = sort(unique(new.result.matrix[,1]))
dmN.datamat = matrix(0, nrow = length(dmN.docnums), ncol = length(dmN.names))

factor.docs = as.factor(new.result.matrix[,1])

for(p in 1:length(dmN.names)) {
  sub = which(new.result.matrix[,2] == p)
  ones = rep(1, times = length(sub))
  
  tapply(ones, INDEX = factor.docs[sub], sum) -> test
  test[is.na(test)] = 0
  dmN.datamat[,p] = test
  print(p)
}

save(new.result.matrix, 
     dmN.datamat, dmN.names, dmN.docnums,
     file = "nosp.add.datamat.Rdata")

load("nosp.add.datamat.Rdata")
load("update3.datamat.Rdata")

cbn.names.nosp = unique(c(dm.names, dmN.names))
first.half = 1:2521
second.half = 2522:6767
cbn.docnums.nosp = sort(unique(c(dm.docnums, dmN.docnums)))

cbn.datamat.nosp = matrix(0, ncol = 6767, nrow = 41722)

first.match = match(dm.docnums, cbn.docnums.nosp)
second.match = match(dmN.docnums, cbn.docnums.nosp)

cbn.datamat.nosp[first.match,first.half] = dm.datamat
cbn.datamat.nosp[second.match,second.half] = dmN.datamat

save(cbn.datamat.nosp, cbn.docnums.nosp, cbn.names.nosp, file = "cbn.nosp.datamat.Rdata")















