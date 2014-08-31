##################################
# Function to set up CV task
#   num.split = how-many-fold-CV
#   data = document data
#   base.filename = what the filename would be, with appending number / .Rdata at end
#   lambda = values of lambda to compute network for
#   use.cur.groups = TRUE if want to load from old data
#   old.filename = OLD filename
# Returns: Time needed for task 

setup.cv.pgl <- function(num.split = 10, data, lambda, base.filename = "TRIAL",
                         use.cur.groups = FALSE, old.filename = "trial") {
  print(paste(date(), "::: Start Time"))
  
  start.time = date()
  
  n.row = dim(data)[1]
  n.col = dim(data)[2]
  
  if (!use.cur.groups) {
    groups = rep(1:num.split, times = n.row/num.split + 2)
    groups = groups[1:n.row]
    groups = groups[sample(x = 1:n.row, size = n.row)]
  }
  
  for(i in 1:num.split) {
    if (use.cur.groups) {
      load(paste(old.filename,i,".Rdata", sep = ""))
    } else {
      training = which(groups != i)
      testing = which(groups == i)
    }
    
    test.pois = my.huge.mb(x = data[training,], 
                           lambda = lambda, 
                           link = "Poisson")
    
    print(paste(date(),"::: Poisson model fit... Trial",i ))
    save(training, testing, test.pois, file = paste(base.filename,i,".Rdata", sep = ""))
    
  }
  
  end.time = date()
  return(c(start.time, end.time))
}

###################
# Function to run k-fold CV
# NN = which splits to run (vector indices)
run.kcv.pgl <- function(k = 10, NN = 1:10, data, base.filename= "TRIAL", 
                        lambda, result.filename) {

  num.datapts = dim(data)[2]
  train.RD = array(0.0, dim = c(num.datapts, length(lambda), k))
  test.RD = array(0.0, dim = c(num.datapts, length(lambda), k))
  options(warn = 2)
  
  for(n in NN) {
    print(date())
    cat("Loading main data: TRIAL =",n, "\n")
    load(paste(base.filename,n,".Rdata", sep = ""))
    
    trainset = data.frame(data[training,])
    testset = data.frame(data[testing,])
    colnames(trainset) = 1:num.datapts
    colnames(testset) = 1:num.datapts
    
    for (i in 1:num.datapts) {
      cat("Trial", i, "--- Time:", date(), "\n")
      
      true.vals = testset[,i]
      wn = which(true.vals > 0)
      for(j in 1:length(lambda)) {
        
        useful.inds = which(test.pois$path[[j]][i,] > 0)
        if(length(useful.inds) == 0) {
          mod.fit = try(glm(trainset[,i] ~ 1, family = "poisson"))
        } else if (length(useful.inds) == 1) {
          mod.fit = try(glm(trainset[,i] ~ trainset[,useful.inds],
                        family = "poisson"))
        } else {
          mod.fit = try(glm(trainset[,i] ~ as.matrix(trainset[,useful.inds, drop = FALSE]),
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
        
        
    } }
    print("computations from refitting glms has been done")
    
    save(train.RD, test.RD, file = result.filename)
  }
}