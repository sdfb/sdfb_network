library(glmnet)
#library(biglasso)
library(parallel)
library(data.table)
library(sqldf)

load("jstor_docCount.Rdata")

# BigLasso/Memory Matrix
#data.BM <- as.big.matrix(data.mat)
#rm(data.mat)

# Using random samples for psuedo bootstrapping so set seed
set.seed(1)

# Test
#sample_idx = sample(1:nrow(data.mat), size=1000, replace=F)
#data.mat = data.mat[sample_idx, ]

#data.BM = data.BM[sample_idx, ]


data.sparse = Matrix(data.mat, sparse=TRUE)

data = data.sparse

# Clean Up
rm(list=c('data.mat', 'data.BM', 'doc_matrix', 'data.sparse', 'X', 'y'))

# These are reused throughout
persons = colnames(data)
docs = rownames(data)

# Fits a single PGL model for a given person y's counts the the counts X of everyone else
# Length, height of y, X = num docs
# Using a single hardcoded lambda value derived by empirical testing with ODNB.  See Lawrence paper.
fit_pgl_func <- function(y, X) {
  fit = glmnet(X, y,
               family="poisson",
               alpha = 1, # Lasso
               thres = 10^(-6)) #,
  #lambda = c(0.001, 0.0005, 0.0001)) # Makes things SLOW!!!!  See glmnet help
  return(fit)
}

fit_biglasso_func <- function(y, X) {
  fit = biglasso(X, y,
                 family="poisson", # Poisson not supported !!!!
                 alpha = 1) # Lasso
  return(fit)
}

# Runs a PGL fit for each person in the doc matrix (i.e. columns)
person_fits_func <- function(doc_matrix, bootstrap_id=0) {
  results = mclapply(1:length(persons), function(i) {
    #cat("i=", i, fill=T)
    y = doc_matrix[,i]
    X = doc_matrix[,-i]
    fit = fit_pgl_func(y, X)
    
    if(length(fit$lambda) == 1) {
      return(NA) # Bad fit, do we need to do more???
    }
    
    node = persons[i] # Should match in length to persons vector
    
    # We let glmnet compute lambdas for speed, so find the closet to desired value 0.001
    fit_lambda = fit$lambda[which.min(abs(0.001 - fit$lambda))]
    coefs = coef(fit, s=fit_lambda)
    coefs = coefs[-1] # Remove intercept
    relationships_idx = which(coefs > 0) # Only care about positive relationships
    
    # If not positive relationships found, return NA
    if(length(relationships_idx) == 0) {
      return(NA)
    }
    
    relationships = persons[-i][relationships_idx] # Need to first remove the 'target' from the list so that the other idxs line up
    
    # For weights, just count 1 if > 0
    #relationship_weights = coefs[relationships_idx]
    
    # Will combine all data.frames into single summing up the simple_counts
    result = data.table(bootstrap_id=bootstrap_id, a=node, b=relationships, w=1)
    
    rm(list=c('fit', 'node', 'fit_lambda', 'coefs', 'relationships_idx', 'relationships', 'y', 'X'))
    gc()
    
    return(result)
  }, mc.cores=48)
  
  return(results)
}

# Using bootstrap to run our fits B times
# Samples the doc_matrix before running person_fits_fnc
# Returns a list of B lists of p (#num persrons) fits: {B1: {fit_p1, fit_p2, ...}, B2: {fit_p1, fit_p2, ...}}
bootstrap_fits_func <- function(doc_matrix, B=100) {
  
  num_docs = nrow(doc_matrix)
  sample_size = ( num_docs/2 ) %/% 1 # Ensure whole integer
  sample_indexes <- lapply(1:B, function(i) { sample(1:num_docs, sample_size, replace=F) })
  
  B_results = lapply(1:length(sample_indexes), function(idx) {
    cat("Boostrap idx = ", idx, fill=T)
    
    boot_sample_idx = unlist(sample_indexes[[idx]])
    sample_doc_matrix = doc_matrix[boot_sample_idx,]
    sample_results = person_fits_func(sample_doc_matrix, bootstrap_id=idx)
    
    return(sample_results)
  })
  
  return(B_results)
}


extract_links_func <- function(boot_results) {
  # We need to go through each of the B fits and for each possible person<->person
  # count how many times either coefficent > 0
  bootstrap_results = mclapply(1:length(boot_results), function(bootstrap_idx) {
    
      fit_results = boot_results[[bootstrap_idx]]
      
      
      # Combine each persons dataframe into a single dataframe
      fit_links = rbindlist(fit_results[!is.na(fit_results)])
      
      # Now combine relationships so that A->B and B->A is represented as just A<->B
      
      # First order a and b so a always < b, then just aggregate
      
      # data.table doesn't allow multiple := , so split into 2
      fit_ordered = fit_links[, a_temp:=ifelse(a<b, a, b)]
      fit_ordered = fit_ordered[, b_temp:=ifelse(b>a, b, a)]
    
      fit_combined = fit_ordered[, min(w), by=c('bootstrap_id', 'a_temp', 'b_temp')]
      
      return(fit_combined)
    })
    
    # Now we need to combine the counts of all the bootstrap samples
    # The end result should be an data.frame representing an adjancey list
    # where a = personA, b=personB and cnt = # of times the relationship occured in the 100 fits
    bootstrap_links = rbindlist(bootstrap_results[!is.na(bootstrap_results)])
    
    bootstrap_combined = bootstrap_links[, sum(V1), by=c('a_temp', 'b_temp')]
    colnames(bootstrap_combined) = c('a', 'b', 'w')
    
    return(bootstrap_combined)
}

system.time((person_results_pgl = person_fits_func(data)))

#system.time((person_results_biglasso = person_fits_func(data)))

system.time((boot_results = bootstrap_fits_func(data, B=3)))

system.time((extract_results = extract_links_func(boot_results)))

save(list=ls(), file="jstor_PGLFit-15119.Rdata")

load(file="jstor_PGLFit-15119.Rdata")
