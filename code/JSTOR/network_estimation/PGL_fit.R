library(glmnet)
library(parallel)
library(data.table)
library(sqldf)

load("code/JSTOR/text_processing/jstor_docCount.Rdata")

# Using random samples for psuedo bootstrapping so set seed
set.seed(1)


# These are reused throughout
persons = colnames(data.mat)
docs = rownames(data.mat)

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

# Runs a PGL fit for each person in the doc matrix (i.e. columns)
person_fits_func <- function(doc_matrix) {
  results = mclapply(1:length(persons), function(i) {
    y = doc_matrix[,i]
    X = doc_matrix[,-i]
    result = fit_pgl_func(y, X)
    return(result)
  }, mc.cores=4)
  return(results)
}

# Using bootstrap to run our fits B times
# Samples the doc_matrix before running person_fits_fnc
# Returns a list of B lists of p (#num persrons) fits: {B1: {fit_p1, fit_p2, ...}, B2: {fit_p1, fit_p2, ...}}
bootstrap_fits_func <- function(doc_matrix, B=100) {
  
  num_docs = nrow(doc_matrix)
  sample_size = ( num_docs/2 ) %/% 1 # Ensure whole integer
  sample_indexes <- lapply(1:B, function(i) { sample(1:num_docs, sample_size, replace=F) })
  
  B_fits = lapply(sample_indexes, function(idx) {
    sample_doc_matrix = doc_matrix[unlist(idx),]
    sample_fits = person_fits_func(sample_doc_matrix)
    return(sample_fits)
  })
  
  return(B_fits)
}


extract_links_func <- function(B_results) {
# We need to go through each of the B fits and for each possible person<->person
# count how many times either coefficent > 0
    bootstrap_results = mclapply(1:length(B_results), function(bootstrap_idx) {
      
      person_fits = B_results[[bootstrap_idx]]
      
      fit_results = lapply(1:length(person_fits), function(person_fit_idx) {
        # Keep up with the count
        cat(person_fit_idx, fill = T)
        
        # I.e a fit from person i to all others
        fit = person_fits[[person_fit_idx]]
        
        # Ensure we got a good fit
        if(length(fit$lambda) == 1) {
          return(NA) # Bad fit, do we need to do more???
        }
        node = persons[person_fit_idx] # Should match in length to persons vector
        
        # We let glmnet compute lambdas for speed, so find the closet to desired value 0.001
        fit_lambda = fit$lambda[which.min(abs(0.001 - fit$lambda))]
        coefs = coef(fit, s=fit_lambda)
        coefs = coefs[-1] # Remove intercept
        relationships_idx = which(coefs > 0) # Only care about positive relationships
        
        # If not positive relationships found, return NA
        if(length(relationships_idx) == 0) {
          return(NA)
        }
        
        relationships = persons[-person_fit_idx][relationships_idx] # Need to first remove the 'target' from the list so that the other idxs line up
        
        # For weights, just count 1 if > 0
        #relationship_weights = coefs[relationships_idx]
        
        # Will combine all data.frames into single summing up the simple_counts
        result = data.table(bootstrap_id=bootstrap_idx, a=node, b=relationships, w=1)
        
        return(result)
      })
      
      # Combine each persons dataframe into a single dataframe
      fit_links = rbindlist(fit_results[!is.na(fit_results)])
      
      # Now combine relationships so that A->B and B->A is represented as just A<->B
      
      # First order a and b so a always < b, then just aggregate
      fit_ordered = sqldf("select bootstrap_id, case when a < b then a else b end as a,
                          case when b > a then b else a end as b, 1
                          from fit_links")
      fit_combined = sqldf("select bootstrap_id, a, b, min(1) as w 
                             from fit_ordered
                             group by 1, 2, 3
                             ")
      
      return(fit_combined)
    })
    
    # Now we need to combine the counts of all the bootstrap samples
    # The end result should be an data.frame representing an adjancey list
    # where a = personA, b=personB and cnt = # of times the relationship occured in the 100 fits
    bootstrap_links = rbindlist(bootstrap_results[!is.na(bootstrap_results)])
    
    bootstrap_combined = bootstrap_links[, sum(w), by=c('a', 'b')]
    
    return(bootstrap_combined)
}

system.time((person_results = person_fits_func(data.mat)))

system.time((boot_results = bootstrap_fits_func(data.mat)))

system.time((extract_results = extract_links_func(boot_results)))

save(list=c("persons", "docs", "results"), file="jstor_PGLFit.Rdata")

