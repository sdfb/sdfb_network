#@S Script that is run for initialization of PGL fitting process
#@L See function documentation for specific functions

# Notes: Global variables needed: DM (well, at least within each cluster)
#         - function calls expect DM to exist; this is to improve runtime. 

# DM = NULL # document-appearance_count matrix
# this should have column names as node names
# row names are optional... could be document identifiers
## TODO: In later iterations, this is no longer needed. 

SUBSET_DM = NULL # subset of document-appearance_count matrix for current iteration. 
# can always use this as full matrix if fit on all data is desired
ALLOWED_INDS = NULL # This is a list of allowed possible links (based on date ranges). 

# TODO: Figure out to do analysis of runtime based on lambda sequence : time AND npasses?
# TODO: Improve/comment code
LAMBDA_DEPTH = 3
lambda_list = list()
lambda_list[[1]] = unique(c(seq(from=100, to=10, by = -5),
                            seq(from=10, to=6, by = -.5),
                            seq(from=6, to=4, by = -.25),
                            seq(from=4, to=2, by = -.1),
                            seq(from=2, to=.9, by = -.05),
                            seq(from=.9, to=.3, by = -.04)))
lambda_list[[2]] = unique(c(seq(from=.3, to=.15, by = -.005),
                            seq(from=.15, to=.1, by = -.0025),
                            seq(from=.1, to=.05, by = -.002)))
lambda_list[[3]] = unique(c(seq(from=.05, to = .02, by = -.001),
                            seq(from=.02, to = .01, by = -.0002),
                            seq(from=.01, to = .005, by = -.0002),
                            seq(from=.005, to = .003, by = -.0001),
                            seq(from=.003, to = .002, by = -.00005),
                            seq(from=.002, to = .001, by = -.00002)))
lambda_list[[4]] = unique(c(seq(from=.001, to = .0005, by = -.00001),
                            seq(from=.0005, to = .0002, by = -.000002),
                            seq(from=.0002, to = .0001, by = -.000001), 
                            seq(from=.0001, to = .00005, by = -.0000002)))
lambdas = unique(round(sort(c(lambda_list[1:LAMBDA_DEPTH], recursive = TRUE), decreasing = TRUE), 8))
MIN_LAMBDAS = min(lambdas)

## Not needed when sampling is done in advance. 
# sample_docs = function() {
#   # samples half of the documents 
#   # TODO: [Document] this function
#   return(sample(1:nrow(DM), size = floor(nrow(DM)/2)))
# }

# New functions -----------------------------------------------------------
load("data_manual/ODNB_dataset.Rdata")

glm_x <- function(x, prog, lamb.adj = 11, run_no = 2,
                  store_model = FALSE, target_node = NULL,
                  mode = "simple", fit_mode = "default") {
  #@F ----------------------------------------
  #@F Function ''
  #@F Args (Input):
  #@F Purpose:
  #@F Example:
  #@F Output:
  #@F Notes: 
  #@F Global Variables Needed:
  #@F ----------------------------------------
  
  # TODO: [Document] this function
  # NOTE THIS FUNCTION USES A FAIR NUMBER OF GLOBAL VARIABLES.
  ## SETUP PARAMETERS
  if (run_no == 1) {
    new_lambda = lambdas
    new_maxit = 2000
    
  } else {
    # Later iterations
    start = prog[[x]]$lambda[length(prog[[x]]$lambda) - 1]
    matches = match(prog[[x]]$lambda,lambdas)
    end = lambdas[max(matches, na.rm = TRUE) + 1]
    
    if (run_no > 5) {
      to.add = seq(from=start, to=end, length.out = (lamb.adj + 10 * floor(run_no / 5)))
    } else {
      to.add = seq(from=start, to=end, length.out = lamb.adj)
    }
    
    new_lambda = unique(sort(c(to.add, prog[[x]]$lambda, lambdas), decreasing = TRUE))
    new_maxit = 2000 * (prog[[x]]$iters+1)
  }
  ##########
  
  ## Either fit single node or multiple nodes
  if (fit_mode == "single") {
    output = try(glmnet(SUBSET_DM[SAMPLES[[r]],-target_node], 
                        SUBSET_DM[SAMPLES[[r]],target_node],
                        family="poisson", lambda = new_lambda, 
                        maxit = new_maxit , thres = 10^(-6)))
  } else if (fit_mode == "default") {
    output = try(glmnet(SUBSET_DM[,-x], SUBSET_DM[,x],
                        family="poisson", lambda = new_lambda, 
                        maxit = new_maxit , thres = 10^(-6)))
  } else if (fit_mode == "date-limited") {
    ## TODO: write this. 
    if (sum(SUBSET_DM[,x]) == 0) { return(list(status = "Done", 
                                               results = data.frame(nodeID = numeric(), name = character(), 
                                                                    lambda = numeric(), finalBeta = numeric(), stringsAsFactors = FALSE))) } 
    output = try(glmnet(x = SUBSET_DM[,ALLOWED_INDS[[x]]], y = SUBSET_DM[,x],
                        family = "poisson", lambda = new_lambda, 
                        maxit = new_maxit, thres = 10^(-6)))
  }
  if (mode == "simple") {
    return(process_glm(output, cur_x = x, store_model = store_model, fit_mode = fit_mode))
  } else if (mode == "full") { 
    return(output)
  } else if (mode == "get_iters") {
    return(process_glm(output, cur_x = x, store_model = store_model, fit_mode = fit_mode, get_iters = TRUE))
  }
}


# TODO: Remove this code?
# TODO: [Document] this function
# TODO: Since this only differs from glm_x in the glmnet calls, perhaps can combine two functions
# glm_x_single <- function(r, prog, lamb.adj = 11, run_no = 2, simple = TRUE, store_model = FALSE) {
#   #@F ----------------------------------------
#   #@F Function ''
#   #@F Args (Input):
#   #@F Purpose:
#   #@F Example:
#   #@F Output:
#   #@F Notes: 
#   #@F Global Variables Needed:
#   #@F ----------------------------------------
#   
#   if (run_no == 1) {
#     # Old glm_x
#     output = try(glmnet(SUBSET_DM[SAMPLES[[r]],-TARGET_NODE], 
#                         SUBSET_DM[SAMPLES[[r]],TARGET_NODE],
#                         family="poisson", lambda = lambdas, 
#                         maxit = 2000, thres = 10^(-6)))
#   } else {
#     # Later iterations; old glm_x_refit
#     start = prog[[x]]$lambda[length(prog[[x]]$lambda) - 1]
#     matches = match(prog[[x]]$lambda,lambdas)
#     end = lambdas[max(matches, na.rm = TRUE) + 1]
#     
#     if (run_no > 5) {
#       to.add = seq(from=start, to=end, length.out = (lamb.adj + 10 * floor(run_no / 5)))
#     } else {
#       to.add = seq(from=start, to=end, length.out = lamb.adj)
#     }
#     
#     new_lambda = unique(sort(c(to.add, prog[[x]]$lambda, lambdas), decreasing = TRUE))
#     new_maxit = 2000 * (prog[[x]]$iters+1)
#     #return(list(new.lambda, maxit2))
#     
#   }
#   if (simple) {
#     return(process_glm(output, cur_x = x, store_model = store_model))
#   } else {
#     return(output)
#   }
# }

process_glm = function(y, cur_x, fit_mode, store_model = FALSE, get_iters = FALSE) {
  #@F ----------------------------------------
  #@F Function ''
  #@F Args (Input):
  #@F Purpose:
  #@F Example:
  #@F Output:
  #@F Notes: 
  #@F Global Variables Needed:
  #@F ----------------------------------------
  
  # TODO: [Document] this function
  # Figures out what the appropriate return value is... 
  #  input is output from glmnet
  if (length(y) == 12) {
    if (y$lambda[length(y$df)] == MIN_LAMBDAS) { #done fitting: 
      locs = apply(y$beta, 1, function(x) { return(which(x != 0)[1]) })
      coords = which(!is.na(locs))
      adj_coords = coords
      if (fit_mode != "date-limited") {
        adj_coords[adj_coords >= cur_x] = adj_coords[adj_coords >= cur_x] + 1
      }
      results = data.frame(
        nodeID = adj_coords, 
        name = rownames(y$beta)[coords], 
        lambda = y$lambda[locs[coords]], 
        finalBeta = y$beta[coords,dim(y$beta)[2]], 
        stringsAsFactors = FALSE)
      if (!store_model) {
        return(list(status="Done",results=results))
      } else {
        return(list(status="Done",results=results,glmnet_model=y))
      }
    } else { # need to continue fitting, save lambda sequence
      return(list(status="InProg", lambda=y$lambda))
    }
  } else { #ERROR
    return(list(status="Error", status2="FAILED"))
  }
}


process_error_results = function(j) {
  #@F ----------------------------------------
  #@F Function 'process_error_resuts'
  #@F Args (Input): j (column number of SUBSET_DM)
  #@F Purpose: checks if column j is all zeros (leads to glmnet fail) or if it is an 
  #@F   "actual" glmnet error. 
  #@F Output: character singleton: "column_all_zeros" or "FAILED"
  #@F Global Variables Needed: SUBSET_DM (training dataset)
  #@F Notes: This is not run on the clusters
  #@F ----------------------------------------
  
  if (sum(SUBSET_DM[,j]) == 0) {
    return("column_all_zeros")
  } else {
    return("FAILED")
  }
}
