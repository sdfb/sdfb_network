#@S Script for running the the actual model fitting, assuming setup is done
#@L Since this code is parallelized, there are some system-dependent sections of code (that should have been ran already)
START_TRIAL = 1
END_TRIAL = 10

library(glmnet)


LAMBDA_DEPTH= 3
source("code/ODNB/network_estimation/PGL_setup.R") ### Code is temporarily here. need to write this correctly

##### (4) Load data 
load("data/ODNB_final/nodeset_overlap_list.Rdata")
ALLOWED_INDS = date_overlap_list

##### (5) Set up parameters, load functions/data into cluster nodes
##### All of these can be manually changed. 
SAVE_RESULTS = FALSE # This takes up a lot of storage... saves all the regression glm models

#FILENAME_MODEL :: This is prefix for model filename
FILENAME_model = "TEMP/SS_newfit"        # 100 iterations on new data, 4/15/2014
RECORD_TIMING = NULL


# SINGLE_NODE :: TRUE/FALSE: FALSE is default for fitting all nodes; TRUE => only fit glm on one node
## MDOE - "default", "single", or "date-limited". 
# TODO: Document this in more detail. 
MODE = "date-limited"

MAX_REITERS = 100 # When rerunning the glm fits, this it the maximum times to try. 
# It really just needs a sensible finite number; very few nodes will ever need 
# more than 5 reiterations (at least at the current lambda values)

NODES = length(ALLOWED_INDS)



cat("--------------------------------------------------", file = "zGEN_PGL_confest.log", append = TRUE)
cat("--------------------------------------------------", file = "zGEN_PGL_confest.log", append = TRUE)
cat("New run of PGL, starting at: ", date(), file = "zGEN_PGL_confest.log", append = TRUE)
cat("--------------------------------------------------", file = "zGEN_PGL_confest.log", append = TRUE)

# cat(  ,"\n", file = "zGEN_PGL_confest.log", append = TRUE)
for(nn in START_TRIAL:END_TRIAL) {
  TIME_start = format(Sys.time(),"%b:%d;%H:%M:%S")
  
  ##### First pass through the data
  cat("Starting Trial",nn,"of",N_TRIALS,"\n", file = "zGEN_PGL_confest.log", append = TRUE)
  
  cat("-- Creating subset of data matrix","\n", file = "zGEN_PGL_confest.log", append = TRUE)
  load(paste("data/ODNB_newfinal/sampmatrix",nn,".Rdata", sep = ""))
  SUBSET_DM = dcmat
  
  filename_save_lambdas = paste(FILENAME_model,nn,".Rdata", sep = "")
  results = list()
  
  for(run in 1:MAX_REITERS) {
    cat("==========\n", file = "zGEN_PGL_confest.log", append = TRUE)
    
    if (run == 1) {
      progress = list()
      remaining_nodes = 1:NODES
    } else {
      cat("Checking progress and processing nodes for next run... \n", file = "zGEN_PGL_confest.log", append = TRUE)
      
      remaining_nodes = which(sapply(results, function(y) {y$status == "InProg"}))
      
      progress = list()
      if (length(remaining_nodes) > 0) {
        for (j in remaining_nodes) {
          progress[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
        }
      } else {
        break
      }  
    }
    
    cat(length(remaining_nodes),"nodes left. \n", file = "zGEN_PGL_confest.log", append = TRUE)
    cat("==========\n", file = "zGEN_PGL_confest.log", append = TRUE)
    
    # If there are still nodes left, continue. (otherwise, could would have went to 'break')
    current_run = NULL # like before, current_run
    
    for(i in remaining_nodes) {
      cat("Trial: ", nn, "ITER:", run, " ", date(), "    ======    Nodes:",i, "\n", sep = "")
      cat("Trial: ", nn, "ITER:", run, " ", date(), "    ======    Nodes:",i, "\n",sep = "",
          file = "zGEN_PGL_confest.log", append = TRUE)
      results[[i]] = glm_x(x = i, prog = progress, run_no = run, fit_mode = MODE, target_node = TARGET_NODE)
#       if (class(temp) != "try-error" | run == 1) {
#         results[[i]] = temp 
#       }
    }
  }
  
  print("GLM ALL RAN")
  if (SAVE_RESULTS) {
    save(results, file = paste("TEMP/results_run_",nn,".Rdata", sep = ""))
  }
  
  # Process errors: 
  errors = which(sapply(results, function(y) {y$status == "Error"}))
  for (j in errors) {
    results[[j]]$status2 = process_error_results(j)
  }
  
  cat("Extracting Lambda values... \n", file = "zGEN_PGL_confest.log", append = TRUE)
  lambda_matrix = Matrix(0, nrow = NODES, ncol = 13500, sparse = TRUE)
  
  # TODO: should we take first instance of positive coef? what about final positive coef, then find first positive instance? etc.
  
  error_presence = FALSE
  for(j in 1:NODES) {
    if (results[[j]]$status == "Error") {
      if (results[[j]]$status2 == "column_all_zeros") {
        # do nothing (since matrix already has row of zeros)
      } else {
        cat("Trial",nn,": Error = Column",j,"has error message that is not column_all_zeros. \n", file = "zGEN_PGL_confest_error.log", append = TRUE)
        error_presence = TRUE
      }
    } else if (results[[j]]$status == "Done") {
      temp = results[[j]]$results
      lambda_matrix[j,temp$nodeID] = temp$lambda
      if (j %% 1000 == 0) { print(j) }
    } else {
      cat("Trial",nn,": Error = Column",j,"does not have status Done or Error. \n", file = "zGEN_PGL_confest_error.log", append = TRUE)
      error_presence = TRUE
    }
  }
  if (error_presence) {
    cat("***************************************\n", file = "zGEN_PGL_confest.log", append = TRUE)
    cat("***************************************\n", file = "zGEN_PGL_confest.log", append = TRUE)
    cat("ERROR IN TRIAL",nn,"OF",N_TRIALS,"\n", file = "zGEN_PGL_confest.log", append = TRUE)
    cat("***************************************\n", file = "zGEN_PGL_confest.log", append = TRUE)
    cat("***************************************\n", file = "zGEN_PGL_confest.log", append = TRUE)
  }
  
  save(lambda_matrix, file = filename_save_lambdas)
  cat("Finished Trial",nn,"of",N_TRIALS,"\n", file = "zGEN_PGL_confest.log", append = TRUE)
  cat("--------------------------------------------------", file = "zGEN_PGL_confest.log", append = TRUE)
  
  TIME_end = format(Sys.time(),"%b:%d;%H:%M:%S")
  
}

