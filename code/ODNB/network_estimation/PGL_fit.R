## Run fitting for currently specified iteration numbers
## Before running, make sure "trials" exists: 

if("TRIALS" %in% ls() && length(TRIALS) == 2) {
  library(glmnet)
  
  START_TRIAL = TRIALS[1]
  END_TRIAL = TRIALS[2]

  LAMBDA_DEPTH = 3
  source("code/ODNB/network_estimation/PGL_setup.R")
  load("data/ODNB_final/nodeset_overlap_list.Rdata")
  ALLOWED_INDS = date_overlap_list
  
  SAVE_RESULTS = FALSE
  FILENAME_model = "TEMP/SS_newfit"
  RECORD_TIMING = NULL
  MODE = "date-limited"
  MAX_REITERS = 100 
  NODES = length(ALLOWED_INDS)
  
  for(nn in START_TRIAL:END_TRIAL) {
    trial_log = paste("TEMP/trial_",nn,".log", sep = "")
    cat("New run of PGL, starting at: ", date(), file = trial_log, append = TRUE)
    
    
    ##### First pass through the data
    cat("Starting Trial",nn,"of",END_TRIAL,"\n", file = trial_log, append = TRUE)
    
    cat("-- Creating subset of data matrix","\n", file = trial_log, append = TRUE)
    load(paste("data/ODNB_newfinal/sampmatrix",nn,".Rdata", sep = ""))
    SUBSET_DM = dcmat
    
    filename_save_lambdas = paste(FILENAME_model,nn,".Rdata", sep = "")
    results = list()
    
    for(run in 1:MAX_REITERS) {
      cat("==========\n", file = trial_log, append = TRUE)
      
      if (run == 1) {
        progress = list()
        remaining_nodes = 1:NODES
      } else {
        cat("Checking progress and processing nodes for next run... \n", file = trial_log, append = TRUE)
        
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
      
      cat(length(remaining_nodes),"nodes left. \n", file = trial_log, append = TRUE)
      cat("==========\n", file = trial_log, append = TRUE)
      
      # If there are still nodes left, continue. (otherwise, could would have went to 'break')
      current_run = NULL # like before, current_run
      
      for(i in remaining_nodes) {
        cat("Trial: ", nn, "ITER:", run, " ", date(), "    ======    Nodes:",i, "\n", sep = "")
        cat("Trial: ", nn, "ITER:", run, " ", date(), "    ======    Nodes:",i, "\n",sep = "",
            file = trial_log, append = TRUE)
        results[[i]] = glm_x(x = i, prog = progress, run_no = run, fit_mode = MODE, target_node = TARGET_NODE)
      }
    }
    
    print("GLM ALL RAN")

    # Process errors: 
    errors = which(sapply(results, function(y) {y$status == "Error"}))
    for (j in errors) {
      results[[j]]$status2 = process_error_results(j)
    }
    
    cat("Extracting Lambda values... \n", file = trial_log, append = TRUE)
    lambda_matrix = Matrix(0, nrow = NODES, ncol = NODES, sparse = TRUE)
    
    error_presence = FALSE
    for(j in 1:NODES) {
      if (results[[j]]$status == "Error") {
        if (results[[j]]$status2 == "column_all_zeros") {
        } else {
          cat("Trial",nn,": Error = Column",j,"has error message that is not column_all_zeros. \n", file = trial_log, append = TRUE)
          error_presence = TRUE
        }
      } else if (results[[j]]$status == "Done") {
        temp = results[[j]]$results
        lambda_matrix[j,temp$nodeID] = temp$lambda
        if (j %% 1000 == 0) { print(j) }
      } else {
        cat("Trial",nn,": Error = Column",j,"does not have status Done or Error. \n", file = trial_log, append = TRUE)
        error_presence = TRUE
      }
    }
    
    if (error_presence) { cat("ERROR IN TRIAL",nn,"OF",END_TRIAL,"\n", file = trial_log, append = TRUE) }
    
    save(lambda_matrix, file = filename_save_lambdas)
    cat("Finished Trial",nn,"of",END_TRIAL,"\n", file = trial_log, append = TRUE)
    cat("--------------------------------------------------", file = trial_log, append = TRUE)
  }
  
} else {
  print("Must specify variable TRIALS")
}
