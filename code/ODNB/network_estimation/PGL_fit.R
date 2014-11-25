## Run fitting for currently specified iteration numbers
## Before running, make sure "TRIALS" exists:
## Also, need "NCORES" exists

## Can use "RESTRICT" to restrict to only a subset of nodes. 

log_result = function(...) {
  cat(...)
  cat(..., file = trial_log, append = TRUE)
}

if ((all(c("NCORES", "TRIALS") %in% ls()) && all(TRIALS > 0))) {
  library(glmnet)
  library(parallel)
  
  if (FALSE) {
    TRIALS = 1; NCORES = 4; RESTRICT = 5500:5599
    TRIALS = 1:3
    NCORES = 1
    RESTRICT = 5500:5505
  }
  
  LAMBDA_DEPTH = 3
  source("code/ODNB/network_estimation/PGL_setup.R")
  load("data/ODNB_final/nodeset_overlap_list.Rdata")
  ALLOWED_INDS = date_overlap_list
  
  SAVE_RESULTS = FALSE
  FILENAME_model = "data_manual/PGL_fit/SS_newfit_pos_"
  MODE = "date-limited"
  MAX_REITERS = 25 
  MAX_BATCHSIZE = 100
  NODES = length(ALLOWED_INDS)
  
  for(nn in TRIALS) {
    trial_log = paste("data_manual/PGL_fit/trial_",nn,".log", sep = "")
    filename_save_lambdas = paste(FILENAME_model,nn,".Rdata", sep = "")
    
    log_result("New run of PGL, starting at: ", date(), "\n")
    
    ##### First pass through the data
    log_result("Starting Trial",nn,"\n")
    log_result("-- Creating subset of data matrix","\n")
    load(paste("data/ODNB_newfinal/sampmatrix",nn,".Rdata", sep = ""))
    SUBSET_DM = dcmat
    
    results = list()
    for(run in 1:MAX_REITERS) {
      log_result("==========\n")
      
      ## Figure out progress on all nodes
      if (run == 1) {
        progress = list()
        remaining_nodes = 1:NODES
        if ("RESTRICT" %in% ls()) {remaining_nodes = intersect(remaining_nodes, RESTRICT)}
      } else {
        log_result("Checking progress and processing nodes for next run... \n")
        remaining_nodes = which(sapply(results, function(y) {!is.null(y) && (y$status == "InProg")}))
        progress = list()
        if (length(remaining_nodes) > 0) {
          progress[remaining_nodes] = lapply(remaining_nodes, function(j) { list(lambda = results[[j]]$lambda, iters = (run - 1)) })
        } else { break }
      }
      
      log_result(length(remaining_nodes),"nodes left. \n","==========\n")
      # If there are still nodes left, continue. (otherwise, could would have went to 'break')
      
      #       ## Single iteration version. 
      #       for(i in remaining_nodes) {
      #         log_result("Trial: ", nn, "ITER:", run, " ", date(), "    ======    Nodes:",i, "\n", sep = ""
      #         results[[i]] = glm_x(x = i, prog = progress, run_no = run, fit_mode = MODE, target_node = TARGET_NODE)
      #       }
      ## Using parallel version
      batches = split(x = remaining_nodes, f = rep(1:ceiling(NODES / MAX_BATCHSIZE), each = MAX_BATCHSIZE)[seq_along(remaining_nodes)])
      for(bat in seq_along(batches)) {
        log_result("Trial:",nn, "-ITER:",run,"-----BATCH:", bat, "/", length(batches),"-----     -----", date(), "\n", sep = "")
        results[batches[[bat]]] = mclapply(X = batches[[bat]], FUN = function(y) { glm_x(x = y, prog = progress, run_no = run, fit_mode = MODE, target_node = TARGET_NODE) }, mc.cores = NCORES)
      }
    }
    log_result("------------GLM Fitting all done\n")
    
    # Process errors: 
    errors = which(sapply(results, function(y) {!is.null(y) && y$status == "Error"}))
    for (j in errors) { results[[j]]$status2 = process_error_results(j) }
    
    log_result("Extracting Lambda values... \n")
    lambda_matrix = Matrix(0, nrow = NODES, ncol = NODES, sparse = TRUE)
    
    error_presence = FALSE
    for(j in which(sapply(results, function(x) {!is.null(x)}))) {
      if (results[[j]]$status == "Error") {
        if (results[[j]]$status2 == "column_all_zeros") {
          ## No need; no fitting done here. 
        } else {
          log_result("Trial",nn,": Error = Column",j,"has error message that is not column_all_zeros. \n")
          error_presence = TRUE
        }
      } else if (results[[j]]$status == "Done") {
        temp = results[[j]]$results
        ## Keep only positive coefficients > 0.001
        poscoefs = which(temp$finalBeta > 0.001)
        temp = temp[poscoefs,]
        if (length(poscoefs) > 0) { lambda_matrix[j,as.numeric(temp$name)] = temp$lambda}
      } else {
        log_result("Trial",nn,": Error = Column",j,"does not have status Done or Error. \n")
        error_presence = TRUE
      }
    }
    
    if (error_presence) { log_result("ERROR IN TRIAL",nn,"\n") }
    
    save(lambda_matrix, file = filename_save_lambdas)
    log_result("Finished Trial",nn,"-----", date(), "\n", "----------------------\n")
  }
} else {
  print("Must specify variable TRIALS and NCORES")
}
