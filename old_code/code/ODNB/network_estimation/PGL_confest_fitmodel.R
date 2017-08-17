#@S Script for running the the actual model fitting, assuming setup is done
#@L Since this code is parallelized, there are some system-dependent sections of code (that should have been ran already)

cat("--------------------------------------------------", file = "zGEN_PGL_confest.log", append = TRUE)
cat("--------------------------------------------------", file = "zGEN_PGL_confest.log", append = TRUE)
cat("New run of PGL, starting at: ", date(), file = "zGEN_PGL_confest.log", append = TRUE)
cat("--------------------------------------------------", file = "zGEN_PGL_confest.log", append = TRUE)

# cat(  ,"\n", file = "zGEN_PGL_confest.log", append = TRUE)
for(nn in START_TRIAL:N_TRIALS) {
  
  # TODO: [TEST] Need to test MAX_REITERS parameter, and check 'cat' working in lambda extraction
  # TODO: Document/comment this code more. 
  
  TIME_start = format(Sys.time(),"%b:%d;%H:%M:%S")
  
  ##### First pass through the data
  cat("Starting Trial",nn,"of",N_TRIALS,"\n", file = "zGEN_PGL_confest.log", append = TRUE)
  
  cat("-- Creating subset of data matrix","\n", file = "zGEN_PGL_confest.log", append = TRUE)
  ## SUBSET_DM = sample_matrix(frac = 0.5)
  ## save(SUBSET_DM, file = paste("USED_subsetdm", nn,".Rdata", sep = ""))

  load(paste("data/ODNB_newfinal/sampmatrix",nn,".Rdata", sep = ""))
  SUBSET_DM = dcmat
  save(SUBSET_DM, file = "TEMP_SS_SUBSETDM.Rdata")
  clusterEvalQ(clust1, load("TEMP_SS_SUBSETDM.Rdata"))
  
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
    num_splits_needed = num_splits_needed = round(NODES/ITER_BATCH) + 10
    for(i in 1:num_splits_needed) {
      # progress[current_run] = paste(run ,"<- iteration done")
      current_run = remaining_nodes[intersect((ITER_BATCH*(i-1)+1):(ITER_BATCH*(i)), 1:length(remaining_nodes))]
      current_run = current_run[!is.na(current_run)] # do i need this?????
      
      if (length(current_run) == 0) {
        break
        # this only happens in later values of i; i did this to ignore rounding issues. 
      }
      
      cat("Trial: ", nn, "ITER:", run, " ", date(), "    ======    Nodes:",min(current_run),"--",max(current_run)," (",length(current_run),")\n",sep = "")
      cat("Trial: ", nn, "ITER:", run, " ", date(), "    ======    Nodes:",min(current_run),"--",max(current_run)," (",length(current_run),")\n",sep = "",
          file = "zGEN_PGL_confest.log", append = TRUE)
      
      # TODO: See how large 'progress' tends to be: passing in a long list is undesirable
      # TODO: When testing timings, because of above, see whether later runs take longer/shorter
      if (length(current_run) > 1) {
        timing = snow.time((results[current_run] = parLapply(clust1, x = current_run, 
                                                             fun = glm_x, 
                                                             prog = progress, 
                                                             run_no = run,
                                                             fit_mode = MODE,
                                                             target_node = TARGET_NODE) )) 
        # glm_x(x = 1, prog = progress, run_no = 1, fit_mode = MODE, target_node = TARGET_NODE)
        # print(timing)
        # TODO: Store 'timing' into a variable, to be able to analyze runtime
      } else if (length(current_run) == 1) {
        results[[current_run]] = glm_x(x = current_run, prog = progress, run_no = run,
                                       mode = MODE, target_node = TARGET_NODE)
      }
    }
    # Save if necessary. 
    # if (SAVE_RESULTS) {
    # cat("Saving... (to TEMP/inprog.Rdata)\n", file = "zGEN_PGL_confest.log", append = TRUE)
    #           if (run %% 3 == 1) {
    #            save(results, file = "inprog.Rdata")
    #          }
    # }
    # TODO: Save progress? this doesnt seem to be a good idea (if time spent saving > time saved if iteration is lost mid-iteration)
    # TODO: Actually use the 'simple' options in the PGL_setup (ie write code to allow using them)
  }
  
  if (SAVE_RESULTS) {
    save(results, file = paste("TEMP/results_run_",nn,".Rdata", sep = ""))
  }
  
  # Process errors: 
  errors = which(sapply(results, function(y) {y$status == "Error"}))
  for (j in errors) {
    results[[j]]$status2 = process_error_results(j)
  }
  
  cat("Extracting Lambda values... \n", file = "zGEN_PGL_confest.log", append = TRUE)
  lambda_matrix = Matrix(0, nrow = NODES, ncol = NODES, sparse = TRUE)
  
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
  
#  if (!is.null(RECORD_TIMING) & RECORD_TIMING) {
#    ind = intersect(which(as.numeric(TIME_matrix[,1]) == nn), 
#                    which(as.numeric(TIME_matrix[,2]) == SAMPLE_SIZES[I_Isamp]))
#    TIME_matrix[ind,3:4] = c(TIME_start, TIME_end)
#    save(TIME_matrix, file = "TIME_data.Rdata") 
#  }
 
}



# TODO: [CLEANUP] Keep code here? or delete if unnecessary
# Old version...
#   spi <- function(x) {
#     # TODO: old code; update in conjunction with later. 
#     return(which(x > 0)[1])
#   }
#   
#   for(j in 1:NODES) {
#     if (results[[j]]$status == "Error") {
#       if (results[[j]]$status2 == "column_all_zeros") {
#         # do nothing (since matrix already has row of zeros)
#       } else {
#         # BE UNHAPPY. 
#         # TODO: 'cat' out some error message. 
#       }
#     } else if (results[[j]]$status == "Done") {
#       locs = apply(results[[j]]$glmnet_model$beta, 1, spi)
#       coords = which(!is.na(locs))
#       adj.coords = coords
#       adj.coords[adj.coords >= j] = adj.coords[adj.coords >= j] + 1
#       lambda_matrix[j,adj.coords] = results[[j]]$glmnet_model$lambda[locs[coords]]
#       if (j %% 250 == 0) { print(j) }
#       # TODO: old code; want to clean/rewrite?
#     } else {
#       # BE UNHAPPY. 
#       # TODO: 'cat' out some error message. 
#     }
#   }
#   
