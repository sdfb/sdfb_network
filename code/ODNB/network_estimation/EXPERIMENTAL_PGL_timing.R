#@S Script for running multiple repetitions to obtain timing of network estimation. 
#@S   (few iterations: can be specified)
#@L Since this code is parallelized, there are some system-dependent 
#@L   sections of code (that should have been ran already)

# Notes: Global variables needed: DM (well, at least within each cluster)
#         - function calls expect DM to exist; this is to improve runtime. 
#   - cannot run this as a source(...) since manual input of passwords is necessary

##### (1) load libraries
library(snow)
library(glmnet)

##### (2) load clusters

## This is obviously implementation/cluster dependent. 

CLUSTER_NAMES = c("node68", "node69", "node70", "node71", "node72",
                  "node73", "node74", "node75", "node76", "node77",
                  "node78", "node79", "node81", "node82", "node83",
                  "node84", "node85", "node86", "node87", "node88")
clust1 = makeSOCKcluster(CLUSTER_NAMES)

### need to input password
# stopCluster(clust1)

##### (3) Set Parameters
##### All of these can be manually changed. 
SAVE_RESULTS = FALSE # This takes up a lot of storage... saves all the regression glm models
START_TRIAL = 1 # 1 by default, but can change if continuing a fit...

#FILENAME_MODEL :: This is prefix for model filename

#FILENAME_model = "TEMP/SS_lambdas"      # for 100 iterations, ran 6/30/2013ish
FILENAME_model = "TEMP/SS_test" # testing code

# SINGLE_NODE :: TRUE/FALSE: FALSE is default for fitting all nodes; TRUE => only fit glm on one node
SINGLE_NODE = FALSE
if (SINGLE_NODE) {
  LAMBDA_DEPTH = 4 # Run more values of lambda if only fitting single node
  TARGET_NODE = which(colnames(DM) == "John Milton")
} else {
  LAMBDA_DEPTH = 3
  TARGET_NODE = NULL
}

CLUSTER_NODES = length(CLUSTER_NAMES) # how many cluster nodes [probably should not change this, unless the networking system is changed]
ITER_BATCH = CLUSTER_NODES * 5 # how many to feed into cluster at once
MAX_REITERS = 35 # When rerunning the glm fits, this it the maximum times to try. 
# It really just needs a sensible finite number; very few nodes will ever need 
# more than 5 reiterations (at least at the current lambda values)

# Parameters for many runs of the dataset. 
SAMPLE_SIZES = c(500, 1000, 2000, 3000, 4000, 5000, 6289)
N_TRIALS = 10 # Number of iterations to determine confidence estimate

RECORD_TIMING = TRUE # This var may not exist sometimes
# TODO: Add this as FALSE to other data

TIME_matrix = matrix("",ncol = 4, nrow = (N_TRIALS * length(SAMPLE_SIZES)))
TIME_matrix[,1] = 1:N_TRIALS
TIME_matrix[,2] = rep(SAMPLE_SIZES,each = N_TRIALS)
colnames(TIME_matrix) = c("Iter", "NumNodes", "StartTime", "EndTime")

for(I_Isamp in 1:length(SAMPLE_SIZES)) {
  
  ##### (4) Load functions/data on each run. 
  source("network_estimation/PGL/PGL_setup.R") ### Code is temporarily here. need to write this correctly
  
  # Try on a subset network. 
  load("data/docu_count_matrix/0325sp.datamat.sparse.Rdata")
  
  DM <- sparse.dm[,sample(1:6289, size = SAMPLE_SIZES[I_Isamp])]
  
  ##### (5) Set up parameters, load functions/data into cluster nodes
  
  NODES = ncol(DM)
  
  SAMPLES = list()
  for(j in 1:N_TRIALS) { 
    SAMPLES[[j]] = sort(sample_docs())
  }
  
  save(lambdas, glm_x, process_glm, SAMPLES, DM, MIN_LAMBDAS, file = "TEMP_SS_data.Rdata")
  clusterEvalQ(clust1, library(glmnet))
  clusterEvalQ(clust1, load("TEMP_SS_data.Rdata"))
  
  ##### (6) Start running model. 
  source("network_estimation/PGL/PGL_confest_fitmodel.R") ### Edit this more? make it a function? not sure. 
  
}


