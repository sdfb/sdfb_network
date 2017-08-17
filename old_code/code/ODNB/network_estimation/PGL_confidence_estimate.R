#@S Script for running PGL + ConfEst.
#@L Since this code is parallelized, there are some system-dependent sections of code

# Notes: Global variables needed: DM (well, at least within each cluster)
#         - function calls expect DM to exist; this is to improve runtime. 
#   - cannot run this as a source(...) since manual input of passwords is necessary

# TODO: rewrite all code in here
# TODO: Find code where I threw away most of the fitted model data to improve runtime vastly

# TODO: write separate code for running a single fit for a large number of runs?
# TODO: See if parLapplyLB will improve time on parLapply. 

# TODO: [TEST] See if single-node fitting works. pull code from this otherwise. 
# TODO: [TEST] See if anythin broke: run pgl with 10 nodes, compare. 

##### (1) load libraries
library(snow)
library(glmnet)

##### (2) load clusters
setwd("/")
CLUSTER_NAMES = c("node68", "node69", "node70", "node71",
                  "node73", "node75", "node76", "node77",
                  "node78", "node79", "node81", "node83",
                  "node84", "node85", "node86", "node87", 
                  "node88",
                  "node89", "node90", "node92", "node93",
                  "node94", "node95", "node96", "node97")

clust1 = makeSOCKcluster(CLUSTER_NAMES)
setwd("/SDFB/")
### need to input password
# stopCluster(clust1)

##### (3) Load functions

# TODO: Do something better about parameters. 

LAMBDA_DEPTH= 3
source("code/PGL_setup.R") ### Code is temporarily here. need to write this correctly

##### (4) Load data 
load("data/ODNB_final/nodeset_overlap_list.Rdata")
ALLOWED_INDS = date_overlap_list
# Try on a subset network. 
# load("data/docu_count_matrix/0325sp.datamat.sparse.Rdata")
# load("data/docu_count_matrix/041514_sparsedm.Rdata")
# 
# DM <- f_count_mat

##### (5) Set up parameters, load functions/data into cluster nodes
##### All of these can be manually changed. 
SAVE_RESULTS = FALSE # This takes up a lot of storage... saves all the regression glm models
N_TRIALS = 100 # Number of iterations to determine confidence estimate
START_TRIAL = 1 # 1 by default, but can change if continuing a fit...

#FILENAME_MODEL :: This is prefix for model filename

#FILENAME_model = "TEMP/SS_lambdas"      # for 100 iterations, ran 6/30/2013ish
FILENAME_model = "TEMP/SS_newfit"        # 100 iterations on new data, 4/15/2014
RECORD_TIMING = NULL


# SINGLE_NODE :: TRUE/FALSE: FALSE is default for fitting all nodes; TRUE => only fit glm on one node
## MDOE - "default", "single", or "date-limited". 
# TODO: Document this in more detail. 
MODE = "date-limited"
# if (SINGLE_NODE) {
#   LAMBDA_DEPTH = 4 # Run more values of lambda if only fitting single node
#   TARGET_NODE = which(colnames(DM) == "John Milton")
# } else {
#   LAMBDA_DEPTH = 3
#   TARGET_NODE = NULL
# }


CLUSTER_NODES = length(CLUSTER_NAMES) # how many cluster nodes [probably should not change this, unless the networking system is changed]
ITER_BATCH = CLUSTER_NODES * 5 # how many to feed into cluster at once
MAX_REITERS = 100 # When rerunning the glm fits, this it the maximum times to try. 
# It really just needs a sensible finite number; very few nodes will ever need 
# more than 5 reiterations (at least at the current lambda values)

NODES = length(ALLOWED_INDS)

# TODO: timing of runtime... need procedure to do this. 

#SAMPLES = list()
#for(j in 1:N_TRIALS) { 
#  SAMPLES[[j]] = sort(sample_docs())
#}

#save(lambdas, glm_x, process_glm, SAMPLES, DM, MIN_LAMBDAS, file = "TEMP_SS_data.Rdata")

save(lambdas, glm_x, process_glm, ALLOWED_INDS, MIN_LAMBDAS, file = "TEMP_SS_data.Rdata")

clusterEvalQ(clust1, library(glmnet))
clusterEvalQ(clust1, load("TEMP_SS_data.Rdata"))

##### (6) Start running model.
source("network_estimation/PGL/PGL_confest_fitmodel.R") ### Edit this more? make it a function? not sure. 

##### (7) Collapse fit files into one confidence matrix
## TODO: [Modify Code] This needs to be rewritten. 

conf_matrix = Matrix(0, nrow = NODES, ncol = NODES, sparse = TRUE)

for(j in 1:N_TRIALS) {
  fname = paste(FILENAME_model,j,".Rdata", sep = "")
  load(fname)
  or_rule = (lambda_matrix > 0) + t(lambda_matrix) > 0
  conf_matrix = conf_matrix + or_rule
  print(j)
}

# Save into what file? (commented out to avoid replacing on accident.)
# save(conf_matrix, file = "conf_matrix.Rdata")

##### (8) Optional collapse: Only if not all files exist..

FILENAME_model = "TEMP/SS_newfit"       
library(Matrix)

load("data/docu_count_matrix/041514_sparsedm.Rdata")
DM <- f_count_mat
NODES = ncol(DM)


conf_matrix = Matrix(0, nrow = NODES, ncol = NODES, sparse = TRUE)
count_matrix = 1

for(j in 1:N_TRIALS) {
  fname = paste(FILENAME_model,j,".Rdata", sep = "")
  exists = try(load(fname))
  if ((class(exists) != "try-error" ) & (count_matrix < 40)) {
    count_matrix = count_matrix + 1
    or_rule = (lambda_matrix > 0) + t(lambda_matrix) > 0
    conf_matrix = conf_matrix + or_rule
    print(j)
  }
}

