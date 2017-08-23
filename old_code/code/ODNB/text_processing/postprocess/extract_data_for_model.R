## Create data matrices

source("code/ODNB/ODNB_setup.R")
load(zzfile_base_entity_matrix)
load(zzfile_curated_nodeset_update)

## Given information about exact data matrix (exact_df, )
library(Matrix)

# Helper Functions --------------------------------------------------------

create_sparse_mat_from_entity_df = function(df, rows, cols, verbose = TRUE) {
  ## Outputs count at every 5% progress
  res = Matrix(0, nrow = length(rows), ncol = length(cols), sparse = TRUE)
  
  rowmatches = match(df$DocNum, rows) 
  for(j in 1:nrow(df)) {
    if ((j %% ceiling(nrow(df) / 20) == 0) & verbose) { print(paste(j, "/", nrow(df))) }
    res[rowmatches[j], df$SDFB_ID[j]] = df$Count[j]
  }
  rownames(res) = rows
  colnames(res) = cols
  return(res)
}

sample_uncertain_doccounts = function(partial_list, rownums = NULL) {
  ## If rownums is null, samples entirity of 'partial_list'
  ## Else, only samples the specific rows. 
  
  ## Used to be 'sample_colnums'
  if (!is.null(rownums)) {
    DocSeg = sapply(partial_list, function(x) {x$Doc})
    which(!is.na(match(DocSeg, rownums))) -> tokeep
    partial_list = partial_list[tokeep]
  }
  
  res = data.frame(
    Count = sapply(partial_list, function(x) { x$Count }),
    DocNum = sapply(partial_list, function(x) { x$Doc }),
    SDFB_ID = sapply(partial_list, function(x) { sample(x$IDs, prob = x$wts, size = 1) }),
    stringsAsFactors = FALSE)
  return(res) 
}

sample_matrix = function(frac = 0.5) {
  ## Requires base_dcmat, partial_list
  keep_rows = sort(sample(1:nrow(base_dcmat), size = floor(nrow(base_dcmat) * frac)), decreasing = FALSE)
  part_df = sample_uncertain_doccounts(partial_list = partial_list)
  res = base_dcmat + create_sparse_mat_from_entity_df(df = part_df, rows = rownames(base_dcmat), cols = colnames(base_dcmat))
  return(res[keep_rows,])
}


# Code --------------------------------------------------------------------

doccount_rows = sort(as.numeric(c(unique(c(as.character(exact_df$DocNum), sapply(partial_list, function(x) {x$Doc}))), recursive = TRUE)))
base_dcmat = create_sparse_mat_from_entity_df(df = exact_df, rows = doccount_rows, cols = 1:nrow(nodeset))

test = sample_matrix()

for(j in 1:100) {
  print("****************")
  print(j)
  filename = paste("data/ODNB_newfinal/sampmatrix", j,".Rdata", sep = "")
  dcmat = sample_matrix()
  save(dcmat, file = filename)
}

