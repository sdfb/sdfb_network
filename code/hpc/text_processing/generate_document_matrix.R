library(stringr)
library(data.table)


counts_data = readLines("/data/00157/walling/sixdegs/ODNB/matching_files-odnb-splits-dataset3.txt")

#counts_data = readLines("/data/00157/walling/sixdegs/JSTOR/matching_files-jstor-split.txt")

#Example: 
#/data/JSTOR/test/10.2307_1763376:William Walter
#/data/JSTOR/test/10.2307_196432:William Watson

# Split into doc, "10.2307_196432", and name, "William Watson"

parts = str_split(counts_data, ":", simplify=T)
dirs = str_split(parts[,1], "/", simplify=T)
docs = dirs[,ncol(dirs)]
names = parts[,2]


data = data.table(doc=docs, name=names)

# Aggregate to counts by name and docs
data.agg = data[, 'cnt':=.N, by=c('doc', 'name')]

####
# Method: Large
####

# There is a bug in data.table dealing with no support for large vectors in dcast
# So we have to split up into batches.
n = nrow(data.agg)
num.batches = 8
batch.size = n%/%num.batches

data.mat.list = lapply(1:num.batches, function(i) {
  start_idx = (i-1)*batch.size + 1
  end_idx = i*batch.size
  
  data.agg.temp = data.agg[start_idx:end_idx]
  data.mat.temp = dcast(data.agg.temp, doc ~ name)
  return(data.mat.temp)
})

data.mat = rbindlist(data.mat.list, fill=T)

####
# Convert large data.table to a sparse matrix
####

n = nrow(data.mat)
num.batches = 8
batch.size = n%/%num.batches

### 
# Fast/efficient NA removal
###

docs = data.mat[,1]
data.mat = data.mat[,-1]

f_dowle3 = function(DT) {
  # either of the following for loops
  
  # by name :
  #for (j in names(DT))
    #set(DT,which(is.na(DT[[j]])),j,0)
  
  # or by number (slightly faster than by name) :
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}

data.sparse.list = lapply(1:(num.batches+1), function(i) {
  gc()
  
  start_idx = (i-1)*batch.size + 1
  end_idx = i*batch.size
  
  if(end_idx > n) {
    end_idx = n
  }
  
  cat('StartIdx: ', start_idx, ' EndIdx: ', end_idx, fill=T)
  
  data.mat.temp = data.mat[start_idx:end_idx,]
  f_dowle3(data.mat.temp)
  data.sparse.temp = as.matrix(data.mat.temp)
  data.sparse.temp = Matrix(data.sparse.temp, sparse=T)
 
  return(data.sparse.temp)
})

data.sparse = do.call(rbind, data.sparse.list)

rownames(data.sparse) = docs$doc

####
# Method: Small
####

# Reshape to names as columns, docs as rows and counts as the values

#data.mat = dcast(data.agg, doc ~ name)

######
# Cleanup
######

# Free up memory
rm(list=c("data", "data.agg", "dirs", "parts", "data.mat.list"))
gc()


save(data.sparse, file="/data/00157/walling/sixdegs/ODNB/nameslist3-split-docCount.Rdata")

