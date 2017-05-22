library(stringr)
library(data.table)

#setwd("/home/walling/dev/git/sdfb_network/code/JSTOR/text_processing")
options(mc.cores=24)

counts_data = readLines("/gpfs/flash/users/walling/sixdegs/data/JSTOR/matching_files-jstor-split.txt")

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

# Reshape to names as columns, docs as rows and counts as the values
data.mat = dcast(data.agg, doc ~ name)

#Dcaast puts the doc name in column 1, remove and then for all the other values to numeric
docs = data.mat[,1]

data.mat = data.mat[,-1]
data.mat[is.na(data.mat)] <- 0

data.mat = as.matrix(data.mat)
rownames(data.mat) = docs$doc


save(data.mat, file="/gpfs/flash/users/walling/sixdegs/data/JSTOR/jostr-split-docCount.Rdata")

