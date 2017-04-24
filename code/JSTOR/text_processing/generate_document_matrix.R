library(stringr)
library(data.table)

counts_data = readLines("/data/00157/walling/sixdegs/matching_files.txt")

#Example: 
#/data/JSTOR/test/10.2307_1763376:William Walter
#/data/JSTOR/test/10.2307_196432:William Watson

# Split into doc, "10.2307_196432", and name, "William Watson"

parts = str_split(counts_data, ":", simplify=T)
docs = str_split(parts[,1], "/", simplify=T)[,7]
names = parts[,2]

data = data.table(doc=docs, name=names)

# Aggregate to counts by name and docs
data.agg = data[, 'cnt':=.N, by=c('doc', 'name')]

# Reshape to names as columns, docs as rows and counts as the values
data.mat = dcast(data.agg2, doc ~ name)

#Dcaast puts the doc name in column 1, remove and then for all the other values to numeric
docs = data.mat[,1]
data.mat = data.mat[,-1]
data.mat[is.na(data.mat)] <- 0
data.mat = as.matrix(data.mat)
rownames(data.mat) = docs

save(data.mat, file="jstor_docCount.Rdata")

