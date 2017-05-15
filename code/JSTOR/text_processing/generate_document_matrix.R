library(stringr)

setwd("/home/walling/dev/git/sdfb_network/code/JSTOR/text_processing")
options(mc.cores=4)

counts_data = readLines("/data/JSTOR/matching_files.txt")

#Example: 
#/data/JSTOR/test/10.2307_1763376:William Walter
#/data/JSTOR/test/10.2307_196432:William Watson

# Split into doc, "10.2307_196432", and name, "William Watson"

parts = str_split(counts_data, ":", simplify=T)
docs = str_split(parts[,1], "/", simplify=T)[,5]
names = parts[,2]

data = data.frame(doc=docs, name=names)

# Aggregate to counts by name and docs
library(sqldf)
data.agg = sqldf("select doc, name, count(*) as cnt
                 from data
                 group by 1, 2")

# Reshape to names as columns, docs as rows and counts as the values
library(data.table)
data.mat = dcast(data.agg, doc ~ name)

#Dcaast puts the doc name in column 1, remove and then for all the other values to numeric
#rownames(data.mat) = data.mat[,1]
data.mat = data.mat[,-1]
#data.mat = apply(data.mat, 1, as.numeric) 
data.mat[is.na(data.mat)] <- 0
data.mat2 = as.matrix(data.mat)

save(data.mat, file="jstor_docCount.Rdata")

