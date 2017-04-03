setwd("/home/walling/dev/git/sdfb_network/code/JSTOR/text_processing")
options(mc.cores=4)
names_file='names_scott_version.csv'
names = readLines(names_file, encoding="UTF-8")

data_path = '/data/JSTOR/'
files = readLines(paste0(data_path, 'uniq_matching_files.txt'))

library(parallel)
doc_results = mclapply(files, function(f) { # For each doc/file
  lines = readLines(f)
  counts = sapply(names, function(n) { # For each name to search
    cat(n)
    count = length(grep(n, lines))
    return(count)
  })
  return(counts) # Length = # of names
}) # Length = # of documents

# Combine Results into document + count
vecs = sapply(doc_results, unlist) # Get the list of list vectors into just a list of vectors
matrix = t(rbind(vecs))

# Drop all zero columns
matrix_final = matrix[, colSums(matrix != 0) > 0]
