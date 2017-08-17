#@S Attempting to extract the network from the short title catalog, using package XML. 
#@S ---Does not work---

################## attempt to use XML package. doesnt work. 
library(XML)

tables <- readHTMLTable(url)

dim(tables)
names(tables)

tables$centre-table

n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))

tables[[which.max(n.rows)]] -> tab

dim(tab)

tab[,1]
