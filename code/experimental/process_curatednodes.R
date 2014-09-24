##@S Examine curated nodes

nodeset = read.csv("data_manual/nodeset_20140923.csv", header = TRUE, stringsAsFactors = FALSE)

tail(nodeset)

hist(nodeset$ODNB_ID)

?read.csv

nodeset$search_names_ALL[589:600]
nodeset[grep("Milton", nodeset$search_names_ALL),]
