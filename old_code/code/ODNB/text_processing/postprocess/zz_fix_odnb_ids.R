##@S Examine curated nodes
source("code/ODNB/ODNB_setup.R")

nodeset = read.csv(zzfile_curated_nodeset, header = TRUE, stringsAsFactors = FALSE)
load(zzfile_textproc_post_entitymatrix)
load(zzfile_textproc_preproc_metadata)



## Fix document number identification issues. 
# nodeset$full_name
full_names = gsub(" +", " ", full_metadata$full_name)
full_dates = gsub("<.+?>", "", full_metadata$Dates)
full_dates = gsub("[()]", "", full_dates)
full_occu = full_metadata$Occu
nodeset_matchnames = lapply(nodeset$full_name, function(x) {which(x == full_names)})
nodeset_matchdates = lapply(nodeset$full_date, function(x) {which(x == full_dates)})
nodeset_matchoccu = lapply(nodeset$occupation, function(x) {which(x == full_occu)})

## Persons who are matched exactly (both full name & full date)
nodeset_matchboth = mapply(FUN = intersect, nodeset_matchnames, nodeset_matchdates)
true_odnb_row = sapply(nodeset_matchboth, function(x) {x[1]})

zeros = which(sapply(nodeset_matchboth, length) == 0)
nodeset_matchzero = mapply(FUN = intersect, nodeset_matchoccu[zeros], nodeset_matchdates[zeros])
true_odnb_row[zeros] = sapply(nodeset_matchzero, function(x) {x[1]})


zerolens = sapply(nodeset_matchzero, length)
which(nodeset$ODNB_ID[zeros[which(zerolens == 0)]] < 30000000)
cbind(which(zerolens > 1), nodeset[zeros[which(zerolens > 1)],])
full_metadata[nodeset_matchzero[which(zerolens > 1)][[7]],]

nodeset[zeros[which(nodeset$ODNB_ID[zeros[which(zerolens == 0)]] < 30000000)],]

## Manual fixes. 
true_odnb_row[1701] = 53490
true_odnb_row[2702] = 54229
true_odnb_row[2886] = 45905
true_odnb_row[3203] = 56063
true_odnb_row[9919] = 56036
true_odnb_row[10716] = 53029
true_odnb_row[12843] = 56041

check_zeros = zeros[which(nodeset$ODNB_ID[zeros[which(zerolens == 0)]] < 30000000)]
adj_findname = sapply(check_zeros, function(x) {intersect(grep(nodeset$first_name[x], full_names), grep(nodeset$surname[x], full_names))}) 

nodeset_matchboth = mapply(FUN = intersect, adj_findname, nodeset_matchdates[check_zeros])
true_odnb_row[check_zeros] = sapply(nodeset_matchboth, function(x) {x[1]})


## ADJUST FINAL DATA SET
nodeset$ODNB_CORRECT_ID = full_metadata$ID[true_odnb_row]
nodeset$ODNB_CORRECT_BIOLENGTH = full_metadata$BioLength[true_odnb_row]

# save(nodeset, full_metadata, file = zzfile_curated_nodeset_update)
