## Get dates for modeling

source("code/ODNB/ODNB_setup.R")

load(zzfile_curated_nodeset_update)

## TODO: [Document] Add this file to documentation

## Can check for exact dates ('IN' for both later. don't need this function...)
extract_searchable_date = function(nodeset_row) {
  # for birth date: 
  b = nodeset_row$ext_birth
  birth = switch(gsub("/", "", nodeset_row$AF.BF.CA.IN), AF = b, AFIN = b - 1, BF = b - 20, BFIN = b - 20, CA = b - 20, IN = b - 1)
  d = nodeset_row$ext_death
  death = switch(gsub("/", "", nodeset_row$AF.BF.CA.IN2), AF = d + 20, AFIN = d + 20, BF = d, BFIN = d + 1, CA = d + 20, IN = d + 1)
  if (length(c(birth, death)) < 2) {
    if (birth > 0) {
      death = birth + 50
    } else {
      death = birth - 50 
    }
  }
  return(c(birth, death))
}

dates = sapply(seq_along(nodeset$SDFB_ID), function(x) {extract_searchable_date(nodeset[x,])} )
dates = t(dates)

is_overlap = function(datepair1, datepair2) {
  return(datepair1[2] >= datepair2[1] & datepair1[1] <= datepair2[2])
}

date_overlap_list = list()
for(j in 2970:13309) {
  date_overlap_list[[j]] = setdiff(which(sapply(seq_along(dates[,1]), function(y) {is_overlap(dates[j,], dates[y,])})),j)
  print(j)
}

save(date_overlap_list, file = "data/ODNB_final/nodeset_overlap_list.Rdata")
