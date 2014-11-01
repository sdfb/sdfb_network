source("code/ODNB/ODNB_setup.R")

load(zzfile_textproc_post_improvedpred)

## Look for documents which have screwed up during processing for some reason or other. there are a couple hundred... 
good_docs = which(sapply(ODNB_improvedpred, function(x) {!is.null(x) & (class(x) != "try-error")}))

## OLD CODE --> full_result2
# good_docs_dates = t(sapply(good_docs, function(k) {
#   res = range(ODNB_improvedpred[[k]][[3]]$date, na.rm = TRUE)
#   if (!is.finite(res[1])) { return(c(NA, NA)) } else {return (res) }
# }))
# 
# match(good_docs, full_result$ID) -> good_match
# ## ERROR ^
# 
# full_result2 = cbind(full_result, bio_min_date = NA, bio_max_date = NA)
# full_result2$bio_min_date[good_match] = good_docs_dates[,1]
# full_result2$bio_max_date[good_match] = good_docs_dates[,2]  
# 
# ## Adjust id numbers
# full_result2$ID = idnums
# 
# z = ODNB_improvedpred[[3]]
# find_closest_date(z[[2]], z[[3]]) -> y
# 
# idlist = z[[1]]
# matches_d = y

aggregate_matches = function(idlist, matches_d) {
  if (is.null(matches_d)) {return(NULL)}
  matches_d = matches_d[matches_d$ID > -1,]
  toreturn = NULL
  for(j in unique(matches_d$DocSplit)) {
    matches_date = matches_d[matches_d$DocSplit == j,]
    ids = unique(matches_date$ID)
    
    toret = data.frame(Entity=idlist$MatchName[match(ids, idlist$ID)],
                       ID = idlist$ID[match(ids, idlist$ID)], Segment = j)
    counts = tapply(rep(1, times = nrow(matches_date)),  matches_date$ID, FUN=sum)
    min_date = tapply(matches_date$Date, matches_date$ID, min)
    max_date = tapply(matches_date$Date, matches_date$ID, max)
    
    second = data.frame(Count = counts, MinDate = min_date, MaxDate = max_date)
    
    
    toret = cbind(toret, second)
    toreturn = rbind(toreturn, toret)
  }
  toreturn = toreturn[toreturn$Count != 0,]
  
  return(toreturn)
}
# aggregate_matches(z[[1]], y)

## Aggregate all the matches in the documents..
ODNB_aggcounts = list()
for(k in good_docs) {
  print(k)
  z = ODNB_improvedpred[[k]]
  y = find_closest_date(z[[2]], z[[3]])
  ODNB_aggcounts[[k]] = aggregate_matches(z[[1]],y)
}
rm(ODNB_improvedpred)

## Now, combine the list of data frames into a large data frame. 
## do.call(rbind) works but seems hard to check progress of... 

partial_entity_matrix = list()
Splitsize = 100
split_list = split(1:length(ODNB_aggcounts), ceiling((1:length(ODNB_aggcounts))/Splitsize))

for(k in seq_along(split_list)) {
  print(k)
  cur_list = lapply(split_list[[k]], function(x) {
    if (!is.null(ODNB_aggcounts[[x]])) { 
      return(cbind(ODNB_aggcounts[[x]], DocumentNum = x))
    } else { return(NULL) }
  })
  partial_entity_matrix[[k]] = do.call(rbind, cur_list)
}

Splitsize = 100
split_list = split(1:length(partial_entity_matrix), ceiling((1:length(partial_entity_matrix))/Splitsize))
bigger_entity_matrix = list()
for(k in seq_along(split_list)) {
  print(k)
  bigger_entity_matrix[[k]] = do.call(rbind, partial_entity_matrix[split_list[[k]]])
}

big_entity_matrix = do.call(rbind, bigger_entity_matrix)

save(big_entity_matrix, file = zzfile_textproc_post_entitymatrix)
