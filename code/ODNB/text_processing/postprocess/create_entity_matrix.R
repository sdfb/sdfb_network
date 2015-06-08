source("code/ODNB/ODNB_setup.R")

load(zzfile_textproc_post_improvedpred)

# Helper Function ---------------------------------------------------------
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


# Script ------------------------------------------------------------------

## There might be documents messed up by the processing for some reason... identifying and ignoring them: 
good_docs = which(sapply(ODNB_improvedpred, function(x) {!is.null(x) & (class(x) != "try-error")}))



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
