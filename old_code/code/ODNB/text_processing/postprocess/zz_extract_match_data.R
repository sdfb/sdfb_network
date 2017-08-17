
# outputting data ---------------------------------------------------------
## TODO: to move -- output stuff for chris. 
source("code/ODNB/ODNB_setup.R")
load(zzfile_base_entity_matrix)
write.csv(exact_df, file = "exact_matches_new.csv",row.names = FALSE )
write.csv(partial_df, file = "partial_matches_new.csv",row.names = FALSE )
load(zzfile_textproc_post_improvedpred)

extract_doccount = function(j) {
  if (is.null(ODNB_improvedpred[[j]])) {return(NULL)}
  if (class(ODNB_improvedpred[[j]]) == "try-error") {return(NULL)}
  x = ODNB_improvedpred[[j]][[2]]
  if (nrow(x) == 0) {return(NULL)}
  res = table(x$MatchName)
  res = data.frame(res, j,stringsAsFactors = FALSE)
  colnames(res) = c("Entity", "Count", "DocNum")
  return(res)
}

templist = list()
for(k in 1:99) {
  print(k)
  test = lapply(1000 * (k -1) + 1:1000, extract_doccount)
  templist[[k]] = do.call(rbind, test)
}
templist[[100]] = do.call(rbind, lapply(99001:length(ODNB_improvedpred), extract_doccount))


raw_doccount = do.call(rbind, templist)
write.csv(raw_doccount, file = "raw_doccount.csv",row.names = FALSE )

load(zzfile_curated_nodeset_update)
write.csv(nodeset, file = "updated_nodeset.csv", row.names = FALSE)

hist(table(exact_df$SDFB_ID), xlim = c(0, 100), breaks = 2500)
nrow(nodeset)
length(table(exact_df$SDFB_ID))
