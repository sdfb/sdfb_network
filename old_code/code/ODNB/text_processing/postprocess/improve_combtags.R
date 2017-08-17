##@S This code does generates improved combtags datafile. 

source("code/ODNB/ODNB_setup.R")

HAVE_MANUAL_METADATA = TRUE

if (!HAVE_MANUAL_METADATA) {
  ## TODO: [Cleanup] Process this code here
  
#   ## Old code: relied on my metadata file. probabaly needs fixing?
#   
#   load(zzfile_textproc_ner_combtags)
#   load("data/ODNB_raw/ODNB_metadata20140404.Rdata")
#   
#   ODNB_improvedpred = list()
#   
#   for(j in seq_along(full_metadata$ID)) {
#     cat(j, "::", full_metadata$ID[j], "\n")
#     
#     mainp = NULL
#     if (!is.na(full_metadata$full_name[j])) {
#       s = strsplit(full_metadata$full_name[j], split = ", *")[[1]]
#       mainp = paste(rev(s), collapse = " ")
#       mainp = gsub("[^[:alpha:] ]", "", mainp)
#       mainp = gsub(" +", " ", mainp)
#       mainp = gsub(" $", "", mainp)
#     }
#     
#     ODNB_improvedpred[[full_metadata$ID[j]]] =
#       try(expr = improve_pred(comb.tag = ODNB_combtags[[full_metadata$ID[j]]], main.person = mainp,
#                               regex.words = c("Commonwealth", "Catholic", "Greek", 
#                                               "Roman", "Describe", "Treatise"),
#                               exact.words = c("Abbey", "House", "Island",
#                                               "University", "College", "Cathedral",
#                                               "Pamphlet", "Religion", "Country", 
#                                               "Act", "Restoration", "Empire",
#                                               "Government", "State", "Lawgiving",
#                                               "Palace", "Petition", "Parliament", "Death")) )
#   }
#   
#   ## Some errors might occur; trap these (and ignore for now..?)
#   
#   ## Adjust cell IDS
#   #10yxxxxx -> single ODNB article
#   #20yxxxxx -> multiple people for ODNB id
#   #30yyyyyy -> not matched to ODNB article
#   ## xx's are ODNB ids
#   ## yy's are indicators for counts
#   
#   idnums = full_metadata$ID
#   which(idnums > 99999) -> tofix
#   match(idnums[tofix] - 100000, idnums) -> lower_tofix
#   lower_tofix = lower_tofix[!is.na(lower_tofix)]
#   setdiff(1:length(idnums), union(tofix, lower_tofix)) -> single_arts
#   
#   idnums[single_arts] = idnums[single_arts] + 10000000
#   idnums[tofix] = idnums[tofix] +             20000000
#   idnums[lower_tofix] = idnums[lower_tofix] + 20000000
#   
#   ## Figure out observed date range of biography
#   save(ODNB_improvedpred, full_metadata, file = "../../private_data/odnb_data_proc/ODNB_improvedpred.Rdata")
#   
} else {
  ## Rerunning, when not needing to set ID numbers. 
  
  load(zzfile_textproc_ner_combtags)
  load(zzfile_textproc_preproc_metadata)
  
  ODNB_improvedpred = list()
  for(j in seq_along(full_metadata$ID)) {
    cat(j, "::", full_metadata$ID[j], "\n")
    
    mainp = NULL
    if (!is.na(full_metadata$full_name[j])) {
      s = strsplit(full_metadata$full_name[j], split = ", *")[[1]]
      mainp = paste(rev(s), collapse = " ")
      mainp = gsub("[^[:alpha:] ]", "", mainp)
      mainp = gsub(" +", " ", mainp)
      mainp = gsub(" $", "", mainp)
    }
    
    ODNB_improvedpred[[full_metadata$ID[j]]] =
      try(expr = improve_pred(comb.tag = ODNB_combtags[[full_metadata$ID[j]]], main.person = mainp,
                              regex.words = c("Commonwealth", "Catholic", "Greek", 
                                              "Roman", "Describe", "Treatise"),
                              exact.words = c("Abbey", "University", "College", "Cathedral",
                                              "Pamphlet", "Religion", "Country", 
                                              "Act", "Restoration", "Empire",
                                              "Government", "State", "Lawgiving",
                                              "Palace", "Petition", "Parliament", "Death")) )
  }
  
  ## Some errors might occur; trap these (and ignore for now..?)
  ## No errors in this iteration? don't seem to have found any!
  
  save(ODNB_improvedpred, full_metadata, file = zzfile_textproc_post_improvedpred)  
}

## Count errors
test = sapply(ODNB_improvedpred, function(x) {class(x) == "try-error"})
test = sapply(ODNB_improvedpred, function(x) {nrow(x[[2]])})
test2 = sapply(test, function(x) {ifelse(is.null(x), 0, x)})
