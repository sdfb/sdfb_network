## Code to improve NER results.

source("code/ODNB/ODNB_setup.R")
## TODO: [Obselete] This file should be completely obseleted. 



## TODO: [Cleanup] This file needs code cleanup. 

####### NOTE: ALL THIS COPIED INTO improve_combtags.R
# 
# load(zzfile_textproc_ner_combtags)
# load("data/ODNB_raw/ODNB_metadata20140404.Rdata")
# 
# ODNB_improvedpred = list()
# 
# for(j in seq_along(full_result$ID)) {
#   cat(j, "::", full_result$ID[j], "\n")
#   
#   mainp = NULL
#   if (!is.na(full_result$main_name[j])) {
#     s = strsplit(full_result$main_name[j], split = ", *")[[1]]
#     mainp = paste(rev(s), collapse = " ")
#     mainp = gsub("[^[:alpha:] ]", "", mainp)
#     mainp = gsub(" +", " ", mainp)
#     mainp = gsub(" $", "", mainp)
#   }
# 
#   ODNB_improvedpred[[full_result$ID[j]]] =
#     try(expr = improve_pred(comb.tag = ODNB_combtags[[full_result$ID[j]]], main.person = mainp,
#           regex.words = c("Commonwealth", "Catholic", "Greek", 
#             "Roman", "Describe", "Treatise"),
#           exact.words = c("Abbey", "House", "Island",
#             "University", "College", "Cathedral",
#             "Pamphlet", "Religion", "Country", 
#             "Act", "Restoration", "Empire",
#             "Government", "State", "Lawgiving",
#             "Palace", "Petition", "Parliament", "Death")) )
# }
# 
# ## Some errors might occur; trap these (and ignore for now..?)
# 
# ## Adjust cell IDS
# #10yxxxxx -> single ODNB article
# #20yxxxxx -> multiple people for ODNB id
# #30yyyyyy -> not matched to ODNB article
# ## xx's are ODNB ids
# ## yy's are indicators for counts
# 
# idnums = full_result$ID
# which(idnums > 99999) -> tofix
# match(idnums[tofix] - 100000, idnums) -> lower_tofix
# lower_tofix = lower_tofix[!is.na(lower_tofix)]
# setdiff(1:length(idnums), union(tofix, lower_tofix)) -> single_arts
# 
# idnums[single_arts] = idnums[single_arts] + 10000000
# idnums[tofix] = idnums[tofix] +             20000000
# idnums[lower_tofix] = idnums[lower_tofix] + 20000000
# 
# ## Figure out observed date range of biography
# save(ODNB_improvedpred, full_result, file = "../../private_data/odnb_data_proc/ODNB_improvedpred.Rdata")
##################################################################################################################
# 
# good_docs = which(sapply(ODNB_improvedpred, function(x) {!is.null(x) & (class(x) != "try-error")}))
# 
# good_docs[2132]
# 
# good_docs_dates = t(sapply(good_docs, function(k) {
#   res = range(ODNB_improvedpred[[k]][[3]]$date, na.rm = TRUE)
#   if (!is.finite(res[1])) { return(c(NA, NA)) } else {return (res) }
# }))
# 
# match(good_docs, full_result$ID) -> good_match
# full_result2 = cbind(full_result, bio_min_date = NA, bio_max_date = NA)
# full_result2$bio_min_date[good_match] = good_docs_dates[,1]
# full_result2$bio_max_date[good_match] = good_docs_dates[,2]  
# 
# ## Adjust id numbers
# full_result2$ID = idnums
# 
# find_closest_date(z[[2]], z[[3]]) -> y
# 
# idlist = z[[1]]
# matches_d = y
# aggregate_matches = function(idlist, matches_d) {
#   if (is.null(matches_d)) {return(NULL)}
#   matches_d = matches_d[matches_d$ID > -1,]
#   toreturn = NULL
#   for(j in unique(matches_d$DocSplit)) {
#     matches_date = matches_d[matches_d$DocSplit == j,]
#     ids = unique(matches_date$ID)
#     
#     toret = data.frame(Entity=idlist$MatchName[match(ids, idlist$ID)],
#       ID = idlist$ID[match(ids, idlist$ID)], Segment = j)
#     counts = tapply(rep(1, times = nrow(matches_date)),  matches_date$ID, FUN=sum)
#     min_date = tapply(matches_date$Date, matches_date$ID, min)
#     max_date = tapply(matches_date$Date, matches_date$ID, max)
#   
#     second = data.frame(Count = counts, MinDate = min_date, MaxDate = max_date)
# 
#     
#     toret = cbind(toret, second)
#     toreturn = rbind(toreturn, toret)
#   }
#   toreturn = toreturn[toreturn$Count != 0,]
# 
#   return(toreturn)
# }
# aggregate_matches(z[[1]], y)
# 
# ## Aggregate all the matches in the documents..
# ODNB_aggcounts = list()
# for(k in good_docs) {
#   print(k)
#   z = ODNB_improvedpred[[k]]
#   y = find_closest_date(z[[2]], z[[3]])
#   ODNB_aggcounts[[k]] = aggregate_matches(z[[1]],y)
# }
# 
# partial_entity_matrix = list()
# for(k in 1:100) {
#   print(k)
#   partial_entity_matrix[[k]] = data.frame()
#   for(j in good_docs[(530 * (k-1)) + 1:530]) {
#     if (!is.null(ODNB_aggcounts[[j]])) {
#       res = cbind(ODNB_aggcounts[[j]], DocumentNum = j)
#       partial_entity_matrix[[k]] = rbind(partial_entity_matrix[[k]], res)
#     }
#   }
# }
# 
# big_entity_matrix = data.frame()
# for(j in 1:100) {
#   print(j)
#   big_entity_matrix = rbind(big_entity_matrix, partial_entity_matrix[[j]])
# }
# 
# 
# ls()
# save(big_entity_matrix, file = "../../private_data/odnb_data_proc/ODNB_entitymatrix.Rdata")

## load("../../private_data/odnb_data_proc/ODNB_entitymatrix.Rdata")

head(big_entity_matrix)
tapply(big_entity_matrix$Count, INDEX=big_entity_matrix$Entity, sum) -> total_entity_counts
tapply(rep(1, nrow(big_entity_matrix)), big_entity_matrix$Entity, sum) -> total_entity_doccounts


tocheck = which(total_entity_doccounts > 2)
name_matches = rep(NA, times = length(tocheck))
for(j in seq_along(tocheck)) {
  print(j)
  name_matches[j] = check_entity_in_fullset(names(total_entity_doccounts)[tocheck[j]])
}

non_matches = tocheck[which(name_matches == 0)]

to_rm = c("Society", "Saint", "Christ", "Tax", "Church", "School", "All", "Council", "English", "Union", "Court", "Catholic", "Holy", "Journal", "Friends", "States", "Association", "Social", "Service", "Lake", "Admiral", "Gulf", "Stream", "Manuscript", "Publish", "Sound", "Club", "Company", "Army", "Corps", "Garden", "Player", "London", "Climate", "Ltd", "Memoirs", "Constitution", "Railway", "Poem", "Library", "Commission", "Control", "Third", "Second", "First", "Hospital", "Picture", "Times", "Road", "Memorial", "Bridge", "Crescent", "Institute", "Antiquities", "Lower", "Upper", "Liberal", "Academy", "Office", "Force", "Museum", "International", "News", "Press", "Bible", "Music", "Engineer", "Letter", "Experiment", "United", "Lover", "Trust", "Fort", "Canal", "Coast", "Guild")
bad_match = NULL
for(j in seq_along(to_rm)) {
  bad_match = c(bad_match, grep(to_rm[j], names(non_matches)))
}
non_matches = non_matches[-unique(bad_match)]
sample(non_matches, 100)

head(big_entity_matrix)

mode(big_entity_matrix$Entity)
listnames = as.character(big_entity_matrix$Entity)

which(listnames == names(non_matches[k]))
occur_min = sapply(1:length(non_matches), function(x) {min(big_entity_matrix$MinDate[which(listnames == names(non_matches[x]))], na.rm = TRUE)})
occur_max = sapply(1:length(non_matches), function(x) {max(big_entity_matrix$MaxDate[which(listnames == names(non_matches[x]))], na.rm = TRUE)})

occur_min[!is.finite(occur_min)] = NA
occur_max[!is.finite(occur_max)] = NA

## add occur_min_date/occur_max_date, NumDocAppearances
total_entity_doccounts
match(names(non_matches), names(total_entity_doccounts)) -> z
NumDocAppear = total_entity_doccounts[z]

full_result3 = cbind(full_result2, occur_min_date = NA, occur_max_date = NA, NumDocAppearances = NA)
rep(NA, times = length(non_matches)) -> narep

new_df = data.frame(ID = (30000000 + (1:length(non_matches))), main_name = names(non_matches), full_name = narep, ext_birth = narep, ext_death = narep, full_date = narep, occupation = narep, bio_length = narep, bio_min_date = narep, bio_max_date = narep,
  occur_min_date = occur_min, occur_max_date = occur_max, NumDocAppearances = NumDocAppear, stringsAsFactors = TRUE)

full_result3 = rbind(full_result3, new_df)
head(full_result3)
tail(full_result3)

save(full_result3, file = "../../private_data/odnb_data_proc/ODNB_fullnamelist.Rdata")

write.csv(full_result3, file = "test.csv")




head(non_matches)
