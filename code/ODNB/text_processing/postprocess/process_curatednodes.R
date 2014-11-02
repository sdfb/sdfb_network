##@S Examine curated nodes
source("code/ODNB/ODNB_setup.R")

nodeset = read.csv(zzfile_curated_nodeset, header = TRUE, stringsAsFactors = FALSE)
load(zzfile_textproc_post_entitymatrix)
load(zzfile_textproc_preproc_metadata)

# Helper Functions --------------------------------------------------------
generate_weights = function(rows) {
  ## takes rows in nodeset & generates appropriate weights
  z = nodeset$ODNB_CORRECT_BIOLENGTH[rows]
  n = length(z)
  basewts = rep((1 / (n + 2)), times = n)
  if (any(is.na(z))) {
    ## weight unknown bios as length 100
    z[is.na(z)] = 100
  }
  return(basewts + ((2 / (n+2)) * z / sum(z)))
}

check_date_overlap = function(docdates, biodates) {
  n = nrow(docdates)
  res = list()
  for(j in 1:n) {
    if (is.na(docdates[j,1])) {
      res[[j]] = 1:nrow(biodates)
    } else {
      res[[j]] = which(biodates[,1] < docdates[j,2] & biodates[,2] > docdates[j,1])
    }
  }
  return(res)
}

## TODO: [Move code] Move and document this function...? but not generally usable. 
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

convert_entitymatrix_into_format = function(em, docn = NULL, entity = NULL, correct_ids) {
  ## either pass in true docnums or entity vector, and corresponding correct_ids, 
  docuseg = as.character(em$DocumentNum + em$Segment / 100)
  if (!is.null(docn)) {
    ID = correct_ids[match(em$DocumentNum, docn)]
  } else if (!is.null(entity)) {
    ID = correct_ids[match(em$Entity, entity)]
  } else {
    ## if neither docn, entity passed in: then correct_ids is vector of appropriate SDFB_IDs. 
    ## in this case, drop rows with NAs. 
    ID = correct_ids
    res = data.frame(SDFB_ID = ID, DocNum = docuseg, Count = em$Count)
    res = res[!(is.na(ID) | ID == 0),]
    return(res)
  }
  res = data.frame(SDFB_ID = ID, DocNum = docuseg, Count = em$Count)
  return(res)
}


# Code --------------------------------------------------------------------

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

save(nodeset, full_metadata, file = zzfile_curated_nodeset_update)
  


# Continue with processing for final matrix -------------------------------

## Examine accents in search_all
accents = which(gsub("[], --\\.'[:alpha:]]", "", nodeset$search_names_ALL) != "")

library(stringr)
## For names with accents, use firstname/surname pair as additional name to search. 
search_names = lapply(strsplit(nodeset$search_names_ALL, ","), function(x) {gsub(" +", " ", str_trim(x))})
firstlast_pair = paste(nodeset$first_name, nodeset$surname, sep = " ")

search_vector = unique(c(c(search_names, recursive = TRUE), firstlast_pair))
search_idlist = list()
for(j in seq_along(search_vector)) {
  search_idlist[[j]] = sort(unique(c(which(sapply(search_names, function(x) {any(x == search_vector[j])})),
                                     which(search_vector[[j]] == firstlast_pair)
  )))
  if (j %% 25 == 0) { print(j) }
}



# z = tapply(big_entity_matrix$ID, big_entity_matrix$DocumentNum, FUN = function(x) {any(x == 1)})
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# y = tapply(big_entity_matrix$ID, big_entity_matrix$DocumentNum, Mode)

# 
# 
# ## Look for exact matches in the ODNB, for each of search_all
# s = as.numeric(sample(names(which(!z)), size = 30))
# s
# t = which((nodeset$ODNB_ID %% 100000) %in% s)
# nodeset[t,]
# big_entity_matrix[which(big_entity_matrix$DocumentNum %in% (nodeset$ODNB_ID[t] %% 100000)),]
# agrep(pattern = search_names[[t[3]]], x = big_entity_matrix[which(big_entity_matrix$DocumentNum %in% (nodeset$ODNB_ID[t] %% 100000)),1], max.distance = 0.2)
# nodeset[2800,]
# agrep(pattern = "John A Lasco", x = big_entity_matrix$Entity[1:100], max.distance = 0.2)
# 
# 

## For each ODNB name, give it the most frequent name in article if it doesn't have one. 
## Adjust big_entity_matrix...
fix_entitymatrix = big_entity_matrix

for(j in seq_along(nodeset$ODNB_CORRECT_ID)) {
  ## IF is ODNB article
  if (is.na(nodeset$ODNB_CORRECT_ID[j])) {
    
  } else {
    id = nodeset$ODNB_CORRECT_ID[j]
    rows = which(fix_entitymatrix$DocumentNum == id)
    ## If no main-author --
    if (!(any(fix_entitymatrix$ID[rows] == 1))) {
      ## If there is a close match in top 5 of count: 
      basenames = names(sort(tapply(fix_entitymatrix$Count[rows], as.character(fix_entitymatrix$Entity[rows]), sum), decreasing = TRUE))
      basenames = basenames[1:min(5, length(basenames))]
      matches = agrep(pattern = firstlast_pair[j], x = basenames, max.distance = 0.4)
      if (length(matches) > 0) {
        fix_entitymatrix$ID[rows[which(fix_entitymatrix$Entity[rows] == basenames[matches[1]])]] = 1
      }
    }
  }
  if (j %% 25 == 0) {print(j)}
}



# Ran until here ------ ---------------------------------------------------




## Three types of matches
## bio matches => ID = 1
## unique matches => ID != 1
## shared matches
docids = nodeset$ODNB_CORRECT_ID[!is.na(nodeset$ODNB_CORRECT_ID)]
bio_matches = fix_entitymatrix[fix_entitymatrix$ID == 1, ]
bio_matches = bio_matches[bio_matches$DocumentNum %in% docids,]

# convert_entitymatrix_into_format(bio_matches, docn = docids, correct_ids = seq_along(docids))

sub_entitymatrix = fix_entitymatrix[fix_entitymatrix$ID > 1,]
fuzziness = 10 # years fuzzy on bio date extraction
all_matches = match(sub_entitymatrix$Entity, search_vector)

# Store results
exact_match = rep(0, times = length(all_matches))
partial_list = list()
partcount = 0

for(j in seq_along(search_vector)[-1]) { #1 is NA
  print(j)
  matches = which(all_matches == j)
  if (length(matches) > 0) {
    docdates = cbind(sub_entitymatrix$MinDate[matches] - fuzziness, sub_entitymatrix$MaxDate[matches] + 10)
    biodates = lapply(search_idlist[[j]], function(x) {extract_searchable_date(nodeset[x,])})
    biodates = do.call(rbind, biodates)
    rownames(docdates) = NULL
    dateoverlap = check_date_overlap(docdates, biodates)
    for(k in seq_along(dateoverlap)) {
      subid = matches[k]
      if (length(dateoverlap[[k]]) > 0) {
        if (length(dateoverlap[[k]]) == 1) {
          exact_match[subid] = search_idlist[[j]][dateoverlap[[k]]]
        } else {
          partcount = partcount+1
          docuseg = as.character(sub_entitymatrix$DocumentNum[subid] + sub_entitymatrix$Segment[subid] / 100)
          partial_list[[partcount]] = list(Doc = docuseg, Count = sub_entitymatrix$Count[subid],
                                           IDs = search_idlist[[j]][dateoverlap[[k]]], 
                                           wts = generate_weights(search_idlist[[j]][dateoverlap[[k]]]))
        }
      }
    }
  }
}

non_bio_exact_match = convert_entitymatrix_into_format(em = sub_entitymatrix, correct_ids = exact_match)
# partial_list

exact_df = rbind(convert_entitymatrix_into_format(bio_matches, docn = docids, correct_ids = seq_along(docids)), non_bio_exact_match)
# head(partial_list)

out_df = lapply(partial_list, 
                function(x) {res = data.frame(SDFB_ID = x$IDs, DocNum = x$Doc, Count = x$Count, Weight = x$wts)
                             return(res)})
partial_df = do.call(rbind, out_df)

## TODO: [DOCUMENT] this data format
save(exact_df, partial_list, partial_df, file = zzfile_base_entity_matrix)




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
for(k in 1:199) {
  print(k)
  test = lapply(1000 * (k -1) + 1:1000, extract_doccount)
  templist[[k]] = do.call(rbind, test)
}
templist[[200]] = do.call(rbind, lapply(190001:length(ODNB_improvedpred), extract_doccount))


raw_doccount = do.call(rbind, templist)
write.csv(raw_doccount, file = "raw_doccount.csv",row.names = FALSE )

