##@S Examine curated nodes
source("code/ODNB/ODNB_setup.R")

load(zzfile_textproc_post_entitymatrix)
load(zzfile_curated_nodeset_update)

# Helper Functions --------------------------------------------------------
generate_weights = function(rows) {
  ## Input is rows, which are the rows in 'nodeset' that can match one specific mention. 
  
  ## The weights here are used to determine who we think is the more likely owner of a unresolved name mention
  ## higher weights are given to people whose biographies are longer.   
  bio_len = nodeset$ODNB_CORRECT_BIOLENGTH[rows]
  n = length(bio_len)
  basewts = rep((1 / (n + 2)), times = n)
  if (any(is.na(bio_len))) {
    ## weight unknown bios as length 100
    bio_len[is.na(bio_len)] = 100
  }
  return(basewts + ((2 / (n+2)) * bio_len / sum(bio_len)))
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

convert_entitymatrix_into_format = function(em, correct_ids) {
  docuseg = as.character(em$DocumentNum + em$Segment / 100)
  ## if neither docn, entity passed in: then correct_ids is vector of appropriate SDFB_IDs. 
  ## in this case, drop rows with NAs. 
  ID = correct_ids
  res = data.frame(SDFB_ID = ID, DocNum = docuseg, Count = em$Count)
  res = res[!(is.na(ID) | ID == 0),]
  return(res)
}




Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# Code --------------------------------------------------------------------


# Create a list of named entities to check for ----------------------------
## Now, we are given a curated nodeset (and each possibly with alternate names -- the work of disambiguation)
##   This step combines all of this into one list -- multiple nodes may request searching the same exact name
##   This step produces a list where each name is identified with a number of nodes that correspond to it. 

## Examine accents in search_all
accents = which(gsub("[], --\\.'[:alpha:]]", "", nodeset$search_names_ALL) != "")

library(stringr)
## For names with accents, use firstname/surname pair as additional name to search. 
search_names = lapply(strsplit(nodeset$search_names_ALL, ","), function(x) {gsub(" +", " ", str_trim(x))})
firstlast_pair = paste(nodeset$first_name, nodeset$surname, sep = " ")

search_vector = unique(c(c(search_names, recursive = TRUE), firstlast_pair))
search_vector = search_vector[!is.na(search_vector)]
search_idlist = list()
## now, check the id list against the search_names fields inside nodeset
# might not be the most efficient direction; runs one time so whatever... 
for(j in seq_along(search_vector)) {
  search_idlist[[j]] = sort(unique(c(which(sapply(search_names, function(x) {any(x == search_vector[j])})),
                                     which(search_vector[[j]] == firstlast_pair)
  )))
  if (j %% 100 == 0) { cat(j, " ") }
}
rm(accents, search_names, j)

## now, the pair search_vector / search_idlist provides information -- 
# search_vector is a vector of character strings to search
# search_idlist provides the corresponding node indices that match each character string (each actual name)


## Noticed that a number of biographies were not correctly ID'ed (possibly related to an ODNB_ID assignment issue earlier), so many biographies did not have their subject identified. (ie in each section of big_entity_matrix, their ID should be 1 (this ID number is reserved for the subject of biographies).) 
## Thus: 
## For each node in the provided nodeset, assign the corresponding ODNB article as their biography if it makes sense to do so (ie. if it seemed to be incorrect)
## Also: store the old ID numbers (so that stuff can be referenced backwards)
## INDEX will just store the index (so can be referenced to a row of big_entity_matrix when necessary)
## SDFB_NODE_MATCH gives the SDFB_ID that corresponds to this person (will be assigned on match later)
fix_entitymatrix = data.frame(big_entity_matrix, OLD_ID = big_entity_matrix$ID, INDEX = seq_len(nrow(big_entity_matrix)), SDFB_NODE_MATCH = 0)

for(j in seq_along(nodeset$ODNB_CORRECT_ID)) {
  ## IF is ODNB article
  if (is.na(nodeset$ODNB_CORRECT_ID[j])) {
    ## do nothing -- this node does not have an ODNB article
  } else {
    id = nodeset$ODNB_CORRECT_ID[j]
    rows = which(fix_entitymatrix$DocumentNum == id)
    ## If no main-author of the article, this might need to be fixed -- 
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
  if (j %% 100 == 0) { cat(j, " ") }
}
rm(firstlast_pair, basenames, matches, id, rows, j)


## Now, need to look for every entry in the list, and find the appropriate matches. 
## There are three types of matches: biography matches (ie. when ID == 1: this means the biography is identified)

##### For biography matches: 
docids = nodeset$ODNB_CORRECT_ID[!is.na(nodeset$ODNB_CORRECT_ID)] ## ODNB IDs in nodeset
bio_matches = fix_entitymatrix[fix_entitymatrix$ID == 1, ] ## Take appropriate subset of fix_entitymatrix to simplify searching
bio_matches = bio_matches[bio_matches$DocumentNum %in% docids,]
doc_match = match(bio_matches$DocumentNum, nodeset$ODNB_CORRECT_ID) ## identify which person each found match belongs to. 

##### For unique matches that are not biography matches (ID != 1), and shared matches: 

sub_entitymatrix = fix_entitymatrix[fix_entitymatrix$ID > 1,]
fuzziness = 10 # years fuzzy on bio date extraction
all_matches = match(sub_entitymatrix$Entity, search_vector)

# Store results
exact_match = rep(NA, times = length(all_matches))
partial_list = list()
partcount = 0

for(j in seq_along(search_vector)[-1]) { #1 is NA
  if (j %% 10 == 0) { cat(j, " ") }
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

exact_df = rbind(convert_entitymatrix_into_format(em = bio_matches, correct_ids = doc_match), 
                 convert_entitymatrix_into_format(em = sub_entitymatrix, correct_ids = exact_match))

out_df = lapply(partial_list, 
                function(x) {res = data.frame(SDFB_ID = x$IDs, DocNum = x$Doc, Count = x$Count, Weight = x$wts)
                             return(res)})
partial_df = do.call(rbind, out_df)

## set SDFB matches in fix_entitymatrix (only for unambiguous matches)
fix_entitymatrix$SDFB_NODE_MATCH[bio_matches$INDEX] = doc_match
fix_entitymatrix$SDFB_NODE_MATCH[sub_entitymatrix$INDEX[!is.na(exact_match)]] = exact_match[!is.na(exact_match)]

save(exact_df, partial_list, partial_df, file = zzfile_base_entity_matrix)
save(search_idlist, search_vector, fix_entitymatrix, file = zzfile_base_entity_matrix_FULLDATA)



