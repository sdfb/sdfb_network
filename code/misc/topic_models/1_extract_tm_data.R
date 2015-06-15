## This file contains a script that extracts the segments near the name mentions, in order to build a topic model. 

## of course, this contains only members with a SDFB_ID (otherwise they weren't directly noted in the dataset)
## Also, this ignores any ambiguous matchings (named entities who we aren't sure who the match is)

## Load Data
source("code/ODNB/ODNB_setup.R")
load(zzfile_base_entity_matrix_FULLDATA)
load(zzfile_textproc_post_improvedpred)
subset_em = fix_entitymatrix[fix_entitymatrix$SDFB_NODE_MATCH > 0,]
rm(fix_entitymatrix)


# Helper Functions --------------------------------------------------------

get_seg_bounds = function(nrows, segnum) {
  doclen = 1:nrows
  nsplit = floor(nrows / 500)
  portion = nrows / nsplit
  docsplit = floor(doclen / portion) + 1
  inds = which(docsplit == segnum)
  return(c(min(inds), max(inds)))
}
find_matches_in_bounds = function(ID, matchdf, bounds) {
  matches = intersect(which(matchdf$ID == ID), intersect(which(matchdf$Position >= bounds[1]), which(matchdf$Position <= bounds[2])))
  return(matchdf$Position[matches])
}

# Find the positions where the entties are mentioned in each docum --------

## Figure out which named entites need to be resolved
ids_to_check = sort(unique(subset_em$SDFB_NODE_MATCH))
extract_loc_list = list()

## Loop over entities in SDFB list
for(j in ids_to_check) { 
  extract_loc_list[[j]] = data.frame(docnum = integer(0), position = integer(0))
  
  for(k in which(subset_em$SDFB_NODE_MATCH == j)) { ## For each actor, loop over documents that it's mentioned in
    docnum = subset_em$DocumentNum[k]
    idnum = subset_em$OLD_ID[k]
    
    nrows = nrow(ODNB_improvedpred[[docnum]][[3]])
    bounds = get_seg_bounds(nrows, subset_em$Segment[k])
    
    extract_loc_list[[j]] = rbind(extract_loc_list[[j]], data.frame(docnum = docnum, position = find_matches_in_bounds(ID = idnum, matchdf = ODNB_improvedpred[[docnum]][[2]], bounds = bounds)))
  }
  cat(j, " ")
}


# Create the bag of words -------------------------------------------------

extract_doc_section = function(df, Nbefore = -15, Nafter = 25) { 
  words = NULL
  ## df is in extract_loc_list
  for(docs in unique(df$docnum)) {
    text = ODNB_improvedpred[[docs]][[3]]
    positionlist = lapply(df$position[df$docnum == docs], function(x) { x + Nbefore:Nafter })
    pos = setdiff(unique(c(positionlist, recursive = TRUE)), which(text$tag != ""))
    pos = pos[pos > 0]; pos = pos[pos <= nrow(text)]
    words = c(words, text$raw[pos])
  }
  return(words)
}

doc_section_list = list()
for(j in ids_to_check) { 
  doc_section_list[[j]] = extract_doc_section(df = extract_loc_list[[j]])
  cat(j, " ") 
}

## save raw bag of words 
save(extract_loc_list, doc_section_list, file = "data/TOPIC_MODEL/topic_model_bags_RAW_20150608.Rdata")


