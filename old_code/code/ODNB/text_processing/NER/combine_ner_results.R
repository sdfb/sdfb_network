##@S This file contains code that combines the compiled tagged tokenized versions of the documents.
source("code/ODNB/ODNB_setup.R")

comb_order = c("ST:PERSON", "LP:PERSON", "ST:ORGANIZATION", "LP:ORGANIZATION")
ODNB_combtags = list()

for(B in 1:3) {
  cat("Processing large compliation number", B, "\n")
  load(zzfile_textproc_ner_token_vec[B])
  ## Figure out which ids are non-empty
  ids = which(sapply(ODNB_tokenized, function(x) {!is.null(x)}))
  for(j in ids) {
    cat(j, "")
    combtag = cbind(proc_tagtext(ODNB_tokenized[[j]]$st, "ST"), proc_tagtext(ODNB_tokenized[[j]]$lp, "LP"))
    comb_index = match(comb_order, colnames(combtag))
    temp = combtag[,comb_index[1]]
    for(k in 1:length(comb_index)) { temp = combine_two(a = temp, b = combtag[,comb_index[k]]) }
    ODNB_combtags[[j]] = 
      data.frame(raw = ODNB_tokenized[[j]]$raw, tag = temp,
                 date = tag_dates(ODNB_tokenized[[j]]$raw), 
                 paren = compute_parenthesis(ODNB_tokenized[[j]]$raw), stringsAsFactors = FALSE)
  }
  print("--- Saving partial progress...")
  rm(ODNB_tokenized)
  save(ODNB_combtags, file = zzfile_textproc_ner_combtags)
}

