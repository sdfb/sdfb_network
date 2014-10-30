##@S This file contains code to extract the locations of NER text. 
source("code/ODNB/ODNB_setup.R")

## TODO: [Rewrite] three iterations of this might not be necessary? 

## Treat the title person differently (use extract names function from the pure HTML, and ignore NER results here.
Ntexts = 199999


## The following needs to be done in three parts, due to memory issues... 
## Process combined documents into individual segments.
ODNB_tokenized = list()
for(B in 1:3) {
  inds_iterate = seq(from = 200*(B-1) + 1, to = min(ZNSPLITS, 200*B))
  for(j in inds_iterate) {
    cat("Processing compilation", j, "------\n")
    z = which(txt_R[[j]] == "@@@@@")
    ts = gsub("<.*?>", "", txt_S[[j]])
    y = which(ts == "@@@@@")
    tl = gsub("<.*?>", "", txt_L[[j]])
    x = which(tl == "@@@@@")
    if (all(z == y) & all (z == x)) {
      starts = z
      ends = c(z[-1] - 1, length(txt_R[[j]]))
      docnums = sapply(txt_R[[j]][starts+3], function(x) {as.numeric(strsplit(x, "=")[[1]][2])})
      for(k in 1:length(docnums)) {
        if (k %% 10 == 0) { print(k) }
        main = (starts[k] + 6):(ends[k] - 2)
        ODNB_tokenized[[docnums[k]]] = list()
        ODNB_tokenized[[docnums[k]]]$raw = format_text_and_split(txt_R[[j]][main])
        ODNB_tokenized[[docnums[k]]]$st = normalize_tagtext(
          txt_S[[j]][main], ODNB_tokenized1[[docnums[k]]]$raw, "ST")
        ODNB_tokenized[[docnums[k]]]$lp = normalize_tagtext(
          txt_L[[j]][main], ODNB_tokenized1[[docnums[k]]]$raw, "LP")
      }
    } else {
      stop(paste("ERROR: Mismatching input lengths in compilation", j))
    }
  }
  save(ODNB_tokenized, file = zzfile_textproc_ner_token_vec[B])
  rm(ODNB_tokenized)
}

