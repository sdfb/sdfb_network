##@S This file contains code that combines the compiled tagged tokenized versions of the documents.
source("code/ODNB/ODNB_setup.R")


## TODO: [Rewrite] This script probably needs to be made 'less hacky'. 

## parameters
PARAM_START = 2

## This also notes where parenthesis are, and also attempts to date-tag the data.




# combined tags: 
# ORDER in proc.docs :
# Text ST:PERSON ST:ORGANIZATION ST:LOCATION LP:PERSON LP:ORGANIZATION LP:LOCATION
#combine.index = match(combine.order, colnames(proc.docs[[1]]))
#combined.tags = list()
#for(i in 1:18150) {
#  if (!is.null(proc.docs[[i]])) {
#    temp = proc.docs[[i]][,combine.index[1]]
#    for(j in 2:length(combine.index)) {
#      temp = combine_two(a=temp, b=proc.docs[[i]][,combine.index[j]])
#|            ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--
#    }
#    combined.tags[[i]] = cbind(proc.docs[[i]][,1],temp)
#  }
#  print(i)
#}
comb_order = c("ST:PERSON", "LP:PERSON", "ST:ORGANIZATION", "LP:ORGANIZATION")
  
if (PARAM_START < 2) {
  ODNB_combtags = list()
  
  load(zzfile_textproc_ner_token1)

  ids = which(sapply(ODNB_tokenized1, function(x) {!is.null(x)}))

  for(j in ids) {
    combtag = cbind(
      proc_tagtext(ODNB_tokenized1[[j]]$st, "ST"),
      proc_tagtext(ODNB_tokenized1[[j]]$lp, "LP"))
    
    comb_index = match(comb_order, colnames(combtag))
    temp = combtag[,comb_index[1]]
    for(k in 1:length(comb_index)) {
      temp = combine_two(a = temp, b = combtag[,comb_index[k]])
    }
    
    ODNB_combtags[[j]] = data.frame(
                   raw = ODNB_tokenized1[[j]]$raw,
                   tag = temp,
                   date = tag_dates(ODNB_tokenized1[[j]]$raw),
                   paren = compute_parenthesis(ODNB_tokenized1[[j]]$raw)
                   )
    print(j)
  }

  save(ODNB_combtags, file = zzfile_textproc_ner_combtags)
} 


### Section 2
if (PARAM_START < 3) {
  rm(ODNB_tokenized1)

  if (PARAM_START > 1) {
    load(zzfile_textproc_ner_combtags)
  }
  load(zzfile_textproc_ner_token2)
  ids = which(sapply(ODNB_tokenized2, function(x) {!is.null(x)}))
  
  for(j in ids) {
    combtag = cbind(
      proc_tagtext(ODNB_tokenized2[[j]]$st, "ST"),
      proc_tagtext(ODNB_tokenized2[[j]]$lp, "LP"))
    
    comb_index = match(comb_order, colnames(combtag))
    temp = combtag[,comb_index[1]]
    for(k in 1:length(comb_index)) {
      temp = combine_two(a = temp, b = combtag[,comb_index[k]])
    }
    
    ODNB_combtags[[j]] = data.frame(
                   raw = ODNB_tokenized2[[j]]$raw,
                   tag = temp,
                   date = tag_dates(ODNB_tokenized2[[j]]$raw),
                   paren = compute_parenthesis(ODNB_tokenized2[[j]]$raw)
                   )
    print(j)
  }
  
  save(ODNB_combtags, file = zzfile_textproc_ner_combtags)
}

if (PARAM_START < 4) {
  ##Section 3
  
  if (PARAM_START > 2) {
    load(zzfile_textproc_ner_combtags)
  }
  
  rm(ODNB_tokenized2)
  load(zzfile_textproc_ner_token3)
  ids = which(sapply(ODNB_tokenized3, function(x) {!is.null(x)}))
  
  for(j in ids) {
    combtag = cbind(
      proc_tagtext(ODNB_tokenized3[[j]]$st, "ST"),
      proc_tagtext(ODNB_tokenized3[[j]]$lp, "LP"))
    
    comb_index = match(comb_order, colnames(combtag))
    temp = combtag[,comb_index[1]]
    for(k in 1:length(comb_index)) {
      temp = combine_two(a = temp, b = combtag[,comb_index[k]])
    }
    
    ODNB_combtags[[j]] = data.frame(
                   raw = ODNB_tokenized3[[j]]$raw,
                   tag = temp,
                   date = tag_dates(ODNB_tokenized3[[j]]$raw),
                   paren = compute_parenthesis(ODNB_tokenized3[[j]]$raw)
                   )
    print(j)
  }
  save(ODNB_combtags, file = zzfile_textproc_ner_combtags)
}
