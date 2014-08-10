##@S This file contains code that combines the compiled tagged tokenized versions of the documents.

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
#      temp = combine.two(a=temp, b=proc.docs[[i]][,combine.index[j]])
#    }
#    combined.tags[[i]] = cbind(proc.docs[[i]][,1],temp)
#  }
#  print(i)
#}
comb_order = c("ST:PERSON", "LP:PERSON", "ST:ORGANIZATION", "LP:ORGANIZATION")
  
if (PARAM_START < 2) {
  ODNB_combtags = list()
  
  load("data/ODNB_intermediate/NER/ODNB_NERtokenized20140202_1.Rdata")
#|      ***********************************************************
#|----##Fix old directory structure --Sat Aug  9 20:41:24 2014--

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

  save(ODNB_combtags, file = "private_data/odnb_data_proc/ODNB_combtags.Rdata")
} 


### Section 2
if (PARAM_START < 3) {
  rm(ODNB_tokenized1)

  if (PARAM_START > 1) {
    load("private_data/odnb_data_proc/ODNB_combtags.Rdata")
  }
  load("data/ODNB_intermediate/NER/ODNB_NERtokenized20140202_2.Rdata")
#|      ***********************************************************
#|----##Fix old directory structure --Sat Aug  9 20:41:24 2014--
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
  
  save(ODNB_combtags, file = "private_data/odnb_data_proc/ODNB_combtags.Rdata")
}

if (PARAM_START < 4) {
  ##Section 3
  
  if (PARAM_START > 2) {
    load("private_data/odnb_data_proc/ODNB_combtags.Rdata")
  }
  
  rm(ODNB_tokenized2)
  load("data/ODNB_intermediate/NER/ODNB_NERtokenized20140202_3.Rdata")
#|      ***********************************************************
#|----##Fix old directory structure --Sat Aug  9 20:41:24 2014--
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
  save(ODNB_combtags, file = "private_data/odnb_data_proc/ODNB_combtags.Rdata")
}
