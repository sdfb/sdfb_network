##@S This file contains code to extract the locations of NER text. 
source("code/ODNB/text_processing/helper/NER_processing.R")

## Treat the title person differently (use extract names function from the pure HTML, and ignore NER results here.
Ntexts = 199999
Ncomp = 529

RUN_TEXT_CHECK = FALSE ## This only needs to be true for the first time; this checks for HTML remnants that could have caused problems with NER.

## Load raw text data file
if (!any(ls() == "ODNB_cleantext")) {
  load("data/ODNB_intermediate/preNER/ODNB_splitcosub20140228.Rdata")
  load("data/ODNB_intermediate/NER/ODNB_NERproc20140202.Rdata")
#|      *****************************************************
#|----##Fix old directory structure --Sat Aug  9 20:31:47 2014--
}

## Text check, if needed

if (RUN_TEXT_CHECK) {
  ## Check if there are any HTML remnants in the code
  for(i in 1:Ntexts) {
    a = grep("<.*>", ODNB_cleantext[[i]])
    if (i %% 10000 == 0) { cat(i) }
    if ( length(a) > 0 ) {
      print(paste("Docu#",i," Index#",a, "\n"))
      b = gregexpr("<.*>", ODNB_cleantext[[i]])
      print(regmatches(ODNB_cleantext[[i]], b))
    }  
  }
  
  check.other.punct <- function(text) {
    b = gregexpr("&[^[:space:]]{1,10};", text)
    m = regmatches(text, b)
    return(m[sapply(m, length) > 0])
  }
  
  for(i in 1:Ntexts) {
    a = check.other.punct(ODNB_cleantext[[i]])
    if (i %% 10000 == 0) {cat(i)}
    if ( length(a) > 0 ) {
      print(paste("Docu#",i," Index#",a))
    }
  }
   
  ## Text is clean, now just need to merge them and extract identified name locations.
}
 

## The following needs to be done in three parts, due to memory issues... 
## Process combined documents into individual segments.
ODNB_tokenized1 = list()
for(j in 1:200) {
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
      ODNB_tokenized1[[docnums[k]]] = list()
      ODNB_tokenized1[[docnums[k]]]$raw = format_text_and_split(txt_R[[j]][main])
      ODNB_tokenized1[[docnums[k]]]$st = normalize_tagtext(
                                    txt_S[[j]][main],
                                    ODNB_tokenized1[[docnums[k]]]$raw,
                                    "ST")
      ODNB_tokenized1[[docnums[k]]]$lp = normalize_tagtext(
                                    txt_L[[j]][main],
                                    ODNB_tokenized1[[docnums[k]]]$raw,
                                    "LP")
                                   
    }
  } else {
    stop(paste("ERROR: Mismatching input lengths in compilation", j))
  }
}

save(ODNB_tokenized1, file = "data/ODNB_intermediate/NER/ODNB_NERtokenized20140202_1.Rdata")
#|                            ***********************************************************
#|----##Fix old directory structure --Sat Aug  9 20:41:24 2014--
rm(ODNB_tokenized1)

ODNB_tokenized2 = list()
for(j in 201:400) {
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
      ODNB_tokenized2[[docnums[k]]] = list()
      ODNB_tokenized2[[docnums[k]]]$raw = format_text_and_split(txt_R[[j]][main])
      ODNB_tokenized2[[docnums[k]]]$st = normalize_tagtext(
                                    txt_S[[j]][main],
                                    ODNB_tokenized2[[docnums[k]]]$raw,
                                    "ST")
      ODNB_tokenized2[[docnums[k]]]$lp = normalize_tagtext(
                                    txt_L[[j]][main],
                                    ODNB_tokenized2[[docnums[k]]]$raw,
                                    "LP")
                                   
    }
  } else {
    stop(paste("ERROR: Mismatching input lengths in compilation", j))
  }
}

save(ODNB_tokenized2, file = "data/ODNB_intermediate/NER/ODNB_NERtokenized20140202_2.Rdata")
#|                            ***********************************************************
#|----##Fix old directory structure --Sat Aug  9 20:41:24 2014--
rm(ODNB_tokenized2)

ODNB_tokenized3 = list()
for(j in 401:Ncomp) {
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
      ODNB_tokenized3[[docnums[k]]] = list()
      ODNB_tokenized3[[docnums[k]]]$raw = format_text_and_split(txt_R[[j]][main])
      ODNB_tokenized3[[docnums[k]]]$st = normalize_tagtext(
                                    txt_S[[j]][main],
                                    ODNB_tokenized3[[docnums[k]]]$raw,
                                    "ST")
      ODNB_tokenized3[[docnums[k]]]$lp = normalize_tagtext(
                                    txt_L[[j]][main],
                                    ODNB_tokenized3[[docnums[k]]]$raw,
                                    "LP")
                                   
    }
  } else {
    stop(paste("ERROR: Mismatching input lengths in compilation", j))
  }
}

save(ODNB_tokenized3, file = "data/ODNB_intermediate/NER/ODNB_NERtokenized20140202_3.Rdata")
#|                            ***********************************************************
#|----##Fix old directory structure --Sat Aug  9 20:41:24 2014--
rm(ODNB_tokenized3)







