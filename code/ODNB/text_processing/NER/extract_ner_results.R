##@S This file contains code to extract the locations of NER text. 

## Treat the title person differently (use extract names function from the pure HTML, and ignore NER results here.
Ntexts = 199999
Ncomp = 529

RUN_TEXT_CHECK = FALSE ## This only needs to be true for the first time; this checks for HTML remnants that could have caused problems with NER.

## Load raw text data file
if (!any(ls() == "ODNB_cleantext")) {
  load("data/ODNB_intermediate/preNER/ODNB_splitcosub20140228.Rdata")
  load("private_data/odnb_data_proc/ODNB_nerproc.Rdata")
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
 
## Split truedocs into words

format_text_and_split = function(text) {
  ## This function does the following:
  ## 1. add spaces before punctuation
  ## 2. add spaces before/after (), [], {}, as to separate them off => easier to count
  ## 3. remove more than one space in a row.
  ## 4. Tokenize (split into a word-by-word representation).
  ## 4a. Fix problems with ;. 

  ## text should be a character vector.
  
  ## Collapse text
  temp = paste(text, collapse = " ")


  ## Add spaces before/after punctuation if needed.
  puncts = c("[.]", ",", ":", ";", "!", "[?]", "`")

  ## add spaces before/after; will remove extra ones later
  puncts = c(".", ",", ":", "!", "?", "`", ";", "(", ")", "{", "}", "[", "]")
  for(t in puncts) {
    temp = gsub(t, paste(" ",t," ", sep = ""), temp, fixed = TRUE)
  }

  ## Remove multiple spaces
  temp = gsub(" +", " ", temp)

  ## Split text on spaces
  text_split = strsplit(temp, split = " ")[[1]]

  ## Fix semicolon problems
  semics = which(text_split == ";")
  semics = semics[semics > 1] ## Cannot be problem as first item

  semic_probs = grep("^&",text_split[semics - 1])
  if (length(semic_probs) > 0) {
    print("Fixing semicolon problems at:")
    print(semics[semic_probs])
    
    ind = semics[semic_probs] - 1
    text_split[ind] = paste(text_split[ind], ";", sep = "")
    text_split = text_split[-semics[semic_probs]]
  }
  
  ## return results
  return(text_split)
}

paste_two <- function(vec,ind.start) {
  # this pastes together two consecutive entries in a vector
  #  starting with ind.start and then ind.start+1
  #  and edits the vector to remove the extra cell
  vec[ind.start] = paste(vec[ind.start], vec[ind.start+1], sep = "")
  return(vec[-(ind.start+1)])
}

normalize_tagtext <- function(text, true.text, type) { 
  ## This vectorizes, normalizes tagged text
  ## type = "ST" or "LP"
  
  #   text = gsub("£", "?", text) ## pound symbol... convert to ?
  #   text = gsub("&amp;", "&", text)
  
  if (type == "ST") {    
    # do nothing
  } else if (type == "LP") {
    ## Single-words ENAMEX, fixes ' as organization issue. 
    text = gsub("<ENAMEX TYPE=\"ORGANIZATION\">'</ENAMEX>","\'", text)
    text = gsub("ENAMEX TYPE=", "ENAMEXTYPE=", text)
    text = gsub("&amp;", "&", text)
    text = gsub("&quot;", "\"", text)
    text = gsub("</*?s.*?>", "", text)
  } else {
    print("Error in (fix.tagtext): Invalid TYPE")
    return("Error in (fix.tagtext): Invalid TYPE")
  }
  
  words = format_text_and_split(text)
  
  to.test = gsub("<.*?>", "", words) # removing all <>'s , comparing spacing
  j = 1
  N = length(true.text)
  while(j <= N) {
    if (true.text[j] == to.test[j]) {
      j = j+1
    } else {
      if (length(grep(paste(to.test[j],to.test[j+1], sep = ""),
                      true.text[j], fixed = TRUE)) > 0) {
        to.test = paste_two(to.test, j)
        words = paste_two(words, j)
      } else {
        print(paste("ERROR... temp: j=",j))
        return("ERROR.")
      }
    }
  }
  while(length(words) > N) {
    words = paste_two(words, N)
  }
  
  return(words)
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

save(ODNB_tokenized1, file = "private_data/odnb_data_proc/ODNB_ner_tokenized1.Rdata")
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

save(ODNB_tokenized2, file = "private_data/odnb_data_proc/ODNB_ner_tokenized2.Rdata")
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

save(ODNB_tokenized3, file = "private_data/odnb_data_proc/ODNB_ner_tokenized3.Rdata")
rm(ODNB_tokenized3)







