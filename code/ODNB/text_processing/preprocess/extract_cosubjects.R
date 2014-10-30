##@S This file contains code that extracts all the cosubjects and splits all cosubject documents according to a simple rule. 

## TODO: [Data Quality] Are there odnb articles beyond index 99999? I think so..., but then what's the limit? Don't want to download a large number of sparsely populated documents. Ignored for now, but may be worth a look if there are significant documents here. 

source("code/ODNB/ODNB_setup.R")

## Load data
load(zzfile_textproc_preproc_rawHTML)


# Split cosubject biographies into appropriate sections -------------------
## Assumption: IF this is an article about a person with other entities added, take the new article as the start of the cosubject identifier until the end of the paragraph. If this is a group biography, do the split on each new person. Of course, this won't be perfect and may require modifications. 

print("----- Identifying documents that have cosubjects")
ind_cosub = sapply(ODNB_rawHTML, exists_cosubject)


print("----- Counting character lengths of all documents")
nchar_list = lapply(1:99999, function(x) {
  if (!is_nobio(ODNB_rawHTML[[x]])) { return(nchar(dnb_grab_main(ODNB_rawHTML[[x]]))) } else { return(0) }
})


print("----- Count number of lines in each document")
nchar_lengths = sapply(nchar_list, length)


print("----- Identifying identical documents and cosubject candidates")
## Find documents that have the same number of lines
length_doc_list = lapply(1:max(nchar_lengths), function(x) {which(x == nchar_lengths)})

cosub_list = list() ## list of indices which all refer to the same document. 
ind = 1 ## indexes progress
new_indicator = rep(TRUE, times = 99999) ## If TRUE -> still need to process this document. 
for(j in 1:99999) {
  if (j %% 100 == 0) { cat(j, " ") }
  
  ## For each document, if it hasn't already been identified as identical, then process. 
  if (new_indicator[j] && any(nchar_list[[j]] > 0)) {
    base = nchar_list[[j]]
    maindnb = dnb_grab_main(ODNB_rawHTML[[j]])
    
    cosub_list[[ind]] = j
    for(k in length_doc_list[[nchar_lengths[j]]]) {
      ## Check for nonequal index, then nonequal character numbers, then finally for nonequal exact text. 
      if ((k != j) && all(base == nchar_list[[k]]) && all(maindnb == dnb_grab_main(ODNB_rawHTML[[k]]))) {
        cosub_list[[ind]] = c(cosub_list[[ind]], k)
      }
    }
    new_indicator[cosub_list[[ind]]] = FALSE
    
    ind = ind + 1
  }
}
## Cleanup
rm(ind, new_indicator, base, maindnb, j, k)


print("----- Splitting cosubject biographies into new list of documents")
ODNB_text = vector("list", length = 199999)
ODNB_groupcosub = rep(FALSE, times = 199999)

for(j in 1:length(cosub_list)) {
  if (j %% 1000 == 0) { cat(j, " ") }
  
  if (length(cosub_list[[j]]) == 1) {
    ODNB_text[[cosub_list[[j]][1]]] = dnb_grab_main(ODNB_rawHTML[[cosub_list[[j]][1]]])
  } else {
    ## Process cosubject determines if name is a 'group' biography; if so, splits are at every name. 
    ## Otherwise, splits are for single paragraphs for smaller cosubjects. 
    cosub_result = process_cosubject(ids_input = cosub_list[[j]])
    ODNB_groupcosub[cosub_list[[j]][1]] = cosub_result$group
    for(k in seq_along(cosub_result$ids)) {
      if (!is.null(ODNB_text[[cosub_result$ids[k]]])) { 
        ## For whatever reason, some people share ID nums (cosubjects). I'm adding 100,000 to the id number for these... 
        if (!is.null(ODNB_text[[cosub_result$ids[k] + 100000]])) { stop("Error at ", j) }
        ODNB_text[[cosub_result$ids[k] + 100000]] = cosub_result$text[[k]]
      } else {
        ODNB_text[[cosub_result$ids[k]]] = cosub_result$text[[k]]
      }
    }
  }
}


## Strange text problems:
# ODNB_text[[62274]] = c(paste(ODNB_text[[62274]][1:3], collapse = " "), ODNB_text[[62274]][-1:-3])
# ODNB_text[[69990]] = c(paste(ODNB_text[[69990]][1:2], collapse = " "), ODNB_text[[69990]][-1:-2])


## Removing accents/HTML
print("----- Cleaning text")

ODNB_cleanup = function(x) {
  ## This removes all remaining <>'s.
  res = gsub("<.*?>", "", x)
  return(res)
}

ODNB_cleantext = vector("list", length = 199999)
for(i in 1:199999) {
  if (i %% 50 == 0) { cat(i, " ") }
  if (!is.null(ODNB_text[[i]]) & (length(ODNB_text[[i]]) > 0)) {
    tmp = ODNB_fix_accent_html(ODNB_text[[i]])
    ODNB_cleantext[[i]] = ODNB_cleanup(tmp)
  }
}

## RAN TO HERE ------------------------------

## Step 2: Save the text documents into files with 100 documents each

## TODO: NEED TO ADD HTML REMOVAL (and accents and stuff)
print("----- Save all these text documents into files")


count = 1
for(j in 1:199999) {
  if (count %% 100 == 1) {
    cat(count, " ")
    cur_file = paste("data/ODNB_intermediate/NER/compiled_raw/", round(count/100) + 1, ".txt", sep = "")
  } 
  if (!is.null(ODNB_cleantext[[j]])) {
    cat(c("@@@@@",
          "", "",  paste("doc=",j), "", "",
          ODNB_cleantext[[j]],
          "", ""), sep = "\n", file = cur_file, append = TRUE)
    count = count + 1
  }
}


print("----- Run text check")
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

save(ODNB_text, ODNB_cleantext, ODNB_groupcosub, file = zzfile_textproc_preproc_splitcosub)
