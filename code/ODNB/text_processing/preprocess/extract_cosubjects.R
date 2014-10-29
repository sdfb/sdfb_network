##@S This file contains code that extracts all the cosubjects and splits all cosubject documents according to a simple rule. 

## TODO: [Data Quality] Are there odnb articles beyond index 99999? I think so..., but then what's the limit? Don't want to download a large number of sparsely populated documents. 


source("code/ODNB/ODNB_setup.R")

## Load data
load(zzfile_textproc_preproc_rawHTML)


## Step 1: Process cosubject biographies, split these into separate bios.
# Assumption: If this is an article about a person, with other entities added, then take the new article as only until the end of that paragraph. If this is a group biography, then split on each new person. Of course this won't be perfect...

## figures out which documents have cosubjects
ind_cosub = sapply(ODNB_rawHTML, exists_cosubject)

## creates a list, each entry is a vector of character lengths of segments. This will be used to compare documents to identify identical documents. 
print("----- Counting character lengths of documents")
nchar_list = list()

for(j in 1:99999) {
  if (!is_nobio(ODNB_rawHTML[[j]])) {
    nchar_list[[j]] = nchar(dnb_grab_main(ODNB_rawHTML[[j]]))
  } else {
    nchar_list[[j]] = 0
  }
  if (j %% 1000 == 0) { cat(j, " ") }
}
nchar_lengths = sapply(nchar_list, length)

## creates a list, where each entry denotes all the documents referring to the same biography (and in one case, the 'blank' biography [server error])
print("----- Aggregating identical documents")
cosub_list = list()
ind = 1
new_indicator = rep(TRUE, times = 99999)
for(j in 1:99999) {
  if (new_indicator[j]) {
    base = nchar_list[[j]]
    cosub_list[[ind]] = j
    
    for(k in which(length(base) == nchar_lengths)) {      
      if (all(base == nchar_list[[k]]) & (k != j)) {
        cosub_list[[ind]] = c(cosub_list[[ind]], k)
      }
    }
    ind = ind + 1
    new_indicator[cosub_list[[ind - 1]]] = FALSE
    cat(j, " ")
  }
}

## if name contains drop_names, then split before each name. Otherwise, split until start of new paragraph or another cosubject.
print("----- Creating new list of documents")
ODNB_text = vector("list", length = 199999)
ODNB_groupcosub = rep(FALSE, times = 199999)

for(j in 1:length(cosub_list)) {
  if (j %% 1000 == 0) { cat(j, " ") }
  
  if (length(cosub_list[[j]]) > 100) {
    ## do nothing; this ignores all the error pages
  } else if (length(cosub_list[[j]]) > 1) {
    ## test for common name
    
    cos_res = process_cosubject(ids = cosub_list[[j]])
    ODNB_groupcosub[cosub_list[[j]][1]] = cos_res$group
    
    for(k in seq_along(cos_res$text)) {
      if (!is.null(ODNB_text[[cos_res$ids[k]]])) {
        ## For whatever reason, some people share ID nums (cosubjects). I'm adding 100,000 to the id number for these... 
        ODNB_text[[cos_res$ids[k] + 100000]] <- cos_res$text[[k]]
      } else {
        ODNB_text[[cos_res$ids[k]]] <- cos_res$text[[k]]
      }
    }
    
  } else {
    temp = dnb_grab_main(ODNB_rawHTML[[cosub_list[[j]][1]]])
    ODNB_text[[j]] = temp[-length(temp)]
  }
}


## Strange text problems:
ODNB_text[[44722]] = c(paste(ODNB_text[[44722]][1:4], collapse = " "), ODNB_text[[44722]][-1:-4])
ODNB_text[[48216]] = c(paste(ODNB_text[[48216]][1:4], collapse = " "), ODNB_text[[48216]][-1:-3])



## Removing accents/HTML
print("Cleaning text")

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

save(ODNB_text, ODNB_cleantext, ODNB_groupcosub, file = zzfile_textproc_preproc_splitcosub)
