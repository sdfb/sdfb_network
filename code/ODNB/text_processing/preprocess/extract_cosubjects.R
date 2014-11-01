##@S This file contains code that extracts all the cosubjects and splits all cosubject documents according to a simple rule. 

## TODO: [Data Quality] Are there odnb articles beyond index 99999? I think so..., but then what's the limit? Don't want to download a large number of sparsely populated documents. Ignored for now, but may be worth a look if there are significant documents here. 

source("code/ODNB/ODNB_setup.R")

library(stringr)


## Load data
load(zzfile_textproc_preproc_rawHTML)


# Split cosubject biographies into appropriate sections -------------------
## Assumption: IF this is an article about a person with other entities added, take the new article as the start of the cosubject identifier until the end of the paragraph. If this is a group biography, do the split on each new person. Of course, this won't be perfect and may require modifications. 
print("----- Identifying documents that have cosubjects")
ind_cosub = sapply(ODNB_rawHTML, exists_cosubject)

print("----- Alternate method to identify cosubjects")
drop_NA = function(x) { return(x[!is.na(x)]) }
drop_large = function(x, large) { return(x[x < large]) }

alt_cosub = list() ## Stores all found ref:odnb/#s. 
for(j in seq_along(ODNB_rawHTML)) {
  alt_cosub[[j]] = as.numeric(gsub("ref:odnb/", "", c(str_extract_all(ODNB_rawHTML[[j]], "ref:odnb/[0-9]+"), recursive = TRUE)))
  if (j %% 500 == 0) { print(j) }
}

bio_ref = rep(NA, times = 99999) ## Stores index of main biography
for(j in seq_along(alt_cosub)) {
  if (is.na(bio_ref[j]) & (length(alt_cosub[[j]]) > 0)) { 
    bio_ref[j] = j 
    ids = alt_cosub[[j]]
    found_ids = which(bio_ref %in% drop_NA(c(ids, bio_ref[ids])))
    all_ids = drop_large(sort(unique(c(ids, found_ids, j))), large = 100000)
    if (length(all_ids) > 0) {
      z = sapply(all_ids, function(x) {length(unique(alt_cosub[[x]])) } )
      bio_ref[all_ids] = all_ids[which(z == max(z))]
    }
  }
  if (j %% 100 == 0) { print(j) }
}

bios_toprocess = sort(unique(bio_ref), na.last = NA)

print("----- Splitting cosubject biographies into new list of documents")
ODNB_text = vector("list", length = 99999)
ODNB_cosubstatus = rep("none", times = 99999)

for(j in seq_along(bios_toprocess)) {
  if (j %% 200 == 0) { cat(j, "") }
  main_id = bios_toprocess[j]
  all_ids = which(bio_ref == main_id)
  
  if (length(all_ids) == 1) {
    ODNB_text[[main_id]] = dnb_grab_main(ODNB_rawHTML[[main_id]])
    ODNB_cosubstatus[main_id] = "none"
  } else {
    cosub_result = process_cosubject(main_id = main_id, all_ids = all_ids)
    for(k in seq_along(cosub_result$ids)) {
      id = cosub_result$ids[k]
      ODNB_cosubstatus[id] = cosub_result$cosub_status[k]
      ODNB_text[[id]] = cosub_result$text[[k]]
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
