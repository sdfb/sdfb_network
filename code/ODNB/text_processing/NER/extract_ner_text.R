##@S This file contains code that extracts the ner-processed text, and saves it into a .Rdata file
source("code/ODNB/ODNB_setup.R")

## Rawtext
txt_R = list()
for(j in 1:ZNSPLITS) {
  filename = paste("data/ODNB_intermediate/NER/compiled_raw/", j, ".txt", sep = "")
  txt_R[[j]] = readLines(filename)
  print(j)
}

## Lingpipe
txt_L = list()
for(j in 1:ZNSPLITS) {
  filename = paste("data/ODNB_intermediate/NER/proc_LING/", j, ".txt", sep = "")
  tmp = readLines(filename)
  tmp = gsub("&amp;", "&", tmp)
  tmp = gsub("&quot;", "\"", tmp)
  txt_L[[j]] = tmp[-length(tmp)] ## check if this is necessary?
  print(j)
}

## Stanford
txt_S = list()
for(j in 1:ZNSPLITS) {
  filename = paste("data/ODNB_intermediate/NER/proc_STAN/", "ST_", j, ".txt", sep = "")
  txt_S[[j]] = readLines(filename)
  print(j)
}

##checking for consistency -- if no errors print out, then yay!
print("Checking for consistency: ---")
clear_brak = function(x) { gsub("<.*?>", "", x) }

for(j in 1:ZNSPLITS) {
  if (j %% 10 == 0) { print(j) }
  lp = sapply(clear_brak(txt_L[[j]]), nchar, USE.NAMES = FALSE)
  st = sapply(clear_brak(txt_S[[j]]), nchar, USE.NAMES = FALSE)
  re = sapply(clear_brak(txt_R[[j]]), nchar, USE.NAMES = FALSE)

  if (any(re != st)) { cat("Error on stanford: doc", j, "\n") }
  if (any(re != lp)) { cat("Error on lingpipe: doc", j, "\n") }
}

save(txt_R, txt_L, txt_S, file = zzfile_textproc_ner_results)
