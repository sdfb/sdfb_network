##@S This file contains code that extracts the ner-processed text, and saves it into a .Rdata file
Ncomp = 529

## Rawtext
txt_R = list()
for(j in 1:Ncomp) {
  filename = paste("private_data/odnb_data_text/COMPILED/", "comp", j, ".txt", sep = "")
  txt_R[[j]] = readLines(filename)
  print(j)
}

## Lingpipe
txt_L = list()
for(j in 1:Ncomp) {
  filename = paste("private_data/odnb_data_text/proc_LING/", "comp", j, ".txt", sep = "")
  tmp = readLines(filename)
  tmp = gsub("&amp;", "&", tmp)
  tmp = gsub("&quot;", "\"", tmp)
  txt_L[[j]] = tmp[-length(tmp)]
  print(j)
}

## Stanford
txt_S = list()
for(j in 1:Ncomp) {
  filename = paste("private_data/odnb_data_text/proc_STAN/", "ST_", j, ".txt", sep = "")
  txt_S[[j]] = readLines(filename)
  print(j)
}

##checking for consistency
print("Checking for consistency: ---")
clear_brak = function(x) { gsub("<.*?>", "", x) }

for(j in 1:Ncomp) {
  lp = sapply(clear_brak(txt_L[[j]]), nchar, USE.NAMES = FALSE)
  st = sapply(clear_brak(txt_S[[j]]), nchar, USE.NAMES = FALSE)
  re = sapply(clear_brak(txt_R[[j]]), nchar, USE.NAMES = FALSE)

  if (any(re != st)) { cat("Error on stanford: doc", j, "\n") }
  if (any(re != lp)) { cat("Error on lingpipe: doc", j, "\n") }
}


save(txt_R, txt_L, txt_S, file = "private_data/odnb_data_proc/ODNB_nerproc.Rdata")
