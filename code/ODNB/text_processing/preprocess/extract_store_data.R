##@S Read HTML files into a ODNB list

source("code/ODNB/ODNB_setup.R")

ODNB_rawHTML = list()

for(i in 1:99999) {
  if (i %% 100 == 0) { print(i) }
  filename = paste("data/ODNB_raw/HTML/file_",i,".txt", sep = "")
  ODNB_rawHTML[[i]] = readLines(filename)
}

save(ODNB_rawHTML, file = zzfile_textproc_preproc_rawHTML)
