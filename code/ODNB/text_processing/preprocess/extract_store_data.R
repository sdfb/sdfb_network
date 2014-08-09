##@S This file contains code to create and store the ODNB.data file.

ODNB_raw = list()

for(i in 1:99999) {
  if (i %% 100 == 0) { print(i) }
  filename = paste("private_data/odnb_data_text/HTML/file_",i,".txt", sep = "")
  ODNB_raw[[i]] = readLines(filename)
}
save(ODNB_raw, file = "data/ODNB_raw/ODNB_rawHTML.Rdata")
