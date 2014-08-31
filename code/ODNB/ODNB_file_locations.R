#@S This file stores directory locations for all the ODNB processing files. 


## A function to remove filename variables (clean up the workspace...)
REMOVE_FILENAME_VARIABLES = function() {
  rm(list=ls()[grep("zzfile_", ls())])
  return("[Filename Variables Removed!]")
}


## Stores raw HTML files
zzfile_textproc_preproc_rawHTML = "data/ODNB_raw/ODNB_rawHTML_20131107.Rdata"

## Stores cosubject splitting information
zzfile_textproc_preproc_splitcosub = "data/ODNB_intermediate/preNER/ODNB_splitcosub20140228.Rdata"






