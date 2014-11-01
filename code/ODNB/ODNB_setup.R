#@S This file runs setup for all the pertinent code files

## Load pertinent libraries

## Load SDFB library or helper files. 
use_library = TRUE

if (use_library) {
  library(SDFBFunctions)
} else {
  source("code/SDFBFunctions/R/clean_HTML_functions.R")
  source("code/SDFBFunctions/R/NER_processing.R")
  source("code/SDFBFunctions/R/NER_improve.R")
}

## Load the file location variables. 
source("code/ODNB/ODNB_file_locations.R")
rm(use_library)

