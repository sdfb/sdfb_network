## To do package code processing (not necessary unless you edit the source files, and even then, it's not necessary as you can manually do most of it), you need packages metacode/stringr. These are freely available except 'metacode' which is a suite of functions that I've written to make my work easier...

library(codeProcessing)
library(stringr)
update_fx_documentation(FD = FilesDescription(dirlist = "code/SDFBFunctions/"))


## To build the package, roxygen2 helps generate the function documentation from comments left inside the sourcecode itself, so it needs to be written (I did not include these documentation files in the git repository since they are automatically generated, and can change...)
library(roxygen2)
roxygenise("code/SDFBFunctions/", clean = TRUE)
system("R CMD INSTALL code/SDFBFunctions")

## TODO: [Check] does this work? installing a subdirectory

