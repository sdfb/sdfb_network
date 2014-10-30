##@S This file contains code that generates scripts for running the NER programs.
source("code/ODNB/ODNB_setup.R")

## Script for stanford NER tool processing
script = rep("", times = ZNSPLITS+3)
script[1] = "#!/bin/sh"
script[2] = "scriptdir=`dirname $0`"
script[3] = ""

for(i in 1:ZNSPLITS) {
  temp = paste("sh ../../../../software/stanNER/ner.sh ../../../../data/ODNB_intermediate/NER/compiled_raw/",i,".txt > ../../../../data/ODNB_intermediate/NER/proc_STAN/ST_",i,".txt", sep = "")
  
  script[i+3] = temp
}

writeLines(script, con = "code/ODNB/text_processing/NER/stanSCRIPT.sh")

## Create folders 'proc' and 'unproc' in lingpipe/demos/generic/bin/.
## Run the following script to copy files over: 

## Script for copying text files over to lingpipe directory
script = rep("", times = 3)
script[1] = "#!/bin/sh"
script[2] = ""
script[3] = "cp ../../../../data/ODNB_intermediate/NER/compiled_raw/* ../../../../software/lingpipe/demos/generic/bin/unproc"
writeLines(script, con = "code/ODNB/text_processing/NER/lingprecopySCRIPT.sh")


## Then, run the following line
## --- code ---
## nice nohup sh cmd_ne_en_news_muc6.sh -inDir=unproc -outDir=proc
## --- code ---
## This should be run from software/lingpipe/demos/generic/bin

## Finally, run this line to fix everything. 
## Script for copying processed files back, and cleans up other directories. 
script = rep("", times = 5)
script[1] = "#!/bin/sh"
script[2] = ""
script[3] = "cp ../../../../software/lingpipe/demos/generic/bin/proc/* ../../../../data/ODNB_intermediate/NER/proc_LING/"
script[4] = "rm ../../../../software/lingpipe/demos/generic/bin/proc/*"
script[5] = "rm ../../../../software/lingpipe/demos/generic/bin/unproc/*"

writeLines(script, con = "code/ODNB/text_processing/NER/lingpostcopySCRIPT.sh")




