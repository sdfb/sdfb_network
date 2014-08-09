##@S This file contains code that generates scripts for running the NER programs.

## Script for stanford NER tool processing
N = 529 # number of split-files
script = rep("", times = N+3)
script[1] = "#!/bin/sh"
script[2] = "scriptdir=`dirname $0`"
script[3] = ""

for(i in 1:N) {
  temp = paste("sh ../../software/stanNER/ner.sh ../../private_data/odnb_data_text/COMPILED/comp",i,".txt > ../../private_data/odnb_data_text/proc_STAN/ST_",i,".txt", sep = "")
  script[i+3] = temp
}

writeLines(script, con = "text_mining/ODNB/stanSCRIPT.sh")

## Create folders 'proc' and 'unproc' in lingpipe/demos/generic/bin/.
## Run the following script to copy files over: 

## Script for copying text files over to lingpipe directory
script = rep("", times = 3)
script[1] = "#!/bin/sh"
script[2] = ""
script[3] = "cp ../../private_data/odnb_data_text/COMPILED/* ../../software/lingpipe/demos/generic/bin/unproc"
writeLines(script, con = "text_mining/ODNB/lingprecopySCRIPT.sh")


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
script[3] = "cp ../../software/lingpipe/demos/generic/bin/proc/* ../../private_data/odnb_data_text/proc_LING/"
script[4] = "rm ../../software/lingpipe/demos/generic/bin/proc/*"
script[5] = "rm ../../software/lingpipe/demos/generic/bin/unproc/*"

writeLines(script, con = "text_mining/ODNB/lingpostcopySCRIPT.sh")




