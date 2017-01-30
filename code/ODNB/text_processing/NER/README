Summary
======

Code for performing NamedEntityRecognition(NER) on the pre-processed ODNB data

Workflow
========

* Step 4: Run code through NER tools: 
  * Summary: Prepare data for NER processing, including copying versions into appropriate directories. Creat
  * Code: 
    * ner_scripts.R generates shell-scripts stanSCRIPT.sh and lingprecopySCRIPT.sh/lingpostcopySCRIPT that are used to run the corresponding NER tools on t he data. There is also a single command line code included in the text of the file. The pre/post-copy files simply move files into the corresponding directory inside the lingpipe files foldler (don't know what was messing up calling directories...)
    * Input: data/ODNB_intermediate/NER/compiled_raw/ -> generate bash calls for each input file here
    * Output: 
      * stanSCRIPT.sh -> Executes ner.sh for each file in input, output to /data/ODNB_intermediate/NER/proc_STAN/
      * lingprecopySCRIPT.sh -> moves each input to folder where Ling can find it
      * run ling -> See documentation -> line 29ish
      * lingposcopySCRIPT.sh -> moves output of ling to /data/ODNB_intermediate/NER/proc_LING/


* Step 5: Combine data extracted from NER tools. 
  * 5.a Load data from NER output
    * Summary: Loads NER results and cleans them up, removing special characters, etc..
    * Code: extract_ner_text.R       ## This file just obtains the text, and stores into regular .Rdata format
    * Input: 
      * data/ODNB_intermediate/NER/compiled_raw/ -> the rawdata input to the NER tools
      * data/ODNB_intermediate/NER/proc_LING/ -> LING output
      * data/ODNB_intermediate/NER/proc_STAN/ -> NER output
    * Output: data/ODNB_intermediate/NER/ODNB_NERproc20141029.Rdata
      * txt_R/L/S -> lists containg text contents of each of the raw, stan/ner, and ling outputs respectively

!!!!!!!!!! START HERE !!!!!!!!!!!!!

  * 5.b
    * Summary:  This file contains code to do processing. This results in a list of tokenized text vectors, a set for each document: The raw text, lingpipe tagged, stanford tagged texts, and they are all normalized (i.e. have same format) to the raw text. 
    * Code: extract_ner_results.R   
    * Input: data/ODNB_intermediate/NER/ODNB_NERproc20141029.Rdata
    * Output: data/ODNB_intermediate/NER/ODNB_NERtokenized20141029_[1,2,3].Rdata
      * ODNB_tokenized -> List of lists containg raw text and tags for each document

  * 5.c
    * Code: combine_ner_results.R This file contains code to compile the results, and this will result in a data frame for each document: tokenized text, named-entity tagging, approximate date estimate. 
    * Input: data/ODNB_intermediate/NER/ODNB_NERtokenized20141029_[1,2,3].Rdata
    * Output: data/ODNB_intermediate/NER/ODNB_combtags20141029.Rdata
      * ODNB_combtags -> data.frame combining the 3 input files

  * 5.d
    * Code: improve_ner_results.R This file contains code that compares the NER results to each other, as well as does minor adjustments that improves the recall/precision. 
    * Input: data/ODNB_intermediate/NER/ODNB_combtags20141029.Rdata
    * Output: code/private_data/odnb_data_proc/ODNB_fullnamelist.Rdata # Odd location???
      * full_result3 -> data.frame with 'improved' data -> Used in postprocessing