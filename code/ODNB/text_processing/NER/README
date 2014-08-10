Step 4: Run code through NER tools: 
-- Code: ner_scripts.R generates shell-scripts stanSCRIPT.sh and lingprecopySCRIPT.sh/lingpostcopySCRIPT that are used to run the corresponding NER tools on t he data. There is also a single command line code included in the text of the file. The pre/post-copy files simply move files into the corresponding directory inside the lingpipe files foldler (don't know what was messing up calling directories...)

Step 5: Combine data extracted from NER tools. 
-- Code extract_ner_text.R       ## This file just obtains the text, and stores into regular .Rdata format

-- Code extract_ner_results.R    ## This file contains code to do processing
                                 ## This results in a list of tokenized text vectors, a set for each document: The raw text, lingpipe tagged, stanford tagged texts, and they are all normalized to the raw text. 

-- Code combine_ner_results.R    ## This file contains code to compile the results, and this will result in a data frame for each document: tokenized text, named-entity tagging, approximate date estimate. 

-- Code improve_ner_results.R    ## This file contains code that compares the NER results to each other, as well as does minor adjustments that improves the recall/precision. 