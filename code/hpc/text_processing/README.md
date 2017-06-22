# Overview

Contains code for processing the raw txt documents and creating a document count matrix

### count_names.sh

A bash script that takes a name list and folder containing documents and performs a parallized count of each name in each document.  The results are saved as a text file.

### count_names.slurm

A slurm schduler submission template for running counts_names.sh in batch mode on an hpc system.

### CountsSummary.Rmd

A Rmarkdown document for summarize the results of count_name.sh

### generate_document_matrix.R

Takes the uniq_matching_files list and creates a document matrix where each row is a document, each column a name and provides the count of a given name in a given document.

### name_lists

For with example names lists used for generating document:name mappings with counts_names.sh.

### split_documents.R

Code for splitting an text document into a folder > 999 words into sub documents with a minimum size of 500 words.




