---
output: 
  html_document: 
    self_contained: no
---

# Overview

Contains code for processing the raw JSTOR data into a document count matrix

# names_scott_version.cvs

The master list of names we want to search for.  Essentially includes all names detected in the ODNB processing.  We are skipping the NER steps for the JSTOR dataset, using this file and grep instead.

# search_products-data.sh

Takes a single file with all the JSTOR documents concatted together to produce list of all unique names from an input list found in any of the documents.

# search_products-files.sh

Takes the names input list and searches for all files containing least one hit for any name, saving the list of matching files.  Used to filter out documents that are not of interest.

# generate_document_matrix.R

Takes the uniq_matching_files list and creates a document matrix where each row is a document, each column a name and provides the count of a given name in a given document.