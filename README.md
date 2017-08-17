# Overview

This directory contains refactored code that attempts to replicate the ODNC processing for new datasets.  Its goal is to make the code for flexible as well as parallelize sections of the code such that is can take advantage of all available resources on a given system.  It was specifically developed using resources available through XSEDE, including Wrangler and Bridges.

JSTOR

The refactored code was developed first using the JSTOR early journal dataset.  The end goal was to prepare data necessary to run the code in the network_estimation folder, but for this new dataset.  Just like ODNB, we first start with the rawdata set and perform a series of processing steps to create data matrices expected by the PGL model fitting.  However, here we skip the step of performing named entity recognition on the raw documents, instead we assume a list of name of interest are already available.

The JSTOR dataset 'Early Journal Content' subset available here: [https://archive.org/details/jstor_ejc&tab=about]

The zip archive contains 451,666 XML files, each representing an 'article' from the collection.  A readme.txt file is include providing additional metadata about the file structures.  This data should be a bit easier to work with than the ODNB rawdata which is just HTML files which have a less consistent structure.

# Code Structure

The code is split into the following sections.  It consists of a combination of parallized bash and R scripts.  Temporary files are created through the process to pass data from one component to another.

## text_processing

This folder contains code for taking any collection of text based documents, searching through those documents given a list of named entities of interest, and generating document count matrices for each document/name combination.  We first split large documents into subdocuments with a minimum length of 500 words.  We perform a parallelized count through those documents for each name in our list, and then transform those counts into a Sparse R matrix.  The columns of this matrix represent the names of interest, the rows the documents, and the cells the counts for a given person in a given document.  This matrix is what is used by the network_estimation code.

## network_estimation

Given our document count matrix, we perform Poisson Graphical Lasso modeling once for each person using GLMNET, treating their column of counts as the reponse variable, the other columns as the predictors.  Potential relationships for a person A are considered as those where there is a positive non-zero coefficient associated with another person B.  If a relationship was found between A and B and/or between B and A, we consider this as a single undirected relationship.

We repeat the above process to form B 'bootstrap' style samples of the results, using a randomized subset of the documents (50%) for each run.  Finally, the samples are combined, and a estimate of the network is created where the weight of the edge between any 2 nodes = the number of times that relationship was detected in the B iterations.

# Overall Workflow

The workflow from rawdata to end results is as follows.

1. Create a folder which contains all raw documents.
2. Split documents > 999 words into subdocuments and store in a seperate folder. (text_processing/split_documents.R)
3. Given a name_list, perform a word count for each name on each document, producing a document:name mapping. (text_processing/count_names.sh)
4. Transform the results of 3 into a Sparse matrix. (text_processing/generate_document_matrix.R)
5. Perform the network estimations. (network_estimation/PGL_fit.R)
6. Extract the resulting network from the results. (network_estimation/network_graph.R)
4. Export network to additional formats such as .gefx. (network_estimation/export_graph.R)

Generally, each R script loads a file as input and saves a file as output, which controlls which datasets are processed.  Most can be run simply with $ Rscript file.R

# Additional Details

Details for each file can be found in the sub-foldersing 'text_processing' and 'network_estimation'.








