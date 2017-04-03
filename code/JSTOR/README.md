---
output: 
  html_document: 
    self_contained: no
---

# Overview

This directory contains the code to replicate the ODNC processing for the JSTOR dataset.

The end goal is to prepare data necessary to run the code in the network_estimation folder, but for this new dataset.  Just like ODNB, we first start with the rawdata set and perform a series of processing steps to create data matrices expected by the PGL model fitting.

The initial JSTOR dataset we will work on is the 'Early Journal Content' subset available here: http://dfr.jstor.org/??view=text&&helpview=about_ejc

The zip archive contains 451,666 XML files, each representing an 'article' from the collection.  A readme.txt file is include providing additional metadata about the file structures.  This data should be a bit easier to work with than the ODNB rawdata which is just HTML files which have a less consistent structure.

# Summary Stats

The first goal is to generate a count of how many files and times each unique name in uniq_names.txt appears in the JSTOR dataset.  See exploration/README for additional details

# Text Processing and Matric Construction

The next goal is to produce the matrix data structures similar to ODNB.  The Matrix X_i_j should contain the counts of each entity (j) in each document (i).  One primary difference is we will use a list of names we care about, instead of running a generaly Named Entity extraction algorithm.






