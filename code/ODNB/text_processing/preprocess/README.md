Summary
======

Contains the code for converting raw ODNB data into data structures used in the NER processing

Workflow
========

* Step 1: Download ODNB Data
  * Run 'download_data.sh'
      * This creates a compressed file with the documents
  * 'decompress_data.sh' decompresses all the files
  * Summary: Downloads html files from http://www.oxforddnb.com/templates/article.jsp?articleid=
  * Output: data/ODNB_raw/HTML/*.html

* Step 2: Extract Data --> Store in .Rdata file
  * Summary: Simply readlines from each HTML file into an R list data structure
* Code: extract_store_data.R
  * Input: data/ODNB_raw/HTML/*.html
  * Output: data/ODNB_raw/ODNB_rawHTML_20141029.Rdata
    * ODNB_rawHTML -> an R list of each html file contents

* Step 3: Split Data along co-subjects
  * Summary: Cleans up the text and extracts co-subject information into its own data structure
  * Code: extract_cosubjects.R
  * Input: data/ODNB_raw/ODNB_rawHTML_20141029.Rdata
  * Intermediate: data/ODNB_intermediate/NER/compiled_raw/*.txt
  * Output: data/ODNB_intermediate/preNER/ODNB_splitcosub20141029.Rdata
    * ODNB_text -> an R list of the 'biographies'
    * ODNB_cleantext-> cleaner version of ODNB_text
    * ODNB_cosubstatus -> char vector of cosubjects, only 13/99,999 != "none"

* Step 4: Extract metadata
  * Summary: Uses regular expressions and other techniques to parse out entities such as occupations and names
  * Code: extract_metadata.R
  * Input: data/ODNB_intermediate/preNER/ODNB_splitcosub20141029.Rdata
  * Output: data/ODNB_raw/ODNB_metadata20141101.Rdata
    * full_metadata -> a data.frame with info on each 'entity' such as dates associated, etc..