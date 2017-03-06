Summary
=======

Combines results of NER and the preprocessed data to create data structures usuable in network_estimation


Workflow
========

Step 1: 
  * Code: improve_combtags.R - 
  * Input: 
    * zzfile_textproc_ner_combtags -> 
    * zzfile_textproc_preproc_metadata ->
  * Output: zzfile_textproc_post_improvedpred -> data/ODNB_final/ODNB_improvedpred20141101.Rdata

Step 2: 
  * Code: create_entity_matrix.R - 
  * Input: zzfile_textproc_post_improvedpred -> data/ODNB_final/ODNB_improvedpred20141101.Rdata
  * Output: zzfile_textproc_post_entitymatrix = "data/ODNB_final/ODNB_entitymatrix20141101.Rdata"

Step 3:
  * Code: zz_fix_odnb_ids.R - 
  * Input: 
    * zzfile_curated_nodeset = "data_manual/dataset_Oct62014.csv" -> Manually created somewhere?
    * zzfile_textproc_post_entitymatrix = "data/ODNB_final/ODNB_entitymatrix20141101.Rdata"
    * zzfile_textproc_preproc_metadata = ""
  * Output: zzfile_curated_nodeset_update = "data_manual/ODNB_dataset.Rdata"

Step 4: 
  * Code: process_curated_nodes.R -
  * Input: 
    * zzfile_curated_nodeset_update = "data_manual/ODNB_dataset.Rdata"
    * zzfile_textproc_post_entitymatrix = "data/ODNB_final/ODNB_entitymatrix20141101.Rdata"
  * Output: None???

Step 5: 
  * Code: extract_data_for_model.R -
  * Input:
    * zzfile_base_entity_matrix = "data/ODNB_final/ODNB_subset_entity_matrix_20141102.Rdata"
    * zzfile_curated_nodeset_update = "data_manual/ODNB_dataset.Rdata"
  * Output: data/ODNB_newfinal/sampmatrix[j].Rdata -> sample matrix

Step 6:
  * Code: create_model_data.R
  * Input:
    * load("../../private_data/odnb_data_proc/ODNB_fullnamelist.Rdata")
    * load("../../private_data/odnb_data_proc/ODNB_entitymatrix.Rdata")
    * load("../../private_data/odnb_data_proc/ODNB_improvedpred.Rdata")
  * Temp:  Appears to create temporary/transient copies of the data throughout the processing.
  * Output:  Lots!!!

Notes
=====
This is where most of the remaining code fixing work needs to be done. 

create_model_data.R contains code that should create the resulting entity matrix, but it's not very clean. 

extract_metadata.R contains code that attempts to extract the metadata from the ODNB files, but it contains various manifestations of this. Needs cleanup. 