## This file contains a script that extracts the segments near the name mentions, in order to build a topic model. 

## of course, this contains only members with a SDFB_ID (otherwise they weren't directly noted in the dataset)

load("data/ODNB_final/ODNB_subset_entity_matrix_20141102.Rdata")
head(exact_df)

load("data/ODNB_final/ODNB_entitymatrix20141101.Rdata")

load("data_manual/ODNB_dataset.Rdata")

