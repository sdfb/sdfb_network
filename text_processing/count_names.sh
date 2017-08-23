############
# This script takes the JSTOR bundle directory and a txt list of names to search for
# and produces a single file concatenating all files that match at least 1 of the names.
# It executes in parallel.
###########

# Input
DOCS_PATH=/pylon2/hm4s82p/walling/data/odnb_modern/splits
NAMES_FILE=/home/walling/sdfb_network/code/hpc/text_processing/name_lists/dataset5-test.csv

# Temp and Output
MATCHING_FILES=/pylon2/hm4s82p/walling/data/odnb_modern/matching_files_dataset5_test.txt

# First use parallel grep to find all files matching at least 1 of the names.
echo "grepping $NAMES_FILE at $DOCS_PATH"

# Output the filename and matched regex name as: /path/to/filename.txt:regex_name

# NOTE: Requires updated 'parallel' program (developed w/ version 20161222) 
source ~/software/sourceme.sh # Update PATH to use specific parallel version

ls $DOCS_PATH/* | parallel grep -o -F -H -f $NAMES_FILE | tee $MATCHING_FILES
