############
# This script takes the JSTOR bundle directory and a txt list of names to search for
# and produces a single file concatenating all files that match at least 1 of the names.
# It executes in parallel.
###########

# Input
DOCS_PATH=/home/00157/walling/work/sixdegs/data/ODNB/odnb_modern/splits
NAMES_FILE=/home/00157/walling/work/sixdegs/jstor/text_processing/name_lists/dataset5.csv

# Temp and Output
MATCHING_FILES=/home/00157/walling/work/sixdegs/data/ODNB/matching_files-odnb_modern-splits-dataset5.txt

# First use parallel grep to find all files matching at least 1 of the names.
echo "grepping $NAMES_FILE at $DOCS_PATH"

# Output the filename and matched regex name as: /path/to/filename.txt:regex_name

ls $DOCS_PATH/* | parallel grep -o -F -H -f $NAMES_FILE | tee $MATCHING_FILES
