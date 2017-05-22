############
# This script takes the JSTOR bundle directory and a txt list of names to search for
# and produces a single file concatenating all files that match at least 1 of the names.
# It executes in parallel.
###########

# Input
JSTOR_PATH=/home/00157/walling/work/sixdegs/data/JSTOR/docs
NAMES_FILE=/home/00157/walling/work/sixdegs/jstor/text_processing/name_lists/NamesToSearch-6bacon4.11.17-cleansed.csv

# Temp and Output
MATCHING_FILES=/home/00157/walling/work/sixdegs/data/JSTOR/matching_files-jstor-split.txt

# First use parallel grep to find all files matching at least 1 of the names.
echo "grepping $NAMES_FILE at $JSTOR_PATH"

# Output the filename and matched regex name as: /path/to/filename.txt:regex_name

ls $JSTOR_PATH/* | parallel grep -o -F -H -f $NAMES_FILE | tee $MATCHING_FILES
