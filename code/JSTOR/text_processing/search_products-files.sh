############
# This script takes the JSTOR bundle directory and a txt list of names to search for
# and produces a single file concatenating all files that match at least 1 of the names.
# It executes in parallel.
###########

# Input
JSTOR_PATH=/data/JSTOR/test
NAMES_FILE=./NamesToSearch-6bacon4.11.17.csv

# Temp and Output
MATCHING_FILES=/data/JSTOR/matching_files.txt

# First use parallel grep to find all files matching at least 1 of the names.
echo "grepping $NAMES_FILE at $JSTOR_PATH"

# Output the filename and matched regex name as: /path/to/filename.txt:regex_name

#cat $NAMES_FILE | parallel -X -j 48 --block 1000 --pipe grep -o -f - $JSTOR_PATH/* | tee $MATCHING_FILES
#ls $JSTOR_PATH/* | parallel -X -j 48 --block 1000 --pipe grep -f $NAMES_FILE | tee $MATCHING_FILES
ls $JSTOR_PATH/* | parallel grep -o -F -H -f $NAMES_FILE | tee $MATCHING_FILES
