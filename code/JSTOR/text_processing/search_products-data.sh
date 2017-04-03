############
# This script takes a single file which is the concatenation of all the JSTOR documents into a single file
# along with a txt list of names to search for.
# It produces a single file with a list of all unique names found in the data
# It executes in parallel.
###########

# Input
DATA=/data/JSTOR/data.txt
NAMES_FILE=./names_scott_version.csv

# Temp and Output
MATCHING_NAMES=/data/JSTOR/matching_names.txt
UNIQUE_MATCHING_NAMES=/data/JSTOR/uniq_matching_names.txt

# Produce a grep output that contains 1 output for each match of each name in the data file
echo "counting"
cat $NAMES_FILE | parallel -n4 --pipe grep -o -f - $DATA | tee $MATCHING_NAMES

# Results are not unique, make them so
echo "unique $MATCHING_NAMES to $UNIQUE_MATCHING_NAMES"
cat $MATCHING_NAMES | sort | uniq > $UNIQUE_MATCHING_NAMES

