############
# This script takes the JSTOR bundle directory and a txt list of names to search for
# and produces a single file concatenating all files that match at least 1 of the names.
# It executes in parallel.
###########

# Input
JSTOR_PATH=/data/JSTOR/test
NAMES_FILE=./names_scott_version.csv

# Temp and Output
MATCHING_FILES=/data/JSTOR/matching_files.txt
UNIQUE_MATCHING_FILES=/data/JSTOR/uniq_matching_files.txt
CONTENT_OUTPUT=/data/JSTOR/content.txt

# First use parallel grep to find all files matching at least 1 of the names.
echo "grepping $NAMES_FILE at $JSTOR_PATH"
# Output the filename and matched regex name as: /path/to/filename.txt:regex_name
cat $NAMES_FILE | parallel -n4 --pipe grep -o -f - $JSTOR_PATH/* | tee $MATCHING_FILES

# Results are not unique, make them so
#echo "unique $MATCHING_FILES to $UNIQUE_MATCHING_FILES"
#cat $MATCHING_FILES | sort | uniq > $UNIQUE_MATCHING_FILES

# Now save contents of all matching files into single file
#echo catting
#cat $UNIQUE_MATCHING_FILES | xargs cat > $CONTENT_OUTPUT
