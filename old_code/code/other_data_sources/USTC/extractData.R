#@S Attempting to extract the network from the short title catalog. 
#@L For now, this just extracts the relevant data and stores it.

# search restricted to English, 1500-1600
# URL Examples (using search choice above)
urlp1 = "http://www.ustc.ac.uk/cicero/ustc_search.php?auth=author&author=&shorttitle=&printer=&imprint=&classification=&language=eng&country=&datelimit=4&date_min=1500&date_max=1700&numresults=100&Submit=+SEARCH+"
urlp2 = "http://www.ustc.ac.uk/cicero/ustc_search.php?auth=author&author=&shorttitle=&printer=&imprint=&classification=&language=eng&country=&datelimit=4&date_min=1500&date_max=1700&digital=&fulltext=&tpimage=&numresults=100&sort_by=&currentpage=2"
urlplast ="http://www.ustc.ac.uk/cicero/ustc_search.php?auth=author&author=&shorttitle=&printer=&imprint=&classification=&language=eng&country=&datelimit=4&date_min=1500&date_max=1700&digital=&fulltext=&tpimage=&numresults=100&sort_by=&currentpage=120"

# Access date 6/4/2013 4 AM
full_data = NULL
for(j in 1:120) {
  # Read file, collapse to single character string
  url = paste("http://www.ustc.ac.uk/cicero/ustc_search.php?auth=author&author=&shorttitle=&printer=&imprint=&classification=&language=eng&country=&datelimit=4&date_min=1500&date_max=1700&digital=&fulltext=&tpimage=&numresults=100&sort_by=&currentpage=", j, sep = "")
  text = readLines(url)
  fulltext = paste(text, collapse = " ")
  
  # Split by rows
  table_rows = strsplit(fulltext, split = "<tr[^>]*>")[[1]]
  table_rows = gsub("</tr>", "", table_rows)
  
  # Rows with pertinent data
  good_rows = grep("^<td width=\"20px\">", table_rows)
  
  # Split up each pertinent row, remove HTML <>'s
  row_split_list = strsplit(table_rows[good_rows], "</td>")
  row_split_list = sapply(row_split_list, function(x) {gsub("(&nbsp;|<.*?>)", " " , x)})
  # val 5 = author, 6 = title, 7 = year, 8 = publish location
  
  # Next row (contains Imprint), remove HTML
  nextrow_split_list = strsplit(table_rows[good_rows+1], "</td>")
  nextrow_split_list = sapply(nextrow_split_list, function(x) {gsub("(&nbsp;|<.*?>)", " " , x)})
  # entry 2 contains imprint info. 
  
  temp_data = data.frame(Author = row_split_list[5,],Title = row_split_list[6,],
                         Year = row_split_list[7,], PubLoc = row_split_list[8,],
                         Imprint = nextrow_split_list[2,], stringsAsFactors = FALSE)
  
  full_data = rbind(full_data, temp_data)
  print(j)
}

# Experiment with HTMLProcessing.R
source("general_help_functions/HTMLProcessing.R")

find_special_characters(full_data$PubLoc)
 # only existing special character is &#091; => "["
strip_spaces(head(full_data$Year))
strip_spaces(head(full_data$Imprint))

save(full_data, file = "existing_networks/USTC/full_uncleaned_Data.Rdata")

#################################################
## Secondary processing: 

load("existing_networks/USTC/full_uncleaned_Data.Rdata")
source("general_help_functions/HTMLProcessing.R")

head(full_data)
full_data$Year = strip_spaces(full_data$Year)
full_data$PubLoc = strip_spaces(gsub("&#091;", "[", full_data$PubLoc))
full_data$Imprint = strip_spaces(gsub("&#091;", "[", full_data$Imprint))
full_data$Author = strip_spaces(full_data$Author)

# remove Imprint:
full_data$Imprint = gsub("Imprint: ", "", full_data$Imprint)
full_data$Imprint

# Number of commas => cases: 
commas_per_line = sapply(full_data$Imprint, function(x) {length(gregexpr(",", x)[[1]])}, USE.NAMES = FALSE)
table(commas_per_line)
full_data$Imprint[commas_per_line == 1]
full_data$Imprint[grep("\\(", full_data$Imprint)]

# TODO: continue cleaning up extracted data (possibly in separate file), and organize into table

