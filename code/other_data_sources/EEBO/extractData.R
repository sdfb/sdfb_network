#@S Attempting to extract the network from EEBO. 
#@S This just extracts the relevant data and stores it.

# search restricted to English, 1500-1700
# URL Examples (using search choice above)
# urlp1 = "http://eebo.chadwyck.com/search?action=GOTO&SOMQUERY=1&SIZE=40&RETRIEVEFROM=1&FILE=../session/1371459563_6262&SOURCE=var_spell.cfg&SOURCE=var_spell.cfg&DISPLAY=AUTHOR&SEARCHSCREEN=CITATIONS&ECCO=N"
# urlp2 = "http://eebo.chadwyck.com/search?action=GOTO&SOMQUERY=1&SIZE=40&RETRIEVEFROM=41&FILE=../session/1371459563_6262&SOURCE=var_spell.cfg&SOURCE=var_spell.cfg&DISPLAY=AUTHOR&SEARCHSCREEN=CITATIONS&ECCO=N"
# urlplast = "http://eebo.chadwyck.com/search?action=GOTO&SOMQUERY=1&SIZE=40&RETRIEVEFROM=124241&FILE=../session/1371459563_6262&SOURCE=var_spell.cfg&SOURCE=var_spell.cfg&DISPLAY=AUTHOR&SEARCHSCREEN=CITATIONS&ECCO=N"
# Note that the /session/... changes, so old links won't work anymore

# NOTE: This data requires CMU VPN... so whether the raw data should be *shared* is questionable

##### Download all the raw data, store it. (since this takes up most of the time)
# Access Date 6/26/2013 8 AM
# 124241 records => 3107 pages

raw_web_data = list()
for (j in 1:3107) {
  url = paste("http://eebo.chadwyck.com/search?action=GOTO&SOMQUERY=1&SIZE=40&RETRIEVEFROM=", 
              ((j-1)*40 + 1),
              "&FILE=../session/1372235847_4940&SOURCE=var_spell.cfg&SOURCE=var_spell.cfg&DISPLAY=AUTHOR&SEARCHSCREEN=CITATIONS&ECCO=N",
              sep = "")
  text = readLines(url)
  raw_web_data[[j]] = text
  print(j)
}

save(raw_web_data, file = "existing_networks/EEBO/rawData.Rdata")
load("existing_networks/EEBO/rawData.Rdata")

##### Extract pertinent sections, store in data frame
full_data = NULL
for(j in 1:3107) { # to 3107
  # Read file, collapse to single character string
  text = raw_web_data[[j]]
  
  auth_rows = grep("<SCRIPT>auth_name=", text)
  date_rows = grep("<BR><SPAN CLASS=\"boldtext\">Date:</SPAN>", text)
  title_rows = grep("<TD VALIGN=\"TOP\" ALIGN=\"left\"><I>", text)
  imprint_rows = grep("<SCRIPT><!-- (Remove EEBO interactions - March2013) write_wiki_link(checkeeboid,'N')--></SCRIPT>", 
                      text, fixed = TRUE)
    
  t_auth = text[auth_rows]
  t_auth = gsub("<SCRIPT>auth_name='X", "", t_auth)
  t_auth = gsub("';</SCRIPT>", "", t_auth)
  
  t_title = gsub("<.*?>", "", text[title_rows])
  
  t_date = gsub("<.+>", "", text[date_rows])
  
  t_imprint = gsub("<SCRIPT>.*</SCRIPT>", "", text[imprint_rows])
  
  match_ID = 1:length(t_imprint)
  if (length(t_imprint) < length(t_title)) {
    match_ID = sapply(imprint_rows, function(x){
      temp = intersect(which(x > title_rows), which(x < c(title_rows[-1], length(text))))
      return(temp)
      })
  }
  temp_data = data.frame(Author = t_auth,Title = t_title,
                         Year = t_date, 
                         Imprint = rep("", times = length(t_title)), stringsAsFactors = FALSE)
  temp_data$Imprint[match_ID] = t_imprint
  
  full_data = rbind(full_data, temp_data)
  cat(j, ":", date(), "\n")
}
save(full_data, file = "existing_networks/EEBO/subsetData.Rdata")
# full_data is a data frame with 120k rows, with columns: 
#   Author, Title, Year, Imprint

