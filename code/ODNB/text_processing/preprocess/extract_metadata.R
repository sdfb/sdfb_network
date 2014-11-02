##@S This file contains code to extract various fields from raw html text

## Load help functions
source("code/ODNB/ODNB_setup.R")


# Other helper functions --------------------------------------------------

ODNB_parse_date = function(bio, end) {
  ## Takes bio, and integer for last position in name segment, and looks for next match.
  if (is.null(bio)) {return(NA)}
  bio = paste(bio[[1]], sep = " ")
  segments = gregexpr("\\(.*?[0-9]+.*?\\)", bio)
  match = regmatches(x = bio, m = segments)[[1]]
  
  if (length(match) > 0) { return(match[1]) } else { return(NA) }
  
  ## TODO: Old code; remove this. 
  # match = regmatches(bio, segments)[[1]]
#   
#   j = min(which(segments[[1]] >= end))
#   if (length(match) > 0 & (!is.na(j)) & (j > 0)) {
#     return(match[j])
#   } else {
#     return(NA)
#   }
}


## Load Data
load(zzfile_textproc_preproc_splitcosub)

## Extract names
ext_names = lapply(ODNB_text, ODNB_extract_name_segment)
sapply(ODNB_text, function(x) { !is.null(x) }) -> temp
sum(temp)
good_names = which(temp)

## Extract dates
mapply(ODNB_parse_date, bio = ODNB_text[good_names],
       end = sapply(ext_names[good_names], function(x) {x$end})) -> temp
temp_dates = gsub("&#150;", "-", temp)
bad_dates = which(nchar(temp_dates) == 2)
length(bad_dates)
tocheck = good_names[sample(bad_dates, 3)]
ODNB_text[tocheck]

## Extract occupations
find_occ = function(inp) {
  x = inp[[1]]
  regm = gregexpr("<span class=\"occ\">.*?</span>", x)
  res = regmatches(x, regm)
  res = gsub("<.*?>", "", res)
  return(res)
}
occs = sapply(ODNB_text[good_names], find_occ)

bio_len = sapply(ODNB_cleantext, function(x) {
  if (length(x) == 0) {return(0) }
  z = strsplit(x, split = " ")
  return(sum(sapply(z, length)))
})

fns = sapply(ext_names, function(x) {
  x$text -> res
  gsub("<.*?>", "", res) -> res
  return(res)} )

pns = gsub("[(\\[].+[])]", "", fns)
pns = gsub(" +", " ", pns)
pns = ODNB_fix_accent_html(pns)

full_metadata = data.frame(ID = good_names, raw_name = sapply(ext_names[good_names], function(x) {x$text}), full_name = fns[good_names], short_name = pns[good_names], Occu = occs, Dates = temp_dates, BioLength = bio_len[good_names], stringsAsFactors = FALSE) 

save(full_metadata, file = zzfile_textproc_preproc_metadata)


## TODO: [Clean] The remainder of the file. 
## Need to process remainder of file; do we need later code? 
## commented code below means probably obselete. 











































## Old get.dates function. Needs to be obseleted.
## TODO: This function is obselete; need to remove all references to it. 
get.dates <- function(text) { ## Updated 12/30/2012 3pm
  ## Input      text = char vector (format = processed through dnb.grab.main)
  ## Output     char singleton
  ## 
  ## Returns an entry containing dates for bio
  notdone = TRUE
  counter = 1
  while(notdone) {
    a = gregexpr(text = text[1], pattern = "\\(")[[1]][counter]
    b = gregexpr(text = text[1], pattern = "\\)")[[1]][counter]
    tx = substr(text[1], start = a+1, stop = b-1)
    tx = gsub(pattern = "&#150;", replacement = "-",tx) 
    if (length(grep("[[:digit:]]", tx)) == 0) {
      counter = counter + 1
    } else {
      notdone = FALSE
    }
  }
  return(tx)
}


# Code to process all dates
NN = length(temp_dates)
date_data = data.frame(Birth=rep(0, times = NN), Death=rep(0, times = NN), 
                       Alive=rep(0, times = NN), type = rep("not_processed", times = NN),
                       Birth_mod = rep("", times = NN), Death_mod= rep("", times = NN),
                       Alive_mod = rep("", times = NN), Birth_uncertainty = rep(0, times = NN),
                       Death_uncertainty = rep(0, times = NN), Alive_uncertainty = rep(0, times = NN),
                       stringsAsFactors = FALSE)

cat(sum(date_data$type == "not_processed"), "entries to process")
for(j in which(date_data$type == "not_processed")) {
  cat(".")
  date_data[j,] = process_ODNB_date(temp_dates[j])
  if(j %% 25 == 0) { print(j) }
}

in_date_range = union(which(date_data$type == "not_processed"),
  intersect(which(date_data$Birth < 1800),
            which(date_data$Death > 1549)))
in_date_range = sort(in_date_range)
z = in_date_range
# 
# fns = sapply(ext_names, function(x) {
#   x$text -> res
#   gsub("<.*?>", "", res) -> res
#   return(res)} )
# 
# pns = gsub("[(\\[].+[])]", "", fns)
# pns = gsub(" +", " ", pns)
# pns = ODNB_fix_accent_html(pns)
# 
# 
# 
# 
# fin_result = data.frame(
#   ID = good_names[z],
#   main_name = pns[z],
#   full_name = fns[z],
#   ext_birth = date_data$Birth[z], ext_death = date_data$Death[z],
#   full_date = temp_dates[z],
#   occupation = occs[z],
#   bio_length = bio_len[good_names[z]])
# 
# head(fin_result, 30)
# 
# write.csv(fin_result, file = "test.csv", row.names = FALSE)
# full_result = data.frame(
#   ID = good_names,
#   main_name = pns[good_names],
#   full_name = fns[good_names],
#   ext_birth = date_data$Birth, ext_death = date_data$Death,
#   full_date = temp_dates,
#   occupation = occs,
#   bio_length = bio_len[good_names], stringsAsFactors = FALSE)
# 
# save(full_result, file = "data/ODNB_raw/ODNB_metadata20140404.Rdata")

res
































ODNB_parse_date(

length(ODNB_extract_name_segment)
length(ODNB_cleantext)
ODNB_groupcosub


ODNB_extract_name_segment(ODNB_text[[2]]) -> z
ODNB_parse_name(z[[1]])

ODNB_parse_date(ODNB_text[[2]], z$end) -> y
process_ODNB_date(y)
ls()



















## Store html in datafile
odnb_data$html = list()
for(i in 1:99999) {
  if (is.nobio(ODNB_rawHTML[[i]])) {
  } else {
    odnb_data$html[[i]] = dnb.grab.main(ODNB_rawHTML[[i]])
  }
  if (i %% 100 == 0) {print(i)}
}

ODNB_data = list()
counter = 1
for(i in 1:99999) {
  if (!is.null(odnb_data$html[[i]])) {
    ODNB_data[[counter]] = list(
               html = odnb_data$html[[i]],
               number = i)
    counter = counter + 1
  }
  if (i %% 1000 == 0) {print(i)}
}

ODNB.nums = sapply(ODNB_data, function(x) { x$number })
ODNB.nums.inv = rep(NA, times = 99999)
ODNB.nums.inv[ODNB.nums] = 1:length(ODNB.nums)
head(ODNB.nums.inv)
tail(ODNB.nums.inv)

ODNB_rawHTML[[j]]

for(j in setdiff(1:99999, cosub_list[[88]])) {
  print(j)
  ODNB_extract_name_segment(dnb_grab_main(ODNB_rawHTML[[j]]))$text
}

sapply(ODNB_rawHTML, function(x) {ODNB_extract_name_segment(x)$text})

odnb_temp_name = t(sapply(ODNB_data,
  function(x) {ODNB_extract_name_segment(x$html)}))


ODNB_parse_name = function(text) {
  ## extract name, and make notes for additional components
  ## text = odnb_temp_name[1,1]
  if(text == "") {return(NULL)}
  text = gsub("^<span.*?>", "", text)
  text = gsub("</span>$", "", text)

  ## extract segments demarqued by [] or <span>
  r = "(<span|\\[|\\().+(span>|]|\\))"
  match = gregexpr(r, text)
  match = regmatches(text, match)

  plaintext = gsub(r,"", text)
  return(list(simple=plaintext, extra=match))
}

odnb_names = t(sapply(odnb_temp_name[,1], ODNB_parse_name))





temp_dates = rep("", times = length(ODNB_data))
for(i in 1:length(ODNB_data)) {
  if (i %% 100 == 0) {print(i)}
  temp_dates[i] = ODNB_parse_date(ODNB_data[[i]]$html, odnb_temp_name[i,2]$end)
}

i = 100
ODNB_parse_date(ODNB_data[[1]]$html[[i]], odnb_temp_name[i,2]$end)



## TODO: This stuff needs to be rewritten...


process_ODNB_date = function(text) {
  # this function processes a single ODNB date (as text segment)
  # TODO: Document this function
  
  output = data.frame(Birth=NA, Death=NA, 
                      Alive=NA, type = "not_processed",
                      Birth_mod = "", Death_mod= "",
                      Alive_mod = "", Birth_uncertainty = 0,
                      Death_uncertainty = 0, Alive_uncertainty = 0,
                      stringsAsFactors = FALSE)
  # Birth uncertainty = number of years later birth might have occurred
  # Alive uncertainty = number of years later alive date might have been
  # Death uncertainty = number of years BEFORE death could have occurred
  text = gsub("^\\(", "", text)
  text = gsub("\\)$", "", text)
  
  num_dashes = nchar(text) - nchar(sub("-", "", text))
  
  # Rid too many dashes (unprocessable)
  if (num_dashes > 1) {
    output$type[1] = "unprocessable"
    return(output)
    # => manual processing / unprocessable
  }
  
  if (num_dashes == 1) {
    
    dates = strsplit(text, "-")[[1]]
    # plain xxx-xxx, and with modifiers
    d1 = extract_date_modifier(dates[1])
    d2 = extract_date_modifier(dates[2])
    if (length(grep("^[[:digit:]]+$", c(d1[1], d2[1]))) == 2) {
      # TODO: Make this check separately; extract separately 
      
      output$Birth[1] = as.numeric(d1[1])
      output$Death[1] = as.numeric(d2[1])
      output$type[1] = ""
      output$Birth_mod = d1[2]
      output$Death_mod = d2[2]
    }
    
    
  }

  return(output) 
}


extract_date_modifier = function(text) {
  
  modifier = ""
  possible_mods = c("<em>c.</em>", "<em>fl. </em>", "<em>per. </em>", 
                    "<em>act. </em>", "<em>supp. </em>", "in or after", "in or before", 
                    "after", "before", "late", "early")
  
  for(s in possible_mods) {
    if (length(grep(s, text)) == 1) {
      text = gsub(s, "", text)
      modifier = paste(modifier,gsub("</*em>", "", s) ,sep = "|")
    }
  }
  return( c(text, modifier))
}



temp_dates = gsub("&#150;", "-", temp_dates)
length(grep("[[:digit:]]", temp_dates))

no_dash = gsub("-", "", temp_dates)
num_dashes = nchar(temp_dates) - nchar(no_dash)
table(num_dashes)
temp_dates[which(num_dashes == 2)]

process_ODNB_date(temp_dates[2])

extract_date_modifier("<em>c.</em>1928")

# Code to process all dates
NN = length(temp_dates)
date_data = data.frame(Birth=rep(0, times = NN), Death=rep(0, times = NN), 
                       Alive=rep(0, times = NN), type = rep("not_processed", times = NN),
                       Birth_mod = rep("", times = NN), Death_mod= rep("", times = NN),
                       Alive_mod = rep("", times = NN), Birth_uncertainty = rep(0, times = NN),
                       Death_uncertainty = rep(0, times = NN), Alive_uncertainty = rep(0, times = NN),
                       stringsAsFactors = FALSE)

cat(sum(date_data$type == "not_processed"), "entries to process")
for(j in which(date_data$type == "not_processed")) {
  cat(".")
  date_data[j,] = process_ODNB_date(temp_dates[j])
  if(j %% 25 == 0) { print(j) }
}

save(date_data, odnb_names, temp_dates, ODNB.nums, ODNB.nums.inv, file = "data/master_list/date_data_v2_11112013.Rdata")


save(date_data, names, file = "data/master_list/date_data_v1_08162013.Rdata")

temp_dates[which(date_data$type == "not_processed")]
sum(date_data$type == "not_processed")
sum(date_data$type != "not_processed")


















