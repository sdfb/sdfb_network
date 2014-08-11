#@S Old code. Updating.

load("private_data/odnb_data_proc/ODNBdatav2.Rdata")

head(ODNB.names, 50)
ODNB_dates = sapply(ODNB.data, function(x) {return(x$dates)})

names = gsub("<.*?>", "", ODNB.names)
names = gsub(" +", " ", names)
head(names)

# extract length of biography
bio_lengths = sapply(1:58265, function(x) {sum(nchar(ODNB.data[[x]]$text))})

length(grep("[[:digit:]]", ODNB_dates))   
no_dash = gsub("-", "", ODNB_dates)
num_dashes = nchar(ODNB_dates) - nchar(no_dash)
table(num_dashes)
ODNB_dates[which(num_dashes == 2)]

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

process_ODNB_date(ODNB_dates[2])

extract_date_modifier("<em>c.</em>1928")

# Code to process all dates
date_data = data.frame(Birth=rep(0, times = 58265), Death=rep(0, times = 58265), 
                       Alive=rep(0, times = 58265), type = rep("not_processed", times = 58265),
                       Birth_mod = rep("", times = 58265), Death_mod= rep("", times = 58265),
                       Alive_mod = rep("", times = 58265), Birth_uncertainty = rep(0, times = 58265),
                       Death_uncertainty = rep(0, times = 58265), Alive_uncertainty = rep(0, times = 58265),
                       stringsAsFactors = FALSE)

cat(sum(date_data$type == "not_processed"), "entries to process")
for(j in which(date_data$type == "not_processed")) {
  date_data[j,] = process_ODNB_date(ODNB_dates[j])
  if(j %% 10 == 0) { print(j) }
}

save(date_data, names, file = "data/master_list/date_data_v1_08162013.Rdata")

ODNB_dates[which(date_data$type == "not_processed")]



#################### OTHER code
load("ODNBdatav1.Rdata")

##### 12/30 testing old date processing; updating 
# messups = setdiff(1:58265, grep("[[:digit:]]{4}", dates.char))
# head(dates.char[messups])
# head(messups)
# aa = cbind(messups, dates.char[messups])
# get.dates(ODNB.data[[188]]$text)
# tail(aa)
# get.dates(ODNB.data[[58242]]$text)
# fixes = rep("", times = 58265)
# for(j in  messups) {
#   fixes[j] = get.dates(ODNB.data[[j]]$text)
#   print(j)
# }
# messups2 = setdiff(messups, grep("[[:digit:]]", fixes))
# dates.char[messups2]
# fixes[messups2]


## Process dates (call get.dates [s2ODNBProcFx.R])
dates.text = rep("", times = length(ODNB.names))
for (j in 1:length(ODNB.names)) {
  dates.text[j] = get.dates(ODNB.data[[j]]$text)
  print(j)
}

## Test if each date thing has at least a number [should be 58265]
length(grep("[[:digit:]]", dates.text))   


dates.matrix = matrix(NA, nrow = 58265, ncol = 3)
# 1st col = birthdate
# 2nd col = deathdate
# 3rd col = Flag for estimating: 
#   -assume 80 years lifespan if only birth/death given  (+- 80)
#   -assume +- 40 years from single date                    (40)
# Anytime there is a range, lower number is taken for simplicity

## Getting rid of 'c.' and 'fl.' and 'per.' and 'act.' and 'supp.'
dates.adj = dates.text
dates.adj = gsub("<em>c.</em>", "", dates.adj)
dates.adj = gsub("<em>fl. </em>", "", dates.adj)
dates.adj = gsub("<em>per. </em>", "", dates.adj)
dates.adj = gsub("<em>act. </em>", "", dates.adj)
dates.adj = gsub("<em>supp. </em>", "", dates.adj)

## remove 'in or after', 'in or before', 'after', 'before'
dates.adj = gsub("in or after", "", dates.adj)
dates.adj = gsub("in or before", "", dates.adj)
dates.adj = gsub("after", "", dates.adj)
dates.adj = gsub("before", "", dates.adj)
dates.adj = gsub("late", "", dates.adj)
dates.adj = gsub("early", "", dates.adj)

dates.adj = gsub("<span class=\"smallcap\">AD</span>", "", dates.adj)

## Getting rid of alternate dates (?'s, /'s)
dates.adj = gsub("/[[:digit:]]+", "", dates.adj)
dates.adj = gsub("\\?", "", dates.adj)
dates.adj = gsub("<span class=\"mult\">x</span>[[:digit:]]+", "", dates.adj)


## Plain dates of the format ####-####
type1 = grep("^[[:digit:]]{2,4}-[[:digit:]]{2,4}$", dates.adj)
for(j in 1:length(type1)) {
  temp = strsplit(dates.adj[type1[j]], "-")[[1]]
  dates.matrix[type1[j],1:2] = as.numeric(temp)
  if (j %% 100 == 1) { print(j) }
}

to.proc = setdiff(1:58265, type1)
dates.adj[type1] = ""


## Dates with <em>
to.test = grep("<em>", dates.adj)
dates.adj[to.test]

# Death (<em>d.</em>)
typed = gregexpr("<em>d.</em> [[:digit:]]{2,4}", dates.adj)
temp = get.all.match(typed, dates.adj)
max(sapply(temp, length))
temp = gsub("<em>d.</em> ", "", temp)

for(j in which(temp != "NULL")) {
  dates.matrix[j,2] = as.numeric(temp[j])
}
dates.adj = gsub("<em>d.</em>.+", "", dates.adj)

#dates.adj[nchar(dates.adj) > 0]

# Birth/baptism (b., bap.)

typeb = gregexpr("<em>b. </em>[[:digit:]]{2,4}", dates.adj)
temp = get.all.match(typeb, dates.adj)
max(sapply(temp, length))
temp = gsub("<em>b. </em>", "", temp)

for(j in which(temp != "NULL")) {
  dates.matrix[j,1] = as.numeric(temp[j])
}
dates.adj = gsub("<em>b. </em>.+,*", "", dates.adj)

typeb = gregexpr("<em>bap. </em>[[:digit:]]{2,4}", dates.adj)
temp = get.all.match(typeb, dates.adj)
max(sapply(temp, length))
temp = gsub("<em>bap. </em>", "", temp)

for(j in which(temp != "NULL")) {
  dates.matrix[j,1] = as.numeric(temp[j])
}
dates.adj = gsub("<em>bap. </em>.+,*", "", dates.adj)
dates.adj = gsub("^ +$", "", dates.adj)


dates.adj[nchar(dates.adj) > 0]

# cheap trick: make 1880s -> 1880, reprocess remainder
dates.adj = gsub("s", "", dates.adj)
dates.adj = gsub(" ", "", dates.adj)

## Remaining dates of the format ####-####
type1 = grep("^[[:digit:]]{2,4}-[[:digit:]]{2,4}$", dates.adj)
for(j in 1:length(type1)) {
  temp = strsplit(dates.adj[type1[j]], "-")[[1]]
  dates.matrix[type1[j],1:2] = as.numeric(temp)
  if (j %% 100 == 1) { print(j) }
}

dates.adj[type1] = ""

## Remaining single dates (+-40)
types = grep("^[[:digit:]]{4}$", dates.adj)
dates.matrix[types,1] = as.numeric(dates.adj[types]) - 40
dates.matrix[types,2] = as.numeric(dates.adj[types]) + 40
dates.matrix[types,3] = 40
dates.adj[types] = ""

# add in birthdate/deathdate estimate
for(j in 1:58265) {
  if (is.na(dates.matrix[j,1]) & !is.na(dates.matrix[j,2])) {
    dates.matrix[j,1] = dates.matrix[j,2] - 80
    dates.matrix[j,3] = -80
  } else if (is.na(dates.matrix[j,2]) & !is.na(dates.matrix[j,1])) {
    dates.matrix[j,2] = dates.matrix[j,1] + 80
    dates.matrix[j,3] = 80
  }
}

### 167 dates unprocessed... 

save(dates.matrix, file = "ODNBdates.Rdata")


