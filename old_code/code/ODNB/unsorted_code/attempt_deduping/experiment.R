#@S Code for attempting to dedupe
source("general_help_functions/HTMLProcessing.R")
load("private_data/odnb_data_proc/ODNBdatav2.Rdata")
#load("data/master_list/date_data_v1_08162013.Rdata")
load("data/master_list/date_data_v2_11112013.Rdata")
load("data/docu_count_matrix/0325sp.datamat.sparse.Rdata")
load("network_visualization/conf_matrix.Rdata")

split_names = strsplit(as.character(odnb_names[,1]), ",")
lnames = strip_spaces(sapply(split_names,
  function(x) {
    if (length(x) > 1) {
      return(x[1])
    } else {
      return(NA)
    }} ))

fnames = strip_spaces(sapply(split_names,
  function(x) {
    if(length(x) > 1) {
      paste(x[-1], paste = "", collapse = "")
    } else {
      return(x)
      }} ))

cnames = paste(fnames, lnames, sep = " ")
head(cnames)
head(lnames)
head(fnames, 300)
nam = "Charles Abbot"
match_name = function(nam, exact = TRUE) {
  ## Tries to match nam to the database. lnames/fnames global vars
  ## databse is in 'Last,First etc' format
  ## name is in two+word format, last word assumed as last name.
  sp = strsplit(nam, " ")[[1]]
  len = length(sp)
 
  if (exact) {
    l_matches = which(lnames == sp[len])
    f_matches = grep(paste(sp[-len], sep = " ", collapse = " "), fnames)
    return(intersect(f_matches, l_matches))
    
  } else {
    sp_reg = paste(".*", gsub(" ", ".*", sp), ".*", sep = "", collapse = ".*")
    sp_reg = gsub("([.][*])+", ".*", sp_reg)
    return(grep(sp_reg, cnames))
    ## not implemented
    ## grep(sp[len], lnames)
  }
}

match_name("John Milton", exact = FALSE) -> z
cnames[z]

text = "John Milton"
match_bio = function(text) {
  colnum = which(colnames(sparse.dm) == text)
  rows = which(sparse.dm[,colnum] > 1)
  docs = strsplit(rownames(sparse.dm)[rows], "[.]")
  docs = as.numeric(unique(sapply(docs, function(x) {x[1]})))
  return(ODNB.nums.inv[docs])
}
match_name_bio = function(text, exact = TRUE) {
  return(intersect(
    match_name(text, exact = exact),
    match_bio(text)))
}

match_name_bio("Charles I", exact = FALSE)

i = 1
sum(sapply(ODNB.data[[i]]$text, nchar))

doclengths = sapply(ODNB.data, function(x) {sum(sapply(x$text, nchar))})

## find matches for each person in network

ls()
potential_matches = list()
for(j in 1:ncol(sparse.dm)) {
  potential_matches[[j]] = match_name_bio(colnames(sparse.dm)[j])
  if(j %% 10 == 0) {print(j)}
}
which(sapply(potential_matches, length) == 0) -> z
for(j in z) {
  potential_matches[[j]] = match_name_bio(colnames(sparse.dm), exact = FALSE)
  cat(".")
}

potential_matches[[3]]
rownums = as.numeric(sapply(strsplit(rownames(sparse.dm), "[.]"), function(x) {x[1]}))

frac_50 = list()
frac_25 = list()
frac_75 = list()
for(j in 1:length(potential_matches)) {
  print(j)
  if (length(potential_matches[[j]]) > 0) {
    frac_50[[j]] = rep(0, times = length(potential_matches[[j]]))
    frac_25[[j]] = rep(0, times = length(potential_matches[[j]]))
    frac_75[[j]] = rep(0, times = length(potential_matches[[j]]))
    
    for(k in 1:length(potential_matches[[j]])) {
      rows = setdiff(which(rownums == ODNB.nums[potential_matches[[j]][k]]),j)
      if (length(rows) > 1) {
        links = which(apply(sparse.dm[rows,], 2, sum) > 0)
      } else {
        links = which(sparse.dm[rows,] > 0)
      } 
      n = length(links)
      frac_50[[j]][k] = sum(!is.na(match(links,  which(conf_matrix[j,] > 50))))/n
      frac_25[[j]][k] = sum(!is.na(match(links,  which(conf_matrix[j,] > 25))))/n
      frac_75[[j]][k] = sum(!is.na(match(links,  which(conf_matrix[j,] > 75))))/n
    }
  }
}

  
for(j in 1:6289) {

}

z = match(rownums, ODNB.nums[potential_matches[[3]]])

colnames(sparse.dm)[[4813]]

t = read.csv("bacon_central_and_between.txt", stringsAsFactors = FALSE)
match(t$name, colnames(sparse.dm))

sapply(frac_50, function(x) {
  if ( (length(x) > 1) & (sum(x > 0.5) > 1)) {return(TRUE) } else { return(FALSE)}}) -> z
sum(z)

match(t$name, colnames(sparse.dm))
reorder_t = t[match(colnames(sparse.dm), t$name), ]
head(reorder_t)
plot(reorder_t$betweenness, reorder_t$centrality, col = z + 1,
     xlim = c(0, 0.04), ylim = c(0, 0.3))

conf_80 = apply(conf_matrix, 1, function(x) {sum(x >= 80)})
conf_50 = apply(conf_matrix, 1, function(x) {sum(x >= 50)}) - conf_80
plot(conf_50, conf_80, col = z + 1, xlim = c(0, 200))


c_data = data.frame(net_name="", deg80=0, deg50=0, odnb_name="", odnb_date="", fr50=0, fr75=0, stringsAsFactors = FALSE)

for(j in 1:6289) {
  print(j)
  n = length(potential_matches[[j]])
  m = potential_matches[[j]]
  if (n == 0) {
    c_data = rbind(c_data,
      data.frame(net_name=reorder_t$name[j], deg80=conf_80[j], deg50=conf_50[j], odnb_name="", odnb_date="", fr50=NA, fr75=NA, stringsAsFactors = FALSE))
  } else {
    c_data = rbind(c_data,
      data.frame(net_name=reorder_t$name[j],
                 deg80=conf_80[j], deg50=conf_50[j], odnb_name=as.character(odnb_names[m,1]),
                 odnb_date=ifelse(!is.na(date_data$birth[m]), paste(
                   date_data$birth[m],
                   "-", date_data$death[m],
                   sep = ""), temp_dates[m]),
                 fr50=frac_50[[j]], fr75=frac_75[[j]],
                 stringsAsFactors = FALSE))
  }
  
}

write.csv(c_data, file = "split_name.csv")
