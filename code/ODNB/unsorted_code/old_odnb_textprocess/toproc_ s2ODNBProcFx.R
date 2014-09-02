##########################################################################
#########################################################################
#### Reading in original data
##   This makes the original HTML files non-necessary, unless mistakes were
##   made in doing this. 


#################################################################################################
## 9/2/2014 -- Are these functions even used anymore? in a different form?
get.subject <- function(text) {
  ## Input      text = char vector (format = processed through dnb.grab.main)
  ## Output     char singleton
  ## 
  ## Returns an entry containing name that bio is about 
  
  b = gregexpr(text = text[1], pattern = "span")[[1]]
  tx = substr(text[1], start=b[1],stop=b[2])
  tx = strsplit(tx, split = ">")[[1]][2]
  tx = strsplit(tx, split = "<")[[1]][1]
  return(tx)
}
listify.text <- function(main.text, num) {
  ## Input      main.text = char vector (format = dnb) 
  ##            num = file number for text
  ## Output     named list: $about [person name]
  ##                        $num [file id]
  ##                        $len [number lines]
  ##                        $dates [dates for person]
  ##                        $text [dnb.grab.main result]
  ## 
  ## Returns a list of entries above, for each file
  
  ab = get.subject(main.text) 
  da = get.dates(main.text)
  len = length(main.text)
  temp = list(about = ab, num = num, len = len, dates = da, text = main.text)
  return(temp)
}
#################################################################################################








compute.error <- function(tr,pr) {
  ## Input      tr = char vec ("P",etc.) [TRUE TAGS]
  ##            pr = char vec ("P",etc.) [PREDICTED TAGS]
  ## Output     num vec
  ## 
  
  num.true.single = sum(tr == "P")
  num.true.mult = sum(tr == "PS")
  num.true = num.true.single + num.true.mult
  total.pred = length(which(pr == "P")) + length(which(pr == "PS"))
  
  # Processing pr = "P"'s
  ### complete matches (single)
  all.predict = which(pr == "P")
  ind.TS.id = intersect(which(tr == "P"), all.predict)
  num.TS.id = length(ind.TS.id)
  all.predict = setdiff(all.predict, ind.TS.id)
  
  ### partial match (single) of longer name
  ind.TM.partial.S = intersect(which(nchar(tr) == 2), all.predict)
  num.TM.partial.S = length(ind.TM.partial.S)
  all.predict = setdiff(all.predict, ind.TM.partial.S)
  
  ### remaining are misses
  num.miss.S = length(all.predict)
  
  
  # Processing pr = "PS".. 
  pr.starts = which(pr == "PS")
  pr.ends = which(pr == "PE")
  num.TM.id = 0
  num.TM.partial.M = 0
  num.miss.M = 0
  num.TS.overlap = 0
  for(i in 1:length(pr.starts)) {
    if (all(tr[pr.starts[i]:pr.ends[i]] == pr[pr.starts[i]:pr.ends[i]])) {
      num.TM.id = num.TM.id + 1
    } else if (any(nchar(tr[pr.starts[i]:pr.ends[i]]) == 2)) { 
      num.TM.partial.M = num.TM.partial.M + 1
    } else if (any(tr[pr.starts[i]:pr.ends[i]] == "P")) {
      num.TS.overlap = num.TS.overlap + length(tr[pr.starts[i]:pr.ends[i]] == "P")
    } else {
      num.miss.M = num.miss.M + 1
    }
  }
  results = list()
  results[[1]] = c(TotalTrue=num.true, TotalTrueS=num.true.single, TotalTrueM=num.true.mult,
                   TotalFound=(num.TS.id+num.TM.id), TotalFoundS=num.TS.id, TotalFoundM=num.TM.id,
                   Total.S.Overlap=num.TS.overlap, 
                   Total.partial.S=num.TM.partial.S, Total.partial.S=num.TM.partial.M,
                   Total.Partial=(num.TS.overlap+num.TM.partial.S+num.TM.partial.M),
                   Total.Miss.S=num.miss.S, Total.Miss.M=num.miss.M)
  results[[2]] = c(paste("Recall:", results[[1]][4]/results[[1]][1] * 100),
                   paste("Recall+Part:", 100*(results[[1]][4] + results[[1]][10] *.5)/results[[1]][1]),
                   paste("Precision:", 100*(results[[1]][4]/total.pred)) )
  return(results)
}


