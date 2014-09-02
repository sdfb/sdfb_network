##########################################################################
#########################################################################
#### Reading in original data
##   This makes the original HTML files non-necessary, unless mistakes were
##   made in doing this. 


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


######################### This function copied over and modified 3/17/2014
# given
proc.tagtext <- function(tagged.text, type,
                         tag.types = c("PERSON", "ORGANIZATION", "LOCATION")) {
  ## returns vectors of tags matching tag. PERSON, ORGANIZATION, LOCATION
  #type = ST or LP
  #tag.types = existing tag.types
  
  toreturn = matrix("", nrow = length(tagged.text), ncol = length(tag.types))
  colnames(toreturn) = paste(type,tag.types,sep =":")
  
  # Finds where tags are
  if (type == "ST") {
    tagged.text = gsub("</*DATE>", "", tagged.text)
    tagged.text = gsub("</*TIME>", "", tagged.text)
    tagged.text = gsub("</*MONEY>", "", tagged.text)
    tagged.text = gsub("</*PERCENT>", "", tagged.text)
    begin = gregexpr(paste("<[[:alpha:]]+>", sep = ""), tagged.text)
    end = gregexpr(paste("</[[:alpha:]]+>", sep = ""), tagged.text)
    adj.start = 1
    adj.stop = 1
  } else if (type == "LP") {
    # tagged.text = gsub("</.*?>-<.*?>", "-", tagged.text) [maybe this can be avoided]
    begin = gregexpr(paste("<ENAMEXTYPE.+?>", sep = ""), tagged.text)
    end = gregexpr(paste("</ENAMEX>", sep = ""), tagged.text)
    adj.start = 13
    adj.stop = 2
  } else {
    print("Error in (fix_tagtext): Invalid TYPE")
#|                   ***********
#|----##replace period with _ --Tue Sep  2 09:43:54 2014--
    return(NULL)
  }
  
  #Assign types to gregexpr starts
  non.zero.begin = sapply(begin, function(x) { return(any(x > 0)) })
  non.zero.end = sapply(end, function(x){ return(any(x > 0)) })
  
  nn = sum(sapply(begin[non.zero.begin], length))
  all.matches = matrix("", nrow = 2*nn, ncol = 3) #col1 = match entry, 
  #col2 = match position, col3 = match type
  cur.pos = 1
  for(i in which(non.zero.begin)) {
    for(j in 1:length(begin[[i]])) {
      all.matches[cur.pos,1] = i
      all.matches[cur.pos,2] = begin[[i]][j]
      all.matches[cur.pos,3] = substr(x = tagged.text[i], 
                                      start = begin[[i]][j] + adj.start, 
                                      stop = attr(begin[[i]], "match.length")[j] - adj.stop)
      cur.pos = cur.pos + 2
    }
  }
  cur.pos = 2
  for(i in which(non.zero.end)) {
    for(j in 1:length(end[[i]])) {
      all.matches[cur.pos,1] = i
      all.matches[cur.pos,2] = end[[i]][j]
      all.matches[cur.pos,3] = "END"
      cur.pos = cur.pos + 2
    }
  }
  #   sorting = order(as.numeric(all.matches[,1]), as.numeric(all.matches[,2]))
  #   match.data = data.frame(ind=as.numeric(all.matches[,1]),
  #                           pos=as.numeric(all.matches[,2]),
  #                           type=all.matches[,3])
  
  #   find.type.gregexpr <- function(g.return, text) {
  #     substr(text, start = g.return[[1]], stop = g.return[[1]] + attr(g.return[[1]], "match.length") -1)
  #   }
  helper.f1 = function(x) {
    return(all.matches[x,1] == all.matches[x+1,1])
  }
  
  # Process results
  for (k in 1:length(tag.types)) {
    good = which(all.matches[,3] == tag.types[k])
    if (length(good) > 0) {
      # Single-length
      singles = sapply(good, helper.f1)
      toreturn[unique(as.numeric(all.matches[good[singles],1])),k] = "P"
      
      # longer: 
      for (m in good[!singles]) {
        s = as.numeric(all.matches[m,1])
        e = as.numeric(all.matches[m+1,1])
        check = (toreturn[s,k] == "PE")
        toreturn[s:e,k] = "P*"
        toreturn[e,k] = "PE"
        if (!check) {
          toreturn[s,k] = "PS"
        }
      }
    }
  }
  
  return(toreturn)
}



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


