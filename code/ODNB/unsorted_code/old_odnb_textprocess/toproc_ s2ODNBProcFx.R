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

### functions for processing truetext and combining processed outputs

paste.two <- function(vec,ind.start) {
  # this pastes together two consecutive entries in a vector
  #  starting with ind.start and then ind.start+1
  #  and edits the vector to remove the extra cell
  vec[ind.start] = paste(vec[ind.start], vec[ind.start+1], sep = "")
  return(vec[-(ind.start+1)])
}


###########################
############# 3/17/2014 : this function copied over / redone for updated version
fix.tagtext <- function(text, true.text, type) { 
  ## This vectorizes, normalizes tagged text
  ## type = "ST" or "LP"
  
  #   text = gsub("£", "?", text) ## pound symbol... convert to ?
  #   text = gsub("&amp;", "&", text)
  
  if (type == "ST") {    
    # do nothing
  } else if (type == "MT") {
    # man.tag issues
    text = gsub("£", "$", text)
  } else if (type == "LP") {
    ## Single-words ENAMEX, fixes ' as organization issue. 
    text = gsub("<ENAMEX TYPE=\"ORGANIZATION\">'</ENAMEX>","\'", text)
    text = gsub("ENAMEX TYPE=", "ENAMEXTYPE=", text)
    text = gsub("&amp;", "&", text)
    text = gsub("&quot;", "\"", text)
  } else {
    print("Error in (fix.tagtext): Invalid TYPE")
    return("Error in (fix.tagtext): Invalid TYPE")
  }
  
  words = strsplit(text, " ")
  words = c(words, recursive = TRUE)
  words = words[words != ""]
  
  to.test = gsub("<.*?>", "", words) # removing all <>'s , comparing spacing
  j = 1
  N = length(true.text)
  #N = 336
  while(j <= N) {
    if (true.text[j] == to.test[j]) {
      j = j+1
    } else {
      if (length(grep(paste(to.test[j],to.test[j+1], sep = ""),
                      true.text[j], fixed = TRUE)) > 0) {
        to.test = paste.two(to.test, j)
        words = paste.two(words, j)
      } else {
        print(paste("ERROR... temp: j=",j))
        return("ERROR.")
      }
    }
  }
  while(length(words) > N) {
    words = paste.two(words, N)
  }
  
  return(words)
}
##############3

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
    print("Error in (fix.tagtext): Invalid TYPE")
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

### Copied over 3/24/2014
combine.two <- function(a,b) {
  ## Input      a,b = char vec ("P", "PS", etc.)
  ## Output     char vec ("P",etc.)
  ## 
  ## Takes two predictors and outputs a combined predictor vector
  
  if (length(a) != length(b)) { error("ERROR: mismatching vector lengths") }
  
  toreturn = a
  a.blank = which(toreturn == "")
  
  # add singles from 2nd one. 
  b.single = which(b == "P")
  toreturn[intersect(b.single, a.blank)] = "P" # fill in
  a.fill = which(toreturn != "")
  
  # add long-matches
  starts = which(b == "PS")
  ends = which(b == "PE")
  if (length(starts) > 0) {
    for(i in 1:length(starts)) {
      both = intersect(starts[i]:ends[i], a.fill)
      if (length(both) == 0) {
        toreturn[starts[i]:ends[i]] = b[starts[i]:ends[i]]
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

########## This function copied over. 4/4/2014
find.tagpositions <- function(mat) { ## formerly find.names
  ## Input      mat = two column matrix of predictions (1st col = text, 2nd = "PS", etc
  ## Output     matrix, 1st col = names identified; 2nd col = location
  ## 
  ## Finds all identified names and outputs it
  
  single = which(mat[,2] == "P")
  starts = which(mat[,2] == "PS")
  ends = which(mat[,2] == "PE")
  
  ## sanity check for tags: make sure appropriate
  if (length(starts) != length(ends) ) {
    return("ERROR (find.names): not same number of PS, PE")
  } 
  num.errs = sum(starts > ends) 
  if (num.errs > 0) {
    return("ERROR (find.names): not all PS/PE are in correct order")
  }
  
  ## Single matches 
  if (length(single) > 0) {
    toreturn = cbind(mat[single, 1],single)
  } else { toreturn = NULL }
  
  # Multiple-length matches    
  if (length(starts) > 0) {
    temp = matrix(nrow = length(starts), ncol = 2)
    for (i in 1:length(starts)) {
      temp[i,] = c(paste(mat[starts[i]:ends[i], 1], collapse = " "), starts[i])
    }
  } else { temp = NULL }
  
  temp2 = rbind(toreturn, temp)
  text = text.remove.punct(temp2[,1])
  text = text.remove.punct(text)
  temp = data.frame(Name=text, Location=as.numeric(temp2[,2]), stringsAsFactors = FALSE)
  
  return(temp[order(temp[,2]),])
}

## copied4/4/2014
text.remove.punct <- function(vec) {
  # Always run TWICE...
  # Removes punctuation from every entry in vec, to go from text entry to "entity name"
  vec = gsub("[,;!?:()|`]", "", vec) #remove `,;!?| (),[]'s
  vec = gsub("\\[", "", vec)
  vec = gsub("]", "", vec)
  
  # Remove tailing 's
  vec = gsub("'s$", "", vec)
  
  #remove tailing/leading ' 's
  vec = gsub(" '+", " ", vec)
  vec = gsub("'+ ", " ", vec)
  vec = gsub("^'+", "", vec)
  vec = gsub("'+$", "", vec)
  vec = gsub("'[.]$", "", vec)
  
  # remove .'s not followed by a SINGLE cap letter
  all.periods = grep("[.]", vec)
  spec.periods = grep(" [[:upper:]][.]", vec)
  diff = setdiff(all.periods, spec.periods)
  vec[diff] = gsub("[.]", "", vec[diff])
  if (length(spec.periods) > 0) {
    for (i in spec.periods) {
      a = gregexpr("[^[:upper:]][.]", vec[i])[[1]]
      if (a[1] != -1) {
        vec[i] = paste(substr(vec[i], 1, a[1]), substr(vec[i], a[1]+2, nchar(vec[i])), sep = "")
      }
    }
  }
  
  return(vec)
}
## COpied 4/4/2014
remove.badentities <- function(comb.tag.mat, position.df) {
  # Anything with \\, /, $, +, *, =, # is not going to be an entity. 
  # Anything with numbers is not going to be an entity
  # Anything length 2 or shorter in total is not an entity
  # Anything 
  
  # Remove unlikely entries: 
  is.cap = function(vec) {
    # checks if it is a potential name (has caps, or de or of)
    b = grep("^[[:upper:]]", vec)
    num.namewords = length(b) + sum(vec == "de") + sum(vec == "of")
    return(num.namewords == length(vec))
  }
  text.totest = strsplit(position.df[,1], split = " ")
  
  bad = unique(c(grep("[\\/$+#*=]",position.df[,1]),
                 grep("[1234567890]", position.df[,1]),
                 which(nchar(position.df[,1]) <= 2),
                 which(!sapply(text.totest, is.cap)) ))
  if (length(bad) > 0) {
    pos = position.df[bad,2]
    comb.tag.mat[,2] = remove.entries(tags = comb.tag.mat[,2], entries = pos)
    return(list(comb.tag.mat, position.df[-bad,]))
  } else {
    return(list(comb.tag.mat, position.df))
  }
}

## Copied 4/4/2014
remove.entries <- function(tags, entries) {
  toreturn = tags
  for (j in entries) {
    if (toreturn[j] == "P") {
      toreturn[j] = ""
    } else if (toreturn[j] == "PS") {
      end = which(toreturn == "PE")
      end = min(end[end >= j])
      toreturn[j:end] = ""
    } else { error("ERROR: why is it not P/PS") }
  }
  return(toreturn)
}
## COpied 4/4/2014
insert.tag <- function(start, len, tags) { # Check for null return. 
  if (all(tags[start:(start+len-1)] == "")) {
    if (len == 1) {
      tags[start] = "P"
    } else if (len == 2) {
      tags[start] = "PS"
      tags[start + 1] = "PE"
    } else if (len > 2) {
      tags[start] = "PS"
      tags[start + len - 1] = "PE"
      tags[start:(start + len - 1)] = "P*"
    }
  } else { # already tagged: 
    return(NULL)
  }
  return(tags)
}

# totest: 

##### Copied Over 3/24/2014
improve.pred <- function(comb.tag, main.person = NULL,
                         exact.words = NULL, regex.words = NULL) {
  # basically, adjustd version of analyze.text
  # input combined tags, etc.; output is cleaned up version with new matches etc. 
  # comb.tag = thing in combined.tags
  # main.person is a singleton character
  
  print("::::: Setup, Tagging")
  # Resolve current matches into Entity/Position matrix
  tag.positions = find.tagpositions(comb.tag)
  
  # Remove weird entries (bad punct in entities, non-capitalized: see remove.badentities)
  temp = remove.badentities(comb.tag.mat = comb.tag, position.df = tag.positions)
  comb.tag = temp[[1]]
  tag.positions = temp[[2]]
  
  # fix spaces posssibly in matches, remove leading/trailing spaces
  tag.positions[,1] = gsub(" +", " ", tag.positions[,1])
  tag.positions[,1] = gsub("^ ", "", tag.positions[,1])
  tag.positions[,1] = gsub(" $", "", tag.positions[,1])
  
  print("::::: Tabulating Current Matches")
  # Create matches.matrix
  if (is.null(main.person)) {
    unique.entities = unique(tag.positions[,1])
    # If main person is known, it will be first in the list, id = 1
    matches.matrix = data.frame(NumWords=1+nchar(gsub("[^ ]", "", unique.entities)), 
                                MatchName=unique.entities, 
                                ID=1+(1:length(unique.entities)), 
                                stringsAsFactors = FALSE)
  } else {
    unique.entities = unique(c(main.person, tag.positions[,1])) 
    # If main person is known, it will be first in the list, id = 1
    matches.matrix = data.frame(NumWords=1+nchar(gsub("[^ ]", "", unique.entities)), 
                                MatchName=unique.entities, 
                                ID=1:length(unique.entities), 
                                stringsAsFactors = FALSE)
  }
  
  # Create idlist
  cur.match.id = match(tag.positions[,1], matches.matrix$MatchName)
  idlist = data.frame(MatchName=tag.positions[,1], Position=tag.positions[,2],
                      ID=matches.matrix$ID[cur.match.id], Length=nchar(tag.positions[,1]))
  
  print("::::: Removing odd words")
  remove.words <- function(matches.matrix, idlist, comb.tag,
                           exact.words = NULL, regex.words = NULL) {
    # returns list(FNI, IDS), removing any entries continaing 'words'
    ids.to.rm = NULL
    for (j in exact.words) {
      sums = rep(0, times = length(matches.matrix$MatchName))
      for (m in 1:length(matches.matrix$MatchName)) {
        sums[m] = sum(strsplit(matches.matrix$MatchName[m],split = " ")[[1]] == j)
      }
      ids.to.rm = c(ids.to.rm, which(sums > 0))
    }
    
    for (j in regex.words) {
      ids.to.rm = c(ids.to.rm,grep(j, matches.matrix$MatchName[-1]))
    }
    
    ids.to.rm = setdiff(unique(ids.to.rm), 1)
    matches.toclear = NULL
    for (j in ids.to.rm) {
      matches.toclear = c(matches.toclear, which(idlist$ID == matches.matrix$ID[j]))
    }
    if (!is.null(matches.toclear)) {
      matches.matrix = matches.matrix[-ids.to.rm,]
      comb.tag[,2] = remove.entries(tags = comb.tag[,2], 
                                    entries = idlist$Position[matches.toclear])
      idlist = idlist[-matches.toclear,]
      
    }
    return(list(matches.matrix, idlist, comb.tag))
  }
  
  temp = remove.words(matches.matrix = matches.matrix, idlist = idlist, comb.tag = comb.tag,
                        regex.words = regex.words, exact.words = exact.words)
  matches.matrix = temp[[1]]
  idlist = temp[[2]]
  comb.tag = temp[[3]]
  
  print("::::: Searching for entities within document --- Removing sub-matches")
  only.cap = function(vec) {
    return(vec[grep("^[[:upper:]]", vec)])
  }
  
  # First, set 1-word names that are partial two-word names to have id = -1
  list.of.names = unique(c(strsplit(matches.matrix[matches.matrix$NumWords == 2,2], split = " "), 
                           recursive = TRUE))
  if (!is.null(main.person)) {
    list.of.names = unique(c(list.of.names, only.cap(strsplit(main.person, split = " "))))
  }
  list.of.names = only.cap(list.of.names[nchar(list.of.names) > 3])
  
  mat.rm = NULL
  for(s in list.of.names) {
    torm.mat = which(matches.matrix$MatchName == s)
    if (length(torm.mat) > 0) {
      mat.rm = c(mat.rm, torm.mat)
    }
  }
  mat.rm = setdiff(mat.rm, which(matches.matrix$ID == 1)) # make sure main is not used
  for (j in mat.rm) {
    idlist$ID[idlist$ID == matches.matrix$ID[j]] = -1
  }
  if (length(mat.rm) > 0) {
    matches.matrix = matches.matrix[-mat.rm,]
  }
  
  print("::::: Searching --- Finding more matches: Exact, length > 1")
  text = text.remove.punct(text.remove.punct(comb.tag[,1]))
  longs = which(matches.matrix$NumWords > 1)
  
  to.add = matrix("", nrow = 2500, ncol = 3)
  cur.add.index = 1
  for(j in longs) {
    splitted = strsplit(matches.matrix[j,2], " ")[[1]]
    fir = grep(splitted[1], text)
    for(i in fir) {
      if (i < length(comb.tag[,2]) - length(splitted) + 1) {
        if (sum(comb.tag[,2][i:(i+length(splitted) - 1)] == "") == length(splitted)) {
          num = 0
          for (k in 1:length(splitted)) {
            num = num + length(grep(splitted[k], text[i + k - 1]))
          }
          if (num == length(splitted)) {
            temp = insert.tag(start = i, len = num, tags = comb.tag[,2])
            if (!is.null(temp)) {
              comb.tag[,2] = temp
              to.add[cur.add.index,] = c(paste(text[i:(i+num-1)], collapse = " "), i, 
                                         matches.matrix$ID[j])
              cur.add.index = cur.add.index + 1 
            }
          }
        }
      }
    }
  }
  
  print("::::: Searching --- Finding more matches: Partial")
  # Search partial words of two-word appearances. set ID = -1
  shorts = list.of.names
  for (j in shorts) {
    loc = grep(j, text)
    for(i in loc) {
      temp = insert.tag(start = i, len = 1, tags = comb.tag[,2])
      if (!is.null(temp)) {
        comb.tag[,2] = temp
        to.add[cur.add.index,] = c(text[i], i, -1)
        cur.add.index = cur.add.index + 1 
      }
    }
  }
  
  print("::::: Searching --- Finding more matches: Exact, length = 1")
  # Search text for remaining 1 word appearances
  shorts.id = which(matches.matrix$NumWords == 1)
  shorts = setdiff(matches.matrix$MatchName[shorts.id], list.of.names)

  for (j in shorts) {
    word.id = which(matches.matrix$MatchName == j)
    loc = grep(j, text)
    for(i in loc) {
      temp = insert.tag(start = i, len = 1, tags = comb.tag[,2])
      if (!is.null(temp)) {
        comb.tag[,2] = temp
        to.add[cur.add.index,] = c(text[i], i, matches.matrix$ID[word.id])
        cur.add.index = cur.add.index + 1 
      }
    }
  }
  to.add = to.add[to.add[,1] != "",,drop=FALSE]
  to.add = data.frame(MatchName=to.add[,1], Position=as.numeric(to.add[,2]),
                      ID=as.numeric(to.add[,3]), Length=nchar(to.add[,1]),
                      stringsAsFactors=FALSE)
  idlist = rbind(idlist, to.add)
  to.add = NULL
  idlist = idlist[order(idlist$Position),]
  
  print("::::: Resolving un-ID'd matches")
  two.word.id = which(matches.matrix$NumWords == 2)
  splits = strsplit(matches.matrix$MatchName, split = " ")
  total = c(splits[two.word.id], recursive = TRUE)
  all.firsts = total[(1:length(two.word.id)) * 2- 1]
  all.lasts = total[(1:length(two.word.id)) * 2]
  newids = idlist$ID
  
  for (j in list.of.names) {
    index.of.matches = which(idlist$MatchName == j)
    if (length(index.of.matches) > 0) {
      firsts = two.word.id[which(all.firsts == j)]
      lasts = two.word.id[which(all.lasts == j)]
      ids.of.matches = as.numeric(matches.matrix$ID[unique(c(firsts, lasts))])
      if (!is.null(main.person)) {
        main.split = strsplit(main.person, split = " ")
        if (sum(only.cap(main.split) == j) > 0) {
          ids.of.matches = unique(c(1, ids.of.matches))
        }
      }
      
      if(any(ids.of.matches == 1)) {
        newids[index.of.matches] = 1

      } else {
        prev.locs = NULL
        for(n in 1:length(ids.of.matches)) {
          prev.locs = rbind(prev.locs, 
                            cbind(ID=ids.of.matches[n], 
                                  Pos=idlist$Position[idlist$ID == ids.of.matches[n]] ))
        }
        
        if (length(prev.locs) == 2) {
          newids[index.of.matches] = prev.locs[1]
        } else if (length(prev.locs) > 2) {
          match.locations = idlist$Position[index.of.matches]
          for(n in 1:length(index.of.matches)) {
            temp = prev.locs[,2]
            temp[temp > match.locations[n]] = 0
            if (max(temp) == 0) {
              newids[index.of.matches[n]] = prev.locs[which(prev.locs[,2] == min(prev.locs[,2])),1]
            } else {
              newids[index.of.matches[n]] = prev.locs[which(temp == max(temp)),1]
            }
          }
        }
        
      }
    }
  }
  idlist$ID = newids
  
  print("::::: Done!")
  return(list(matches.matrix, idlist, comb.tag))
}

###############






