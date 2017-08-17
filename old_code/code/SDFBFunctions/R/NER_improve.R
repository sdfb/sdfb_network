#@S Functions relating to NER iterative improvements

## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (improve_pred)
#' <<BasicInfo>> 
#' 
#' @param comb.tag temp
#' @param main.person temp
#' @param exact.words temp
#' @param regex.words temp
#' 
#' @return temp
#' 
#' @export
#' 
improve_pred = function(comb.tag, main.person = NULL,
                         exact.words = NULL, regex.words = NULL) {
  # basically, adjustd version of analyze.text
  # input combined tags, etc.; output is cleaned up version with new matches etc. 
  # comb.tag = thing in combined.tags
  # main.person is a singleton character
  
  print("::::: Setup, Tagging")
  # Resolve current matches into Entity/Position matrix
  tag.positions = find_tagpositions(comb.tag)
  
  # Remove weird entries (bad punct in entities, non-capitalized: see remove_badentities)
  temp = remove_badentities(comb.tag.mat = comb.tag, position.df = tag.positions)
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
      comb.tag[,2] = remove_entries(tags = comb.tag[,2], 
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
  text = text_remove_punct(text_remove_punct(comb.tag[,1]))
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
            temp = insert_tag(start = i, len = num, tags = comb.tag[,2])
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
      temp = insert_tag(start = i, len = 1, tags = comb.tag[,2])
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
      temp = insert_tag(start = i, len = 1, tags = comb.tag[,2])
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


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (find_tagpositions)
#' <<BasicInfo>> 
#' 
#' @param mat temp
#' 
#' @return temp
#' 
#' @export
#' 
find_tagpositions = function(mat) { ## formerly find.names
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
  text = text_remove_punct(temp2[,1])
  text = text_remove_punct(text)
  temp = data.frame(Name=text, Location=as.numeric(temp2[,2]), stringsAsFactors = FALSE)
  
  return(temp[order(temp[,2]),])
}


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (text_remove_punct)
#' <<BasicInfo>> 
#' 
#' @param vec temp
#' 
#' @return temp
#' 
#' @export
#' 
text_remove_punct = function(vec) {
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

## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (remove_badentities)
#' <<BasicInfo>> 
#' 
#' @param comb.tag.mat temp
#' @param position.df temp
#' 
#' @return temp
#' 
#' @export
#' 
remove_badentities = function(comb.tag.mat, position.df) {
  # Anything with lower-case words (except 'de' or 'of') is not an entity
  # Anything with \\, /, $, +, *, =, # is not an entity
  # Anything with numeric characters is not an entity
  # Anything 2 or fewer characters is not an entity
  
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
    comb.tag.mat[,2] = remove_entries(tags = comb.tag.mat[,2], entries = pos)
    return(list(comb.tag.mat, position.df[-bad,]))
  } else {
    return(list(comb.tag.mat, position.df))
  }
}

## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (remove_entries)
#' <<BasicInfo>> 
#' 
#' @param tags temp
#' @param entries temp
#' 
#' @return temp
#' 
#' @export
#' 
remove_entries = function(tags, entries) {
#|**************
#|----##replace period with _ --Mon Sep  1 16:56:42 2014--
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

## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (insert_tag)
#' <<BasicInfo>> 
#' 
#' @param start temp
#' @param len temp
#' @param tags temp
#' 
#' @return temp
#' 
#' @export
#' 
insert_tag = function(start, len, tags) { # Check for null return. 
#|**********
#|----##replace period with _ --Mon Sep  1 16:57:46 2014--
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


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (find_closest_date)
#' <<BasicInfo>> 
#' 
#' @param matches temp
#' @param tag_df temp
#' @param doc_split temp
#' 
#' @return temp
#' 
#' @export
#' 
find_closest_date = function(matches, tag_df, doc_split = 500) {
  ## doc split = how to split document; this is the minimum split length.
  ##  - take wordcount, divide by doc_split (roudn down) to get number of splits. then split accordingly...
  
  if (nrow(matches) == 0) {
    return(NULL)
  }
  matches = cbind(matches, Date=NA)
  
  date_inds = which(!is.na(tag_df$date) & (tag_df$paren == 0))
  for(k in seq_along(date_inds)) {
    a = which(is.na(matches$Date) & (matches$Position < date_inds[k]))
    matches$Date[a] = tag_df$date[date_inds[k]]
  }
  if (length(date_inds) > 0) {
    a = which(is.na(matches$Date))
    matches$Date[a] = tag_df$date[max(date_inds)]
  }
  
  ## Compute appropriate splitting
  nsplit = floor(nrow(tag_df) / doc_split)
  portion = nrow(tag_df) / nsplit
  matches$DocSplit = floor(matches$Position / portion) + 1
  return(matches)
}




## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (check_entity_in_fullset)
#' <<BasicInfo>> 
#' 
#' @param entity temp
#' 
#' @return temp
#' 
#' @export
#' 
check_entity_in_fullset = function(entity) {
  names = strsplit(entity, split = " ")[[1]]
  matching_fns = NULL
  for(j in seq_along(names)) {
    mats =     grep(names[j], full_result2$main_name, fixed = TRUE)
    if (is.null(matching_fns)) {
      matching_fns = mats
    } else {
      matching_fns = intersect(mats, matching_fns)
    } 
  }
  return(length(matching_fns))
}

