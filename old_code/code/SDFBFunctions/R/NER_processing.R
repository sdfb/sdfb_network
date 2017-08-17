## this file contains functions to clean up the NER extracted data. 


## Split truedocs into words

#' This function does the following:
#' 1. add spaces before punctuation
#' 2. add spaces before/after (), [], {}, as to separate them off => easier to count
#' 3. remove more than one space in a row.
#' 4. Tokenize (split into a word-by-word representation).
#' 4a. Fix problems with ;. 
#' 
#' @param text character vector
#' 
#' @return formatted text
#' 
#' @export
#' 
format_text_and_split = function(text) {
  ## Collapse text
  temp = paste(text, collapse = " ")
  
  
  ## Add spaces before/after punctuation if needed.
#   puncts = c("[.]", ",", ":", ";", "!", "[?]", "`")
  
  ## add spaces before/after; will remove extra ones later
  puncts = c(".", ",", ":", "!", "?", "`", ";", "(", ")", "{", "}", "[", "]")
  for(t in puncts) {
    temp = gsub(t, paste(" ",t," ", sep = ""), temp, fixed = TRUE)
  }
  
  ## Remove multiple spaces
  temp = gsub(" +", " ", temp)
  
  ## Split text on spaces
  text_split = strsplit(temp, split = " ")[[1]]
  
  ## Fix semicolon problems
  semics = which(text_split == ";")
  semics = semics[semics > 1] ## Cannot be problem as first item
  
  semic_probs = grep("^&",text_split[semics - 1])
  if (length(semic_probs) > 0) {
    print("Fixing semicolon problems at:")
    print(semics[semic_probs])
    
    ind = semics[semic_probs] - 1
    text_split[ind] = paste(text_split[ind], ";", sep = "")
    text_split = text_split[-semics[semic_probs]]
  }
  
  ## return results
  return(text_split)
}



#' this pastes together two consecutive entries in a vector
#' starting with ind.start and then ind.start+1
#' and edits the vector to remove the extra cell
#' 
#' @param vec vector
#' @param ind.start where to paste
#' 
#' @return updated vector
#' 
#' @export
#' 
paste_two = function(vec,ind.start) {
  vec[ind.start] = paste(vec[ind.start], vec[ind.start+1], sep = "")
  return(vec[-(ind.start+1)])
}


#' This vectorizes and normalizes the tagged text by standardizing the tags to have the same format between taggers. 
#' 
#' @param text tagged text
#' @param true.text raw text
#' @param type "ST" or "LP"
#' 
#' @return normalized form of tagged text 
#' 
#' @export
#' 
normalize_tagtext = function(text, true.text, type) { 
  #   text = gsub("£", "?", text) ## pound symbol... convert to ?
  #   text = gsub("&amp;", "&", text)
  
  if (type == "ST") {    
    # do nothing
  } else if (type == "LP") {
    ## Single-words ENAMEX, fixes ' as organization issue. 
    text = gsub("<ENAMEX TYPE=\"ORGANIZATION\">'</ENAMEX>","\'", text)
    text = gsub("ENAMEX TYPE=", "ENAMEXTYPE=", text)
    text = gsub("&amp;", "&", text)
    text = gsub("&quot;", "\"", text)
    text = gsub("</*?s.*?>", "", text)
  } else {
    print("Error in (fix_tagtext): Invalid TYPE")
#|                   ***********
#|----##replace period with _ --Tue Sep  2 09:43:54 2014--
    return("Error in (fix_tagtext): Invalid TYPE")
#|                    ***********
#|----##replace period with _ --Tue Sep  2 09:43:54 2014--
  }
  
  words = format_text_and_split(text)
  
  to.test = gsub("<.*?>", "", words) # removing all <>'s , comparing spacing
  j = 1
  N = length(true.text)
  while(j <= N) {
    if (true.text[j] == to.test[j]) {
      j = j+1
    } else {
      if (length(grep(paste(to.test[j],to.test[j+1], sep = ""),
                      true.text[j], fixed = TRUE)) > 0) {
        to.test = paste_two(to.test, j)
        words = paste_two(words, j)
      } else {
        print(paste("ERROR... temp: j=",j))
        return("ERROR.")
      }
    }
  }
  while(length(words) > N) {
    words = paste_two(words, N)
  }
  
  return(words)
}


#' returns vectors of tags matching tag. PERSON, ORGANIZATION, LOCATION 
#' 
#' @param tagged.text vectorized tagged text
#' @param type ST or LP
#' @param tag.types  existing tag.types
#' 
#' @return fix this
#' 
#' @export
#' 
proc_tagtext = function(tagged.text, type,
                         tag.types = c("PERSON", "ORGANIZATION", "LOCATION")) {
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
  
  ## Fix cases where the end is before first beginning
  min_end = min(which(non.zero.end))
  min_beg = min(which(non.zero.begin))
  if (is.infinite(min_end) | is.infinite(min_beg)) {
    ## No matches found ..
    return(toreturn)
  }
  
  if (min_end < min_beg) {
    end[[min_end]][1] = -1
  } else if (min_end == min_beg) {
    if (end[[min_end]][1] < begin[[min_beg]][1]) {
      if (length(end[[min_end]]) == 1) {
        end[[min_end]][1] = -1
      } else {
        atr = attr(end[[min_end]], "match.length")[-1]
        end[[min_end]] = end[[min_end]][-1]
        attr(end[[min_end]], "match.length") <- atr
      }
    }
  }
  
  ## Fix cases where the last beginning has no end
  max_end = max(which(non.zero.end))
  max_beg = max(which(non.zero.begin))
  if (max_beg > max_end) {
    begin[[max_beg]][1] = -1
  } else if (max_beg == max_end) {
    if (begin[[max_beg]][length(begin[[max_beg]])] > end[[max_end]][length(end[[max_end]])]) {
      if (length(begin[[max_beg]]) == 1) {
        begin[[max_beg]][1] = -1
      } else {
        mb = length(begin[[max_beg]])
        atr = attr(begin[[max_beg]], "match.length")[-mb]
        begin[[max_beg]] = begin[[max_beg]][-mb]
        attr(begin[[max_beg]], "match.length") <- atr
      }
    }
  }
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


## TODO: [Documentation-AUTO] Check/fix Roxygen2 Documentation (tag_dates)
#' Finds dates between 1200-1900
#' 
#' @param vec text
#' @param mindate temp
#' @param maxdate temp
#' 
#' @return vector with dates extracted...
#' 
#' @export
#' 
tag_dates = function(vec, mindate = 1100, maxdate = 1900) {
  numbs = as.numeric(vec)
  ids = which(numbs >= mindate & numbs <= maxdate)
  res = rep(NA, times = length(vec))
  res[ids] = numbs[ids]
  return(res)
}


#' This function computes the level of parenthesis embedding
#' 
#' @param vec text
#' 
#' @return vector of parenthesis embedding
#' 
#' @export
#' 
compute_parenthesis = function(vec) {
  starts = grep("[{([]", vec)
  ends = grep("[]})]", vec)
  res = rep(0, times = length(vec))
  L = length(vec)
  
  if (length(starts) != length(ends)) {
    print("Mismatched parenthesis")
  }
  
  for(j in starts) {
    res[j:L] = res[j:L] + 1
  }
  for(j in ends) {
    m = min(c(j+1, L))
    res[m:L] = res[m:L] - 1
  }
  return(res)
}


#' Takes two predictor vectors and outputs a combined predictor vector, giving priority to vector a. 
#' 
#' @param a tagged output
#' @param b tagged output
#' 
#' @return combined tagged output
#' 
#' @export
#' 
combine_two = function(a,b) {
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
