### Old code that is outdated






##############################
##############################
############## OLD FUNCTIONS that have been upgraded (so these can be 
deleted...)
proc.lingpipe <- function(text) {
  ## Input      text = char vec (lines read from lingpipe processed text)
  ## Output     char vec
  ## 
  ## Removes all <s>, </s> tags from text and removes empty lines. 
  text = gsub("<[?]xml.*?>", "", text)
  text = gsub("</*output>", "", text)
  text = gsub("<s.*?>", "", text)
  text = gsub("</s>", "", text)
  return(text[text != ""])
}

combine.punct <- function(vec.text) {
  ## Input      vec.text = char vec (output of proc.lingpipe)
  ## Output     char vec 
  ## 
  ## Fixes ' as organization, combines ENAMEX TYPE. 
  
  # a = grep("^[^[:alnum:]<]", vec.text)
  # b = grep("^'[0-9A-RT-Za-rt-z]", vec.text)
  # if (length(b) > 0) {
  #  for(j in 1:length(b)) {
  #   a = a[a != b[j]]
  #  }
  # }
  # for(i in rev(a)) {
  #  vec.text[i-1] = paste(vec.text[i-1],vec.text[i], sep = "")
  # }
  # vec.text = vec.text[-a]
  vec.text = gsub("<ENAMEX TYPE=\"ORGANIZATION\">'</ENAMEX>","\'", 
                  vec.text)
  vec.text = gsub("ENAMEX TYPE=", "ENAMEXTYPE=", vec.text)
  return(vec.text)
}

vectorize.lingpipe <- function(vec.text, true.text) {
  ## Input      vec.text = char vec (output of combine.punct)
  ##            true.text = char vec (words: [1,] output of proc.person)
  ## Output     tagged version of true char vec
  ## 
  ## Returns lingpipe-tagged text, word-separated properly
  
  words = strsplit(vec.text, " ")
  words = c(words, recursive = TRUE)
  to.test = gsub("<.*?>", "", words)
  for(i in 1:length(true.text)) {
    #print(i)
    if (to.test[i] == true.text[i]) {
    } else {
      #   print(to.test[i])
      #   print(words[i])
      #   print(true.text[i])
      #   return()
      one = paste(to.test[i], to.test[i+1], sep = "")
      words[i] = paste(words[i], words[i+1], sep = "")
      words = words[-(i+1)]
      to.test[i] = one
      to.test = to.test[-(i+1)]
      i = i-1
    } 
  }
  return(words)
}

proc.enamex <- function(text, tag = "PERSON") { 
  ## Input      text = char vec (words: output of vectorize.lingpipe)
  ##            tag = which tag to look for
  ## Output     char matrix: 1st row = true text
  ##                         2nd row = "", "P", "PS", "PE", "P*"
  ## 
  ## Processes lingpipe text
  
  words = text
  toreturn = rbind(words, "")
  begin = grep(paste("<ENAMEXTYPE=\"",tag,"\">", sep = ""), words)
  end = grep(paste("</ENAMEX>", sep = ""), words) 
  for(i in 1:length(begin)) {
    cur = end[end >= begin[i]][1]
    if (begin[i] == cur) {
      toreturn[2,begin[i]] = "P"
    } else { 
      toreturn[2,begin[i]] = "PS"
      toreturn[2,cur] = "PE"
      if (cur-begin[i] > 1) {
        toreturn[2,(begin[i]+1):(cur-1)] = "P*"
      }
    }
  } 
  toreturn[1,] = gsub("<.*?>", "", toreturn[1,])
  return(toreturn)
}

proc.person <- function(text, tag = "PERSON") {
  ## Input      text = char vec (sentence text from file)
  ##            tag = which tag to look for
  ## Output     char matrix: 1st row = true text
  ##                         2nd row = "", "P", "PS", "PE"
  ## 
  ##
  
  words = strsplit(text, " ")
  words = c(words, recursive = TRUE)
  words = words[!(words == "")]
  toreturn = rbind(words, "")
  begin = grep(paste("<",tag,">", sep = ""), words)
  end = grep(paste("</",tag,">", sep = ""), words) 
  if (length(begin) > 0) {
    for(i in 1:length(begin)) {
      if (begin[i] == end[i]) {
        toreturn[2,begin[i]] = "P"
      } else { 
        toreturn[2,begin[i]] = "PS"
        toreturn[2,end[i]] = "PE"
        if (end[i]-begin[i] > 1) {
          toreturn[2,(begin[i]+1):(end[i]-1)] = "P*"
        }
      }
    } 
  }
  toreturn[1,] = gsub("<.*?>", "", toreturn[1,])
  return(toreturn)
}
