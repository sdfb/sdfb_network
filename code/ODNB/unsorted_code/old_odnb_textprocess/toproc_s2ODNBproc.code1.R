### Code to convert from raw text into ODNBdata.Rdata
## This code reads from text files into ODNBdata.Rdata. No need to run this again

##################################################
#### 1. OLD CODE FOR METADATA EXTRACTION
##################################################

## Figuring out ODNB.names (not ideal code; should be replaced eventually)
ODNB.names = rep("", times = length(ODNB.data))
for(j in 1:length(ODNB.data)) {
  ss = ODNB.data[[j]]$text[1]
  st = gregexpr("<span", ss)[[1]]
  en = gregexpr("</span>", ss)[[1]]
  
  embed = 1
  st.ed = st[st != 1]
  en.ed = en
  while (embed > 0) {
    small = min(c(st.ed, en.ed))
    if (sum(st.ed == small) == 1) {
      embed = embed + 1
      st.ed = st.ed[st.ed != small]
    } else if (sum(en.ed == small) == 1) {
      embed = embed - 1
      en.ed = en.ed[en.ed != small]
    } else {
      print ("ERROR")
    }
  }
  ODNB.names[j] = substr(ss, start = 1, stop = small + 6)
  if(j %% 100 == 0) {
    print(j)
  }
}


fix.accents <- function(text) {
  for(j in 1:length(text)) {
    temp = text[j]
    temp = gsub("&AElig;", "AE", temp)
    temp = gsub("&aelig;", "ae", temp)
    temp = gsub("&eacute;", "e", temp)
    temp = gsub("&Aacute;", "A", temp)
    temp = gsub("&aacute;", "a", temp)
    temp = gsub("&atilde;", "a", temp)
    temp = gsub("&acirc;", "a", temp)
    temp = gsub("&Agrave;", "A", temp)
    temp = gsub("&ccedil;", "c", temp)
    temp = gsub("&ocirc;", "o", temp)
    temp = gsub("&oacute;", "o", temp)
    temp = gsub("&Eacute;", "E", temp)
    temp = gsub("&ecirc;", "e", temp)
    temp = gsub("&uacute;", "u", temp)
    temp = gsub("&#150;", "-", temp)
    temp = gsub("[()]", "", temp)
    temp = gsub("&egrave;", "e", temp)
    temp = gsub("&uuml;", "u", temp)
    temp = gsub("&euml;", "e", temp)
    temp = gsub("<span class=\"st\">.*?</span>", "", temp)
    text[j] = temp
  }
  return(text)
}

ODNB.names = fix.accents(ODNB.names)

## Obtaining ODNB.nums
ODNB.nums = rep(0, times = length(ODNB.data))
for(i in 1:length(ODNB.nums)) {
  ODNB.nums[i] = ODNB.data[[i]]$num
}

## Figuring out which documents involve cosubjects
NN = length(ODNB.data) # data size
ODNB.cosubject = rep(FALSE, times = NN)
for(i in 1:NN) {
  if (exists.cosubject(ODNB.data[[i]]$text)) {
    ODNB.cosubject[i] = TRUE
  }
  if (i %% 1000 == 0) {print(i)}
}

## computing ODNB.nums.inv
ODNB.nums.inv = rep(0, times = 99999)
for(i in 1:NN) {
  ODNB.nums.inv[ODNB.nums[i]] = i
  if (i %% 1000 == 0) {print(i)}
}

## Updating ODNB.names

save(ODNB.data, ODNB.cosubject, ODNB.names, ODNB.nums, ODNB.nums.inv,
     file = "ODNBdatav1.Rdata")


###############################
######################## REMOVE \/ \/
###############################

##################################################
#### 5. combining results
##################################################


# Make sure formatting is preserved. 
# Stanford
for(k in 1:500) {
  tempa = readLines(paste(k, ".txt", sep = ""))
  tempb = readLines(paste(k,".st.txt\r", sep = ""))
  if (length(tempa) != length(tempb)) { print(paste("mismatching lengths in",k))}
  if (k %% 10 == 0) {print (k)}
}



# separate true document word by word.
# connect single punctuation to previous word
# remove single spaces.
load("truedocs.Rdata")

true.docs.word = list()
for(i in which(exists.truedoc)) {
  temp = strsplit(true.docs[[i]], " ")
  temp = c(temp, recursive = TRUE)
  single.punct = grep("^[.,:;!?'`]$", temp)
  temp = temp[temp != ""]
  for(j in single.punct) {
    temp[j-1] = paste(temp[j-1], temp[j], sep = "")
    temp[j] = ""
  }
  temp = temp[temp != ""]
  true.docs.word[[i]] = temp
  if (i %% 50 == 0) { print(i) }
}

save(exists.truedoc, true.docs, true.docs.word, file = "truedocsv2.Rdata")
# 9/3 3:16 AM




###############################
######################## REMOVE /\ /\
###############################



#lingpipe
# compiling existing lengths
true.len = rep(0, times = 500)
# these two matrices are not completely filled in row 500. 
true.emptypos = matrix(NA, nrow = 500, ncol = 400)
true.docnums = matrix(NA, nrow = 500, ncol = 100)

for(k in 1:500) {
  tempa = readLines(paste(k, ".txt", sep = ""))
  true.len[k] = length(tempa)
  print(paste("file",k,"has",length(which(tempa == "This is an empty sentence")))) 
  mats = which(tempa == "This is an empty sentence")
  true.emptypos[k,1:length(mats)] = mats
  for(j in 1:(length(mats)/4)) {
    startpos = true.emptypos[k,1+(4*(j-1))]
    remaining = gsub("This is the start of document number ", "", tempa[startpos+1])
    true.docnums[k,j] = as.numeric(remaining)
  }
}
save(true.len, true.emptypos, true.docnums, file= "true.len.Rdata")
# 9/2 4:00 PM

for(k in 1:500) {
  tempa = readLines(paste(k, ".txt", sep = ""))
  if (length(tempa) != true.len[k] + 1) { # + 1 for lingpipe, +0 for stanford
    print(paste("mismatching lengths in",k))
  }
  if (k %% 10 == 0) {print (k)}
  #if (length(which(nchar(temp) == 25)) != 400) { print(k) } 
} #works ok for both; no need to rerun. 

# Processing stanford docs
st.docs = list()
for(k in 1:500) {
  print(k)
  tempa = readLines(paste(k,".st.txt\r", sep = ""))
  for(j in 1:sum(!is.na(true.docnums[k,]))) {
    tempb = tempa[(true.emptypos[k,(4*j-2)]+2):(true.emptypos[k,(4*j-1)]-2)]
    if (sum(nchar(tempa[true.emptypos[k,(4*j - 3):(4*j)]]) == 25) == 4) {
      st.docs[[true.docnums[k,j]]] = tempb
    } else {
      print(paste("[",k,",",j,"]"))
      match.greg = gregexpr("<.*>", tempa[true.emptypos[k,(4*j - 1)]])[[1]]
      match.toappend = substr(tempa[true.emptypos[k,(4*j - 1)]], match.greg, 
                              (match.greg+attr(match.greg, "match.length") - 1))
      tempb[length(tempb)] = paste(tempb[length(tempb)], match.toappend)
      st.docs[[true.docnums[k,j]]] = tempb
    }
  }
}

#checking results (right number of brackets <>'s)
sapply(st.docs, length) -> doc.lines

helper = function(text) {return(gregexpr("<.+?>", text))}
lapply(st.docs, helper) -> greg.loc.all
helper2 = function(text) {return(gregexpr("</.+?>", text))}
lapply(st.docs, helper2) -> greg.loc.ends

save(st.docs, doc.lines, greg.loc.all, greg.loc.ends, file = "stancheck.Rdata")
# 9/2 4:05 PM

lengthpos = function(x) {sum(x > 0)}
for(i in 1:99996) {
  if (length(st.docs[[i]]) > 0) {
    total = sum(sapply(greg.loc.all[[i]], lengthpos))
    ends = sum(sapply(greg.loc.ends[[i]], lengthpos))
    if (ends*2 != total) {print(paste("doc",i,"has error"))}
  }
  if (i %% 5000 == 0) { print(i) }
}
# okay for stanford. 


# Processing lingpipe docs
load("true.len.Rdata")
lp.docs = list()
for(k in 1:500) {
  print(k)
  tempa = readLines(paste(k,".txt", sep = ""))
  tempa = gsub("</*s.*?>", "", tempa)
#  if (all(which(tempa == "This is an empty sentence") == 
#    true.emptypos[k,1:sum(!is.na(true.emptypos[k,]))])) {
    for(j in 1:sum(!is.na(true.docnums[k,]))) {
      tempb = tempa[(true.emptypos[k,(4*j-2)]+2):(true.emptypos[k,(4*j-1)]-2)]
      if (sum(nchar(tempa[true.emptypos[k,(4*j - 3):(4*j)]]) == 25) == 4) {
        lp.docs[[true.docnums[k,j]]] = tempb
      } else {
        print(paste("[",k,",",j,"]"))
        index = which(nchar(tempa[true.emptypos[k,(4*j - 3):(4*j)]]) != 25)
        if (index == 3) {
          match.greg = gregexpr("<.*>", tempa[true.emptypos[k,(4*j - 1)]])[[1]]
          match.toappend = substr(tempa[true.emptypos[k,(4*j - 1)]], match.greg, 
                                  (match.greg+attr(match.greg, "match.length") - 1))
          tempb[length(tempb)] = paste(tempb[length(tempb)], match.toappend)
        } else if (index == 2) {
          match.greg = gregexpr("<.*>", tempa[true.emptypos[k,(4*j - 2)]])[[1]]
          match.toappend = substr(tempa[true.emptypos[k,(4*j - 2)]], match.greg, 
                                  (match.greg+attr(match.greg, "match.length") - 1))
          tempb[1] = paste(match.toappend, tempb[1])
        }
      }
      lp.docs[[true.docnums[k,j]]] = tempb
    } 
#  } else {
#    print(paste("large-file error in file", k))
#  }
}

#checking results (right number of brackets <>'s)
sapply(lp.docs, length) -> doc.lines

helper = function(text) {return(gregexpr("<.+?>", text))}
lapply(lp.docs, helper) -> greg.loc.all
helper2 = function(text) {return(gregexpr("</.+?>", text))}
lapply(lp.docs, helper2) -> greg.loc.ends

save(lp.docs, doc.lines, greg.loc.all, greg.loc.ends, file = "lpcheck.Rdata")


lengthpos = function(x) {sum(x > 0)}
for(i in 1:99996) {
  if (length(lp.docs[[i]]) > 0) {
    total = sum(sapply(greg.loc.all[[i]], lengthpos))
    ends = sum(sapply(greg.loc.ends[[i]], lengthpos))
    if (ends*2 != total) {print(paste("doc",i,"has error"))}
  }
  if (i %% 5000 == 0) { print(i) }
}
# okay for lingpipe
# 9/3 12:52 AM
