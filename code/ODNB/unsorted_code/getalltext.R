## saving data
#save(is.checkCO, is.name, is.wordCO, uses, file = "curnums.Rdata")

name.normal = rep("", times = length(ODNB.data))

for(j in which(name.normal == "")) {
  ss = ODNB.names[j]
  if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *, *[-'.[:alpha:]]+ *</span>$", ss)) > 0) {
    name.normal[j] = "L,F" #Two words: Last,First!
  } else if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *, *Sir *[-'.[:alpha:]]+ *</span>$", ss)) > 0) {
    name.normal[j] = "L,SF" #Three words: Last, Sir First!
  } else if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *, *[-'.[:alpha:]]+ *[-'.[:alpha:]]+ *</span>$", ss)) > 0) {
    name.normal[j] = "L,FM" #Three words: Last, First Mid!
  } else if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *[-'.[:alpha:]]+ *, *[-'.[:alpha:]]+ *[-'.[:alpha:]]+ *</span>$", ss)) > 0) {
    name.normal[j] = "LL,FM" #Four words: Last Last, First Mid
  } else if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *, *Sir *[-'.[:alpha:]]+ *[-'.[:alpha:]]+ *</span>$", ss)) > 0) {
    name.normal[j] = "L,SFM" #Three words: Last, Sir First Mid!
  } else if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *, *[-'.[:alpha:]]+ *[-'.[:alpha:]]+ *[-'.[:alpha:]]+ *</span>$", ss)) > 0) {
    name.normal[j] = "L,FMM" #Four words: Last, First Mid Mid
  } else if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *, *Sir *[-'.[:alpha:]]+ *[-'.[:alpha:]]+ *[-'.[:alpha:]]+ *</span>$", ss)) > 0) {
    name.normal[j] = "L,SFMM" #Four words: Last, Sir First Mid Mid
  } else if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *, *[-'.[:alpha:]]+ *[-'.[:alpha:]]+ *[-'.[:alpha:]]+ *[-'.[:alpha:]]+ *</span>$", ss)) > 0) {
    name.normal[j] = "L,FMMM" #Five words: Last, First Mid Mid Mid
  } else if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *</span>$", ss)) > 0) {
    name.normal[j] = "S" #Single Name!
  } else if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *[-'.[:alpha:]]+ *</span>$", ss)) > 0) {
    name.normal[j] = "SS" #DOUBLE Name!
  } else if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *[-'.[:alpha:]]+ *[-'.[:alpha:]]+ *</span>$", ss)) > 0) {
    name.normal[j] = "SSS" #TRIPLE Name!
  } else if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *, *Sir *[-'.[:alpha:]]+ *, *of[ [:alpha:]]+</span>$", ss)) > 0) {
    name.normal[j] = "L,SF,of" #Three words: Last, Sir First, of blah
  } else if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *, *[-'.[:alpha:]]+ *, *of[ [:alpha:]]+</span>$", ss)) > 0) {
    name.normal[j] = "L,F,of" #Two words: Last, First, of blah
  } else if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *, *[-'.[:alpha:]]+ *[-'.[:alpha:]]+ *, *of[ [:alpha:]]+</span>$", ss)) > 0) {
    name.normal[j] = "L,FM,of" #Two words: Last, First Mid, of blah
  } else if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *, *[-'.[:alpha:]]+ *, *the[ [:alpha:]]+</span>$", ss)) > 0) {
    name.normal[j] = "L,F,the" #Two words: Last, First, the blah
  } else if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *, *Sir *[-'.[:alpha:]]+ *, *the[ [:alpha:]]+</span>$", ss)) > 0) {
    name.normal[j] = "L,SF,the" #Three words: Last, Sir First, the blah
  } else if (length(grep("^<span class=\"headword\"> *[-'.[:alpha:]]+ *, *[-'.[:alpha:]]+ *[-'.[:alpha:]]+ *, *the[ [:alpha:]]+</span>$", ss)) > 0) {
    name.normal[j] = "L,FM,the" #Two words: Last, First Mid, the blah
  }
  if(j %% 50 == 0) {
    print(j)
  }
}
sum(name.normal == "")

name.mat = matrix("", nrow = 99996, ncol = 2) 
ss = ODNB.nums[rr]
ww = which(len > 3)
tt = intersect(ss, ww)

save(tt, name.mat, file = "names.tt.Rdata")
for(i in tt) { # This processes L,F
  id = which(ODNB.nums == i)
  temp = ODNB.names[id]
  temp = gsub("<.*?>", "", temp)
  temp = gsub(",", "", temp)
  temp = rev(strsplit(temp, " ")[[1]])
  temp = temp[temp != ""]
  name.mat[i,] = temp
  if (i %% 1000 == 0) { print(i)}
}
process.name(ODNB.names[62], type = "L,SFM")

#name.list = list() initialize only
for (j in 1:58265) {
  test = process.name(name = ODNB.names[j], type = name.normal[j])
  if (!is.null(test)) {
    name.list[[ODNB.nums[j]]] = test
  }
  if(j %% 250 == 0) {
    print(j)
  }
}

process.name <- function(name, type) {
  temp = gsub("<.*?>", "", name)
  if (type == "L,F" | type == "L,SF") {
    temp = gsub(",", " ", temp)
    temp = gsub(" Sir ", " ", temp)
    temp = rev(strsplit(temp, " ")[[1]])
  } else if (type == "L,FM" | type == "L,SFM") {
    temp = gsub(",", " ", temp)
    temp = gsub(" Sir ", " ", temp)
    temp = gsub("^ *", "", temp)
    temp = strsplit(temp," ")[[1]]
    temp = c(temp[-1], temp[1])
  } else if (type == "S" | type == "SS" | type == "SSS") {
    temp = strsplit(temp, " ")[[1]]
  } else {
    return(NULL)
  }
  temp = temp[temp != ""]
  return(temp)
}


is.name = as.numeric(name.normal != "")

for(j in which(name.normal == "")[1:50]) {
  print(ODNB.names[j])
}
for(j in 1:60000) {
  ss = ODNB.data[[j]]$text
  if (gregexpr("<span class=\"headword\">", ss)[[1]][1] != 1) {
    print (j)
  }
  if (j %% 1000 == 0) {
    print(paste("Progress: ", j))
  }
}



### labelling texts with cosubjects
is.checkCO = ### CODE REMOVED: is t/f dep if check.co > 0
is.wordCO = ### CODE REMOVED: is t/f same as exists.cosubject (new function)

############################################
setwd("C:/Users/Lawrence/Desktop/ADAproj")

bb = find.tags(num = 85, which.types = cur.types)

find.names <- function(mat) { ## updated version, is not compatible with old results. 
  ## Input      mat = two row matrix of predictions
  ## Output     matrix, 1st col = names identified; 2nd col = location
  ## 
  ## Finds all identified names and outputs it
  
  starts = which(mat[,2] == "PS")
  ends = which(mat[,2] == "PE")
  
  ## sanity check for tags
  if (length(starts) != length(ends) ) {
    return("ERROR (find.names): not same number of PS, PE")
  } 
  if (length(starts) > 0) {
    num.errs = 0
    for(j in 1:length(starts)) {
      if (starts[j] > ends[j]) {
        num.errs = num.errs + 1
      }
    }
    if (num.errs > 0) {
      return("ERROR (find.names): not all PS/PE are in correct order")
    }
  }
  ## ## ## ## ##
  
  single = which(mat[,2] == "P")
  if (length(single) > 0) {
    toreturn = cbind(mat[single, 1],single)
  } else { toreturn = NULL }
  
  if (length(starts) > 0) {
    temp = matrix(nrow = length(starts), ncol = 2)
    for (i in 1:length(starts)) {
      temp[i,] = c(paste(mat[starts[i]:ends[i], 1], collapse = " "), starts[i])
    }
  } else { temp = NULL }
  
  temp = rbind(toreturn, temp)
  temp[,1] = gsub("[.']s*$", "", temp[,1])
  temp[,1] = gsub("\'$", "", temp[,1])
  temp[,1] = gsub("^ *", "", temp[,1])
  temp[,1] = gsub(" *$", "", temp[,1])
  temp = data.frame(temp[,1], as.numeric(temp[,2]))
  return(temp[order(temp[,2]),])
}

find.names(bb)
n = 34570
bb = find.tags(num = n, which.types = cur.types)
d = find.wordlists(main.person = c("David","Lloyd", "George"), dat.mat = bb)

find.nonletters <- function (vec) {
  vec = gsub("[]';,/)!?:.[& [:alnum:]-]+", "", vec)
  return(paste(vec, collapse = ""))
}

find.wordlists <- function(main.person, dat.mat) { # need to make more general? for diff dat.mat
  ## dat.mat = result of find.tags
  ## types = tells types of tag for each column
  
  cur.found = find.names(dat.mat)
  if (length(find.names(dat.mat)) == 1) {
    return(cur.found)
  }
  # make sure all are letters. 
  if(find.nonletters(cur.found[,1]) != "") {
    print(paste("Non-letters:", find.nonletters(cur.found[,1])))
    return(paste("ERROR: Did not do: non-letters=", find.nonletters(cur.found[,1])))
  }
  
  vec.guess = cur.found[,1]
  guess.loc = cur.found[,2]
  text = dat.mat[,1]
  tags = dat.mat[,2]
  toreturn = analyze.text(main.person = main.person, vec.guess = vec.guess,
                   guess.loc = guess.loc, text = text, tags = tags,
                   regex.words = c("Commonwealth", "£", "Catholic", "Greek", 
                                   "Roman", "Describe", "Treatise"),
                   exact.words = c("Abbey", "House", "Island",
                                   "University", "College", "Cathedral",
                                   "Pamphlet", "Religion", "Country", 
                                   "Act", "Restoration", "Empire",
                                   "Government", "State", "Lawgiving",
                                   "Palace", "Petition", "Parliament", "Death"))
  
  temp = toreturn[[2]]
  counts = matrix(0, nrow = length(temp$Location), ncol = 2)
  for(j in 1:length(temp$Location)) {
    word.len = length(strsplit(temp$Names[j], " ")[[1]])
    counts[j,1] = sum(dat.mat[temp$Location[j]:(temp$Location[j]+word.len-1),3:4] != "") / 2 / word.len
    counts[j,2] = sum(dat.mat[temp$Location[j]:(temp$Location[j]+word.len-1),5:6] != "") / 2 / word.len
  }
  temp = cbind(temp, counts)
  names(temp)[5:6] = c("PER", "ORG")
  toreturn[[2]] = temp
  
  temp1 = toreturn[[1]]
  counts1 = matrix(0, nrow = length(temp1$Name), ncol = 3)
  for(i in 1:length(temp1$Name)) {
    id = temp1$ID[i]
    counts1[i,1] = mean(counts[temp$ID == id,1])
    counts1[i,2] = mean(counts[temp$ID == id,2])
    counts1[i,3] = sum(temp$ID == id)
  }
  temp1 = cbind(temp1,counts1)
  names(temp1)[6:8] = c("PER", "ORG", "Count")
  toreturn[[1]] = temp1
  
  return(toreturn)
}






# text1 = c(strsplit(readLines("true/1.proc.txt"), " "), recursive = TRUE)
# text1 = text1[text1 != ""]
# lp1 = lp.docs[[1]][[2]]
# st1 = st.docs[[1]][[2]]
#get.truetext(1000)
#test1 = fix_tagtext(text = lp1, true.text = text1, type = "LP")
#|       ***********
#|----##replace period with _ --Tue Sep  2 09:43:54 2014--
#test2 = fix_tagtext(text = st1, true.text = text1, type = "ST")
#|       ***********
#|----##replace period with _ --Tue Sep  2 09:43:54 2014--
#test3 = proc_tagtext(text = test1, type = "LP")
#|       ************
#|----##replace period with _ --Tue Sep  2 09:49:17 2014--
#test4 = proc_tagtext(text = test2, tag="LOCATION", type = "ST")
#|       ************
#|----##replace period with _ --Tue Sep  2 09:49:17 2014--

cur.types = matrix(c("ST", "LP", "ST", "LP", "PERSON", "PERSON", "ORGANIZATION", 
                     "ORGANIZATION"), nrow = 4)




which(ODNB.nums[which(uses)] == 3239)

### Get tags

uses = (is.checkCO + is.wordCO + is.name == 3)
save(uses, ODNB.nums, file="nums.Rdata")



