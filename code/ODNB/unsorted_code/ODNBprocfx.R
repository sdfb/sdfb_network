
find.err.rates <- function(num) {
  ## Input      File number
  ## Output     num matrix
  ## 
  ## Takes a file number, and outputs error rates. 
  
  man.fn = ".mtag.txt"
  sta.fn = ".s7tag.txt"
  lin.fn = ".lptag.txt"
  toreturn = matrix(0, ncol = 5, nrow = 10)
  
  true.tag = proc.person(readLines(paste("adatagged/",num, man.fn, sep = "")))
  sta.txt = readLines(paste("adatagged/",num, sta.fn, sep = ""))
  lin.txt = proc.lingpipe(readLines(paste("adatagged/",num, lin.fn, sep = "")))
  lin.txt = vectorize.lingpipe(combine.punct(lin.txt), true.text = true.tag[1,])
  
  colnames(toreturn) = c("Total Names", "Correct", "Partial", "Missed", "Extra")
  rownames(toreturn) = c("St: PERSON", "St: ORG", "St: LOC", 
                         "LP: PERSON", "LP: ORG", "LP: LOC", 
                         "SL: PERSON", "St: P/O", "LP: P/O", "SL:P/O")
  
  sta.tag.p = proc.person(sta.txt)
  toreturn[1,] = error.rate(true.tag[2,], sta.tag.p[2,])
  sta.tag.o = proc.person(sta.txt, "ORGANIZATION")
  toreturn[2,] = error.rate(true.tag[2,], sta.tag.o[2,])
  sta.tag.l = proc.person(sta.txt, "LOCATION")
  toreturn[3,] = error.rate(true.tag[2,], sta.tag.l[2,])
  
  lin.tag.p = proc.enamex(lin.txt)
  toreturn[4,] = error.rate(true.tag[2,], lin.tag.p[2,])
  lin.tag.o = proc.enamex(lin.txt, "ORGANIZATION")
  toreturn[5,] = error.rate(true.tag[2,], lin.tag.o[2,])
  lin.tag.l = proc.enamex(lin.txt, "LOCATION")
  toreturn[6,] = error.rate(true.tag[2,], lin.tag.l[2,])
  
  SL.p = combine_two(sta.tag.p[2,], lin.tag.p[2,])
#|       ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--
  S.po = combine_two(sta.tag.p[2,], sta.tag.o[2,])
#|       ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--
  L.po = combine_two(lin.tag.p[2,], lin.tag.o[2,])
#|       ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--
  SL.po = combine_two(S.po, L.po)
#|        ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--
  toreturn[7,] = error.rate(true.tag[2,], SL.p)
  toreturn[8,] = error.rate(true.tag[2,], S.po)
  toreturn[9,] = error.rate(true.tag[2,], L.po)
  toreturn[10,] = error.rate(true.tag[2,], SL.po)
  
  return(toreturn)
}






#########################################################

#########################################################

#########################################################
DONT NEED BELOW THIS (I THINK)
#########################################################

#########################################################

#########################################################


error.rate <- function(tr, pr) {
  ## Input      tr = char vec ("P",etc.) [TRUE TAGS]
  ##            pr = char vec ("P",etc.) [PREDICTED TAGS]
  ## Output     num vec
  ## 
  ## Outputs 5 numbers: 1. Total true tags 
  ## 2. Number true ID's. 3. Number partial ID's.
  ## 4. Number missed ID's [1-2]. 5. Number ID's not names. 
  which.ids <- function(vec) { ## OBSELETE... don't need this, since there are "P*'s"
    ## Input      vec = char vec (consisting of "P", "PS", "PE")
    ## Output     num vec 
    ## 
    ## Returns the coordinate numbers which are names 
    
    toret = rep(0, times = length(vec))
    loc = 1
    in.word = FALSE
    for (i in 1:length(vec)) {
      if (vec[i] == "P") { 
        toret[loc] = i
        loc = loc + 1
      } else if (vec[i] == "PS") {
        toret[loc] = i
        loc = loc + 1
        in.word = TRUE
      } else if (vec[i] == "PE") {
        toret[loc] = i
        loc = loc + 1
        in.word = FALSE
      } else if (in.word) {
        toret[loc] = i
        loc = loc + 1
      } 
    }
    return(toret[1:(loc-1)])
  }
  
  
  num.true = sum(tr == "P") + sum(tr == "PS")
  
  num.true.id = 0
  num.true.part = 0
  num.true.miss = 0
  num.false.id = 0
  
  tr.left = which.ids(tr)
  pr.left = which.ids(pr)
  
  j = which(pr == "P")
  if (length(j) > 0) {
    for(i in 1:length(j)) {
      if (tr[j[i]] == "P") { 
        num.true.id = num.true.id + 1
        tr.left = tr.left[tr.left != j[i]]
        pr.left = pr.left[pr.left != j[i]]
      } else if (sum(tr.left == j[i]) > 0) { 
        num.true.part = num.true.part + .5
        pr.left = pr.left[pr.left != j[i]]
      } else {
        num.false.id = num.false.id + 1
        pr.left = pr.left[pr.left != j[i]]
      }
    }
  }
  
  k = which(pr == "PS")
  l = which(pr == "PE")
  if (length(k) > 0) {
    for(i in 1:length(k)) {
      if (tr[k[i]] == "PS" & tr[l[i]] == "PE") {
        num.true.id = num.true.id + 1
        tr.left = tr.left[-which(tr.left > k[i]-1 & tr.left < l[i]+1)]
        pr.left = pr.left[-which(pr.left > k[i]-1 & pr.left < l[i]+1)]
      } else if (sum(tr.left == k[i]) + sum(tr.left == l[i]) > 0) { 
        num.true.part = num.true.part + 0.5 * (sum(tr.left == k[i]) + sum(tr.left == l[i]))
        pr.left = pr.left[-which(pr.left > k[i]-1 & pr.left < l[i]+1)]
        tr.left = tr.left[-which(tr.left > k[i]-1 & tr.left < l[i]+1)]
      } else { 
        num.false.id = num.false.id + 1
        pr.left = pr.left[-which(pr.left > k[i]-1 & pr.left < l[i]+1)]
      }
    } 
  }
  
  num.true.miss = sum(tr[tr.left] == "P") + sum(tr[tr.left] == "PS")
  return(c(num.true, num.true.id, num.true.part, num.true.miss, num.false.id))
}


### New functions:  ---- dont need
get.truetext <- function(num) { #unneeded?
  ## This loads/fixes true.text (splits into one long vector)
  toreturn = c(strsplit(true.docs[[recordt[num]]][[2]], " "), recursive = TRUE)
  toreturn = gsub("£", "?", toreturn) ## pound symbol... convert to ?
  return(toreturn[toreturn != ""])
}


find.tags <- function(num, which.types) { ######### DONT NEED EXCEPT TO COMBINE TWO COLUMNS. 
  ## This function has bad form, but this is to avoid passing in tons of data. 
  ## This requires recordl, records, st.docs, lp.docs to exist. 
  ## num is just #
  ## which.types should be a 2 column matrix, 1st col = LP/ST, 2nd col = tags, 
  ##   in order of precedence for combine.types

  true = get.truetext(num)
  check = (lp.docs[[recordl[num]]][[1]] == st.docs[[records[num]]][[1]])
  if (!check) {
    print("ERROR in (find.tags): LP doc number, ST doc number dont match")
  }
  lp.text = fix_tagtext(text = lp.docs[[recordl[num]]][[2]], true.text = true, type = "LP")
#|          ***********
#|----##replace period with _ --Tue Sep  2 09:43:54 2014--
  st.text = fix_tagtext(text = st.docs[[records[num]]][[2]], true.text = true, type = "ST")
#|          ***********
#|----##replace period with _ --Tue Sep  2 09:43:54 2014--
  
  if (is.null(lp.text) | is.null(st.text)) {
    return(NULL)
  }

  comb.tags = rep("", times = length(true))
  toreturn = cbind(true, comb.tags)
  NN = dim(which.types)[1]
  name = rep("", times = NN)
  for(i in 1:NN) {
    typ = which.types[i,1]
    if (typ == "LP") {
      tags = proc_tagtext(text = lp.text, tag = which.types[i,2], type = typ)
#|           ************
#|----##replace period with _ --Tue Sep  2 09:49:17 2014--
    } else if (typ == "ST") {
      tags = proc_tagtext(text = st.text, tag = which.types[i,2], type = typ)
#|           ************
#|----##replace period with _ --Tue Sep  2 09:49:17 2014--
    }
    name[i] = paste(which.types[i,1], 
                    substr(which.types[i,2],1,min(3, nchar(which.types[i,2]))), sep = "")

    comb.tags = combine_two(comb.tags, tags)
#|              ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--
    if (is.null(comb.tags)) {
      return("ERROR in combine_two (maybe LP error?)")
#|                     ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--
    }
    toreturn = cbind(toreturn, tags)
    print(i)
  }
  colnames(toreturn) = c("True", "Combined", name)
  toreturn[,2] = comb.tags
  
  return(toreturn)
}

############## Recording lpfile   ## dont need eventually
reload.lpfile <- function(num, lp.docs, folder) {
  if (is.na(recordl[num])) {
    print("ERROR: no list entry yet.")
    return(NULL)
  } else if (lp.docs[[recordl[num]]][[1]] != num) {
    print("ERROR: lp doc entry mismatch number")
    return(NULL)
  } else {
    doc = readLines(paste("lptag/tagdocs",folder,"/",num,".proc.txt", sep = ""))
    lp.docs[[recordl[num]]][[2]] = doc
    return(lp.docs) 
  }
}
 lp.docs = reload.lpfile(num = 150, lp.docs = lp.docs, folder = 1)
 save(lp.docs, recordl, file="lpdocs.Rdata")
###########

which.ids <- function(vec) { ## OBSELETE... don't need this, since there are "P*'s"
  ## Input      vec = char vec (consisting of "P", "PS", "PE")
  ## Output     num vec 
  ## 
  ## Returns the coordinate numbers which are names 
  
  toret = rep(0, times = length(vec))
  loc = 1
  in.word = FALSE
  for (i in 1:length(vec)) {
    if (vec[i] == "P") { 
      toret[loc] = i
      loc = loc + 1
    } else if (vec[i] == "PS") {
      toret[loc] = i
      loc = loc + 1
      in.word = TRUE
    } else if (vec[i] == "PE") {
      toret[loc] = i
      loc = loc + 1
      in.word = FALSE
    } else if (in.word) {
      toret[loc] = i
      loc = loc + 1
    } 
  }
  return(toret[1:(loc-1)])
}

