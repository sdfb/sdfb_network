##@S This contains very old code, done at the very beginning of the project. Experiments were done on only a small subset of documents (60 documents), and the ideas used here are not really applicable on data of the scale that we need... 



#(ada start.R)

#setwd("C:/Users/Lawrence/Desktop")

## Getting the 59 document indices. 
samp.html = readLines("subsample.html")
indices = grep(pattern = "<a target", x = samp.html)
to.scan = samp.html[indices]
a = regexpr(pattern = "/[0-9]+/?", to.scan)
text.ind = sort(as.numeric(substr(to.scan, a+1, a+attr(a, "match.length")-1)))

## Reading the docs
filenames = paste("ada/file_",text.ind,".txt", sep = "")
html.files = list()
for(i in 1:length(filenames)) {
  html.files = c(html.files, list(readLines(filenames[i])))
}

## First 54 files: 
#rm.spaces <- function(text) {   # removes lines of text that have only spaces
# a = grep(pattern = "^[[:space:]]*$", text)
# return(text[-a])
#}
#
#dnb.grab.main <- function(artic) {   # keeps only the body to check
# temp1 = rm.spaces(artic)
# t1 = min(grep(pattern = "<div class=\"para\">" ,temp1))
# t5 = grep(pattern = "<div id=\"references\">" , temp1)
# return(temp1[(t1+1):(t5 -1)])
#}

text.toproc = list()
for(i in 1:length(filenames)) {
  text.toproc = c(text.toproc, list(dnb.grab.main(html.files[[i]])))
}
gregexpr(pattern = "hi", text = "obaiowehiasdfaweiofahi")

dnb.grab.name <- function(art.main) {
  matches = gregexpr(pattern = "<span class=\"headword\">.+<span class=\"occ\">",  text = art.main)
  toreturn = ""
  for(i in 1:length(art.main)) {
    posit = matches[[i]]
    index = attr(matches[[i]], "match.length")
    if(posit[1] > 0) {
      for(j in 1:length(posit)) {
        temp.sub = substr(art.main[i], start = posit[j], stop = posit[j] + index[j] - 1)
        t1 = gsub(pattern = "\\(.*?\\)", replacement = "", x = temp.sub)
        t2 = gsub(pattern = "<.*?>", replacement = "#", x = t1)
        t3 = gsub(pattern = " *, +", replacement = ",", x = t2)
        t4 = gsub(pattern = " +", replacement = " ", x = t3)
        t5 = gsub(pattern = "#,#", replacement = "#", x = t4)
        t6 = gsub(pattern = " *#+ *", replacement = "#", x = t5)
        t7 = gsub(pattern = "##+", replacement = "#", x = t6)
        toreturn = c(toreturn, t7)
      }
    }
  }
  return(toreturn[-1])
}
returned = lapply(text.toproc, FUN = dnb.grab.name)
list.collapse.top <- function(lis) {
  toreturn = ""
  for(i in 1:length(lis)) {
    toreturn = c(toreturn,lis[[i]][1])
  }
  return(toreturn[-1])
}
coll = list.collapse.top(returned)
regexpr(pattern = "\\[", text = "oawejh[oaw")


find.names <- function(vect) {
  toreturn = matrix("", ncol = 7, nrow = length(vect))
  for(i in 1:length(vect)) {   ### Standard #Last,First#
    b = gregexpr(pattern = "#[[:alpha:]]+ *[[:alpha:]]*,[[:alpha:]]+ *[[:alpha:]]*#", text = vect[i])[[1]]
    if(b[1] > 0) {
      t1 = substr(vect[i], b[1]+1, b[1] + attr(b, "match.length")[1] - 2)
      loc = regexpr(pattern = ",", text = t1)
      last.name = substr(t1, 1, loc - 1)
      first.name = substr(t1, loc + 1, stop = nchar(t1))
      toreturn[i,1:2] = c(first.name,last.name)
      vect[i] = sub(pattern = "#[[:alpha:]]+ *[[:alpha:]]*,[[:alpha:]]+ *[[:alpha:]]*#", replacement = "#", x = vect[i])
    }
  }
  for(i in 1:length(vect)) {   ### # Last#[Last.Alt]#,First#
    if (!(vect[i] == "#")) {
      b = gregexpr(pattern = "#[[:alpha:]]+ *[[:alpha:]]*#\\[[[:alpha:]]+\\]#,[[:alpha:]]+ *[[:alpha:]]*#", text = vect[i])[[1]]
      if(b[1] > 0) {
        t1 = substr(vect[i], b[1]+1, b[1] + attr(b, "match.length")[1] - 2)
        loc = regexpr(pattern = ",", text = t1)
        first.name = substr(t1, loc + 1, stop = nchar(t1))
        locl = regexpr(pattern = "#", text = t1)
        last.name = substr(t1, 1, locl - 1)
        toreturn[i,1:2] = c(first.name,last.name)
        alt.last = substr(t1, locl + 2, loc - 3)
        vect[i] = sub(pattern = "#[[:alpha:]]+ *[[:alpha:]]*#\\[[[:alpha:]]+\\]#,[[:alpha:]]+ *[[:alpha:]]*#", replacement = "#", x = vect[i])
        toreturn[i,4] = alt.last
      }
      b = gregexpr(pattern = "\\[[[:alpha:]]+\\]", text = vect[i])[[1]]
      if (b[1] > 0) {
        t1 = substr(vect[i], b[1]+1, b[1] + attr(b, "match.length")[1] - 2)
        toreturn[i,3] = t1
        vect[i] = sub(pattern = "\\[[[:alpha:]]+\\]", "#", x = vect[i])
      }
      b = gregexpr(pattern = "\\[[[:alpha:]]+ [[:alpha:]]+\\]", text = vect[i])[[1]]
      if (b[1] > 0) {
        t1 = substr(vect[i], b[1]+1, b[1] + attr(b, "match.length")[1] - 2)
        loc = regexpr(pattern = " ", text = t1)
        alt.first = substr(t1, 1, loc - 1)
        alt.last = substr(t1, loc + 1, nchar(t1))
        toreturn[i,3:4] = c(alt.first,alt.last)
        vect[i] = sub(pattern = "\\[[[:alpha:]]+ [[:alpha:]]+\\]", "#", x = vect[i])
      }
    }
  }
  return(list(vect, toreturn))
}
temp = find.names(coll)



###########
# Plan: somehow keep track of names that are checked/unchecked so that there 
#  is no need to "reread" a document

names.mat = temp[[2]]

num.in.bio.last = matrix(0, nrow = 54, ncol = 54) # num paragraph shows up
num.in.bio.first = matrix(0, nrow = 54, ncol = 54) # num paragraph shows up
num.in.bio.both = matrix(0, nrow = 54, ncol = 54) # num paragraph both shows up

num.in.bio.self = rep(0, times = 54)
self = c(he, him, his, her, she, hers)

for(i in 1:54) {
  for(j in 1:54) {
    e = length(grep(pattern = names.mat[j,2], x = text.toproc[[i]]))
    d = sum(e)
    num.in.bio.last[i,j] = d
    e = length(grep(pattern = names.mat[j,1], x = text.toproc[[i]]))
    d = sum(e)
    num.in.bio.first[i,j] = d
  }
}

for(i in 1:54) {
  for(j in 1:54) {
    d = 0
    e = grep(pattern = names.mat[j,2], x = text.toproc[[i]])
    f = grep(pattern = names.mat[j,1], x = text.toproc[[i]])
    if (length(e) > 0) {
      for(k in 1:length(e)) {
        d = d + sum(f == e[k])
      }
    }
    num.in.bio.both[i,j] = d
  }
  print(i)
}

zz = 0
for(i in 1:54) {
  zz[i] = sum(num.in.bio.both[i,]) - num.in.bio.both[i,i] }

which(zz > 5) -> rr
num.in.bio.both[rr,rr] -> tt
tt = tt - diag(1,18)*50 - 1
tt[tt < 1] = 0
tt = tt + t(tt)

names.mat[rr,1]
names.mat[rr,2]
ss = ""
for(i in 1:54) {
  ss[i] = paste(names.mat[i,1], names.mat[i,2])
}
test2 = network(num.in.bio.both)
plot.network(test2, label = ss)

rownames(tt) = ss
colnames(tt) = ss
library(network)
test = network(tt)
plot.network(test, label = ss)

num.both.close =  matrix(0, nrow = 54, ncol = 54) # number times within 15 words
for(i in 1:54) {
  for(j in 1:54) {
    e = grep(pattern = names.mat[j,2], x = text.toproc[[i]])
    d = 0
    if (length(e) > 0) { 
      for(k in 1:length(e)) {
        f = gregexpr(pattern = names.mat[j,2], text.toproc[[i]][e[k]])
        g = gregexpr(pattern = names.mat[j,1], text.toproc[[i]][e[k]])
        if (g[[1]][1] > 0) {
          for(l in 1:length(f)) {
            if (sum(abs(f[[1]][l] - g[[1]]) < 15) > 0) { d = d + 1 } 
          }
        }
      }
    }
    num.both.close[i,j] = d
  }
  print(i)
}

yy = network(num.both.close)
plot.network(yy, label = ss)


h = num.in.bio.both - 500*diag(1,54)
h[h<0] = 0

f = num.in.bio.first - 500*diag(1,54)
g = num.in.bio.last - 500*diag(1,54)
f[f<0] = 0
g[g<0] = 0

hist(h[h>0], main = "Histogram of non-diagonal entries of B", xlab = "Value of B_ij's")
(sum(f == 0) - 54) / (54 * 53)
(sum(g == 0) - 54) / (54 * 53)
(sum(h == 0) - 54) / (54 * 53)

proc



#lapply(text.toproc, FUN = function(x) {sapply(x, FUN = substr, start = 1, stop = 50, USE.NAMES=FALSE)})







###
library(XML)
xmlParse(filenames[[1]])
htmlTreeParse(filenames[[6]])[[3]][[1]][[2]]








## (openNLP.R)
library(openNLP)
library(openNLPmodels.en)
library(tm)

test = "The quick brown fox jumped over the slow lazy cat."
tagPOS(test)

testfile = readLines("file_11516.txt")
test2 = dnb.grab.main(testfile)

#relabel.html <- function(text) {
# temp = text
# temp = gsub("&#145;", "\'", temp)
  # temp = gsub("&#146;", "\'", temp)
  # temp = gsub("&#150;", "-", temp)
  # temp = gsub("&#151;", "-", temp)
  # temp = gsub("%20", " ", temp)
  # temp = gsub("%2C", ",", temp)
  # temp = gsub("%3F", "-", temp)
  # temp = gsub("<br>", " ", temp)
  # temp = gsub("<a.*?</h2>", " ", temp)
  # temp = gsub("<.*?>", " ", temp)
  # temp = gsub("\\( d .", "\\(died", temp)
  # return(temp[-length(temp)])
#}

bb = relabel.html(test2)
bc = sentDetect(bb)
bd = tagPOS(bc)

be = strsplit(bd, split = " ")

zz = regexpr("/.*?$", be[[1]])
substr(be[[1]], zz, zz + attr(zz, "mat"))
substr(be[[1]], 1, zz-1)

split.vector = function(vec) { # returns matrix
  zz = regexpr("/.*?$", vec) 
  tags = substr(vec, zz, zz + attr(zz, "mat"))
  text = substr(vec, 1, zz-1)
  return(rbind(text,tags))
}

split.list = function(lis) { #return list of split.vector matrices
  toreturn = list()
  for(i in 1:length(lis)) {
    toreturn = c(toreturn, list(split.vector(lis[[i]])))
  }
  return(toreturn)
}
bf = split.list(be)

extract.noun.seq = function(lis) { #input: list of matrices
  ## Columns:
  # 1. combined text
  # 2. tag
  # 3. sent. location start
  # 4. sent. location end
  # 5. sentence
  toreturn = matrix(NA, nrow = length(lis)*10, ncol = 5)
  index = 1
  for(i in 1:length(lis)) {
    pron = grep("/PRP", lis[[i]][2,])
    if (length(pron) > 0) {
      for (j in 1:length(pron) ) { 
        toreturn[index,] = c(lis[[i]][1,pron[j]], lis[[i]][2,pron[j]], pron[j], pron[j], i)
        index = index + 1
      }
    }
    nnp = grep("/NNP", lis[[i]][2,])
    if (length(nnp) > 0) {
      nnp.mat = find.seq(nnp)
      #   print(nnp.mat)
      for(j in 2:dim(nnp.mat)[1]) {
        toreturn[index,] = c( 
          paste(lis[[i]][1,nnp.mat[j,1]:nnp.mat[j,2]], collapse = " "), 
          "/NNP", 
          nnp.mat[j,1], nnp.mat[j,2], i)
        index = index + 1
      }
    }
  }
  return(toreturn[1:index,])
}

extract.noun.seq(bf)

find.seq = function(vec) { #output = matrix of start/end, 2 cols
  cp.vec = vec
  ret = matrix(NA,nrow = 50, ncol = 2)
  ind = 1
  last = -10
  for(i in 1:length(vec)) {
    if(cp.vec[1] == last+1) {
      ret[ind,2] = cp.vec[1]
      last = cp.vec[1]
      cp.vec = cp.vec[-1]
    } else {
      if (ind > 0) {
        if (is.na(ret[ind,2])) { ret[ind,2] = ret[ind,1] }
      }
      ind = ind + 1
      ret[ind,1] = cp.vec[1]
      last = cp.vec[1]
      cp.vec = cp.vec[-1]
    }
  }
  if(is.na(ret[ind,2])) { ret[ind,2] = ret[ind,1] }
  
  return(ret[1:ind,])
}

tt = grep("/NNP", bf[[16]][2,])

tokenize(bd)
list.mats = list()
for(i in 1:54) {
  a1 = relabel.html(text.toproc[[i]])
  a2 = sentDetect(a1)
  a3 = tagPOS(a2)
  a4 = strsplit(a3, split = " ")
  a5 = split.list(a4)
  a6 = extract.noun.seq(a5) 
  list.mats = c(list.mats, list(a6))
  print(i)
}

rm.pron.etc <- function(lis) {
  tmp = list()
  for(i in 1:length(lis)) {
    test = lis[[i]][-dim(lis[[i]])[1],]
    test = test[test[,2] == "/NNP",]
    
    tmp = c(tmp, list(test)) 
  } 
  return(tmp)
}
list.names = rm.pron.etc(list.mats)

test1 = c(as.Corpus(text.toproc))

## (xmlstuff.R)
library(XML)

b = readLines("en-tagdict.xml")
r = xmlParseDoc(b)
