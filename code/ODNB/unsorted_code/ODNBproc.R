## Loading libraries
library(openNLP)
library(openNLPmodels.en)
library(tm)

load("odnbdata.Rdata")

#nee 45951
#title 47951

###### Error rate analysis ######

## Writing text to files to be taggable. 
## Process: 12359, 16359, 7951, 13771, 27671
i = 12359
writeLines(text = sentDetect(relabel.html(ODNB.data[[i]]$text)), con = paste(i,".txt",sep = ""))
i = 16359
writeLines(text = sentDetect(relabel.html(ODNB.data[[i]]$text)), con = paste(i,".txt",sep = ""))
i = 7951
writeLines(text = sentDetect(relabel.html(ODNB.data[[i]]$text)), con = paste(i,".txt",sep = ""))
i = 13771
writeLines(text = sentDetect(relabel.html(ODNB.data[[i]]$text)), con = paste(i,".txt",sep = ""))
i = 27671
writeLines(text = sentDetect(relabel.html(ODNB.data[[i]]$text)), con = paste(i,".txt",sep = ""))

a = sentDetect(relabel.html(ODNB.data[[27671]]$text))
b = as.vector(rbind(a, ""))
writeLines(text = b, con = "27671.txt")
sentDetect(relabel.html(ODNB.data[[16359]]$text))
sentDetect(relabel.html(ODNB.data[[7951]]$text))
sentDetect(relabel.html(ODNB.data[[13771]]$text))
sentDetect(relabel.html(ODNB.data[[27671]]$text))


## Finding error rates

filenums = c(7951, 12359, 16359, 13771, 27671)
man.fn = ".mtag.txt"
sta.fn = ".s7tag.txt"
lin.fn = ".lptag.txt"

test.err = find.err.rates(filenums[5])

err.rates.array <- array(dim = c(5,10,5))
find.err.rates(filenums[1]) -> err.rates.array[1,,]
find.err.rates(filenums[2]) -> err.rates.array[2,,]
find.err.rates(filenums[3]) -> err.rates.array[3,,]
find.err.rates(filenums[4]) -> err.rates.array[4,,]
find.err.rates(filenums[5]) -> err.rates.array[5,,]
total.number.true = sum(err.rates.array[,1,1])

j = 9
total.number.correct = sum(err.rates.array[,j,c(2,3)])
total.number.correct / total.number.true
total.number.guesses = sum(err.rates.array[,j,c(2,5)]) + 2 * sum(err.rates.array[,j,3])
total.number.correct / total.number.guesses



find.names <- function(mat) {
  ## Input      mat = two row matrix of predictions
  ## Output     matrix, 1st col = names identified; 2nd col = location
  ## 
  ## Finds all identified names and outputs it

  starts = which(mat[2,] == "PS")
  ends = which(mat[2,] == "PE")
  single = which(mat[2,] == "P")
  toreturn = cbind(mat[1,single],single)
  temp = matrix(nrow = length(starts), ncol = 2)
  for (i in 1:length(starts)) {
    temp[i,] = c(paste(mat[1,starts[i]:ends[i]], collapse = " "), starts[i])
  }
  temp = rbind(toreturn, temp)
  temp[,1] = gsub("[,.']s*$", "", temp[,1])
  temp[,1] = gsub("\'$", "", temp[,1])
  temp = data.frame(temp[,1], as.numeric(temp[,2]))
  return(temp[order(temp[,2]),])
}

a = proc.person(readLines("adatagged/12359.s7tag.txt"))
z = proc.person(readLines("adatagged/12359.mtag.txt"))
t(rbind(z,a[2,]))

find.names(a) -> b
find.names(z) -> x
head(b, 25)

ss = error.findall(z,a)

yy = list()
for(i in 1:5) {
  
  num = filenums[i]
man.fn = ".mtag.txt"
sta.fn = ".s7tag.txt"
lin.fn = ".lptag.txt"
true.tag = proc.person(readLines(paste("adatagged/",num, man.fn, sep = "")))
sta.txt = readLines(paste("adatagged/",num, sta.fn, sep = ""))
lin.txt = proc.lingpipe(readLines(paste("adatagged/",num, lin.fn, sep = "")))
lin.txt = vectorize.lingpipe(combine.punct(lin.txt), true.text = true.tag[1,])
sta.p = proc.person(sta.txt)
sta.o = proc.person(sta.txt, "ORGANIZATION")
lin.p = proc.enamex(lin.txt)
lin.o = proc.enamex(lin.txt, "ORGANIZATION")
sta.op = combine_two(sta.p[2,], sta.o[2,])
#|       ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--
lin.op = combine_two(lin.p[2,], lin.o[2,])
#|       ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--
stalin.op = combine_two(sta.op, lin.op)
#|          ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--

  yy[[i]] = error.findall(true = true.tag, pred = rbind(true.tag[1,], stalin.op))
}

zzz = find.names(rbind(cur.text, cur.test))

cur.text = true.tag[1,]
cur.test = stalin.op




num = filenums[2]
true.tag = proc.person(readLines(paste("adatagged/",num, man.fn, sep = "")))
sta.txt = readLines(paste("adatagged/",num, sta.fn, sep = ""))
lin.txt = proc.lingpipe(readLines(paste("adatagged/",num, lin.fn, sep = "")))
lin.txt = vectorize.lingpipe(combine.punct(lin.txt), true.text = true.tag[1,])
sta.p = proc.person(sta.txt)
sta.o = proc.person(sta.txt, "ORGANIZATION")
lin.p = proc.enamex(lin.txt)
lin.o = proc.enamex(lin.txt, "ORGANIZATION")
sta.op = combine_two(sta.p[2,], sta.o[2,])
#|       ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--
lin.op = combine_two(lin.p[2,], lin.o[2,])
#|       ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--
stalin.op = combine_two(sta.op, lin.op)
#|          ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--

true.tag = proc.person(readLines(paste("adatagged/",12359, man.fn, sep = "")))
name.test = paste(rev(strsplit(ODNB.data[[12359]]$about, ",")[[1]]), collapse = " ")
main.person = rm.spaces(strsplit(name.test, " ")[[1]])
vec.guess = yy[[2]][[2]][,1]
guess.loc = yy[[2]][[2]][,2]
text = true.tag[1,]
tags = stalin.op


b = analyze.text(main.person = main.person, vec.guess = vec.guess,
             guess.loc = guess.loc, text = text, tags = tags)





for(i in 1:length(newids)) {
  print(paste(idlist$Names[i],":::",full.names.id$Name[full.names.id$ID == newids[i]]))
}

idlist$Location[idlist$ID == 0]
which(idlist$ID == 0)
zzz[which(idlist$ID == 0),2]
test2 = remove_entries(tags = cur.test, entries = zzz[,2])
#|      **************
#|----##replace period with _ --Mon Sep  1 16:56:42 2014--

error.rate(true.tag[2,], b[[3]])
error.findall(true.tag, rbind(true.tag[1,], b[[3]]))




error.findall <- function(true, pred) {
  ## Input      tr = char mat (text, "P",etc.) [TRUE TAGS]
  ##            pr = char mat (text, "P",etc.) [PREDICTED TAGS]
  ## Output     list of different errors
  ## 
  ## Outputs list as follows: 

  tr = true
  false.names.poss = pred
  pr = pred[2,]
  
  true.names = find.names(tr)
  Status = as.character(rep("", times = dim(true.names)[1]))
  true.names = data.frame(true.names, Status, stringsAsFactors= FALSE)
  colnames(true.names) = c("Name", "Position", "Status")
  rownames(true.names) = 1:dim(true.names)[1]
  
  false.guesses = data.frame(find.names(false.names.poss), "", stringsAsFactors = FALSE)
  colnames(false.guesses) = c("Name", "Position", "Status")
  rownames(false.guesses) = 1:dim(false.guesses)[1]
  
  tr = true[2,]
  tr.left = which.ids(tr)
  pr.left = which.ids(pr)
  
  j = which(pr == "P")
  if (length(j) > 0) {
    for(i in 1:length(j)) {
      if (tr[j[i]] == "P") { 
        
        temp = max(true.names[true.names[,2] <= j[i],2])
        temp = which(true.names[,2] == temp)
        true.names$Status[temp] = "CORRECT"
        tr.left = tr.left[tr.left != j[i]]
        pr.left = pr.left[pr.left != j[i]]
        
      } else if (sum(tr.left == j[i]) > 0) { 
        temp = max(true.names[true.names[,2] <= j[i],2])
        temp = which(true.names[,2] == temp)
        true.names$Status[temp] = "PARTIAL"
        
        pr.left = pr.left[pr.left != j[i]]
      } else { 
        temp = max(false.guesses[false.guesses[,2] <= j[i],2])
        temp = which(false.guesses[,2] == temp)
        false.guesses[temp,3] = "EXTRA"
      
        pr.left = pr.left[pr.left != j[i]]
      }
    }
  }
  
  k = which(pr == "PS")
  l = which(pr == "PE")
  if (length(k) > 0) {
    for(i in 1:length(k)) {
      if (tr[k[i]] == "PS" & tr[l[i]] == "PE") {
        
        temp = max(true.names[true.names[,2] <= l[i],2])
        temp = which(true.names[,2] == temp)
        true.names$Status[temp] = "CORRECT"
        tr.left = tr.left[-which(tr.left > k[i]-1 & tr.left < l[i]+1)]
        pr.left = pr.left[-which(pr.left > k[i]-1 & pr.left < l[i]+1)]
      } else if (sum(tr.left == k[i]) + sum(tr.left == l[i]) > 0) { 
        temp = max(true.names[true.names[,2] <= l[i],2])
        temp = which(true.names[,2] == temp)
        true.names$Status[temp] = "PARTIAL"
        
        pr.left = pr.left[-which(pr.left > k[i]-1 & pr.left < l[i]+1)]
        tr.left = tr.left[-which(tr.left > k[i]-1 & tr.left < l[i]+1)]
      } else { 
        temp = max(false.guesses[false.guesses[,2] <= l[i],2])
        temp = which(false.guesses[,2] == temp)
        false.guesses[temp,3] = "EXTRA"
        
        pr.left = pr.left[-which(pr.left > k[i]-1 & pr.left < l[i]+1)]
      }
    } 
  }
  
  return(list(true.names, false.guesses))
}


####### Other Tests ######
## Doc lengths
length(ODNB.data)
doc.length = rep(0, times = 58265)
for(i in 1:58265) {
  doc.length[i] = ODNB.data[[i]]$len
}

ODNB.data[[27111]]
ODNB.data[[27111]]

## Tests
testfile = readLines("file_11516.txt")
test2 = dnb.grab.main(testfile)
test3 = relabel.html(test2)

testfile2 = readLines("file_42029.txt")
is.nobio(testfile2)
is.nobio(testfile)

testfile2 = readLines("file_42074.txt")
is.nobio(testfile2)
dnb.grab.main(testfile2)



