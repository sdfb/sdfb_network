#testing combination code
load("truedocsv2.Rdata")

# checking function: stanford/lingpipe
load("stancheck.Rdata")
for(i in which(exists.truedoc)) {
  a = fix_tagtext(text = st.docs[[i]], true.text = true.docs.word[[i]], type = "ST")
#|    ***********
#|----##replace period with _ --Tue Sep  2 09:43:54 2014--
  if (length(a) > 2) {
  } else {
    print(paste("ERROR:",i))
  }
  if (i %% 50 == 0) { print(i) }
}

load("lpcheck.Rdata")
for(i in which(exists.truedoc)) {
  a = fix_tagtext(text = lp.docs[[i]], true.text = true.docs.word[[i]], type = "LP")
#|    ***********
#|----##replace period with _ --Tue Sep  2 09:43:54 2014--
  if (length(a) > 2) {
  } else {
    print(paste("ERROR:",i))
  }
  if (i %% 50 == 0) { print(i) }
}

# works for both stanford/lingpipe 
# [each tagged text can be resolved with full text]

# Find tagged positions => get matrix: 1st col = words, 
#  other columns = positions of matches of different types. 

# examine multiple matches per word: what types?
load("truedocsv2.Rdata")
load("stancheck.Rdata")
for(i in which(exists.truedoc)) {
  a = fix_tagtext(text = st.docs[[i]], true.text = true.docs.word[[i]], type = "ST")
#|    ***********
#|----##replace period with _ --Tue Sep  2 09:43:54 2014--
  # temp fixes : need to apply to processing function
  a = gsub("</*DATE>", "", a)
  a = gsub("</*MONEY>", "", a)
  if (length(a) > 2) {
    b = gregexpr("<.*?>", a)
    helper3 = function(x) {return(length(x) > 1)}
    d = sapply(b, helper3)
    matches = a[which(d)]
    mis.order = grep("</.*?>.*<.*>", matches)
    if (length(mis.order) > 0) {
      for(j in mis.order) {
        print(matches[j])
      }
    }
  } else {
    print(paste("ERROR:",i))
  }
  if (i %% 50 == 0) { print(i) }
}
# stanford docs: this happens
# "'<PERSON>John</PERSON><DATE>'" ------ get rid of DATE
# "<MONEY>$18,000</MONEY>-<MONEY>$20,000</MONEY>; --- get rid of MONEY
#  "<LOCATION>Hudson</LOCATION>'s-<LOCATION>Bay'</LOCATION>,"
# "<LOCATION>Venetia</LOCATION>./<ORGANIZATION>Fecit"
# "<PERSON>Stephen</PERSON>'s-<ORGANIZATION>Green</ORGANIZATION>,"
# 

load("lpcheck.Rdata")
for(i in which(exists.truedoc)) {
  a = fix_tagtext(text = lp.docs[[i]], true.text = true.docs.word[[i]], type = "LP")
#|    ***********
#|----##replace period with _ --Tue Sep  2 09:43:54 2014--
  # temp fixes: apply to processing function
  a = gsub("</ENAMEX>-<ENAMEXTYPE=\"[[:alpha:]]+\">", "-", a)
  if (length(a) > 2) {
    b = gregexpr("<.*?>", a)
    helper3 = function(x) {return(length(x) > 1)}
    d = sapply(b, helper3)
    matches = a[which(d)]
    mis.order = grep("</.*?>.*<.*>", matches)
    if (length(mis.order) > 0) {
      for(j in mis.order) {
        print(matches[j])
      }
    }
  } else {
    print(paste("ERROR:",i))
  }
  if (i %% 50 == 0) { print(i) }
}
# lp docs: this happens
# "Eudaemon</ENAMEX>-<ENAMEXTYPE=\"PERSON\">Joannis"                  -- see below
# "Warning</ENAMEX>-<ENAMEXTYPE=\"ORGANIZATION\">Piece</ENAMEX>'"     -- get rid of <..>-<..> -> assume first match is right
# 

#script
load("truedocsv2.Rdata")
load("stancheck.Rdata")
load("lpcheck.Rdata")
source("s2ODNBProcFx.R")
rm(true.docs)

# proc.docs = list() #trial 1
#proc.docs2 = list() #trial 2
proc.docs3 = list() #trial 3
to.do = which(exists.truedoc)
#to.do = to.do[to.do>18150] #for trial 2
to.do = to.do[to.do>34840] #for trial 3
for(i in to.do) {
  print(i)
  st.fix = fix_tagtext(text = st.docs[[i]], true.text = true.docs.word[[i]], type = "ST")
#|         ***********
#|----##replace period with _ --Tue Sep  2 09:43:54 2014--
  lp.fix = fix_tagtext(text = lp.docs[[i]], true.text = true.docs.word[[i]], type = "LP")
#|         ***********
#|----##replace period with _ --Tue Sep  2 09:43:54 2014--
  proc.docs3[[i]] = cbind(true.docs.word[[i]],  #modify storage location
                          proc_tagtext(tagged.text = st.fix, type = "ST"),
#|                        ************
#|----##replace period with _ --Tue Sep  2 09:49:17 2014--
                          proc_tagtext(tagged.text = lp.fix, type = "LP"))
#|                        ************
#|----##replace period with _ --Tue Sep  2 09:49:17 2014--
}

# save(proc.docs, file = "proc.docs.Rdata")
# save(proc.docs2, file = "proc.docs2.Rdata")
# Trial 1: 1 to 18150. 
# Trial 2: 18151 to 34840.
#Trial3 : 34841 to . 
save(proc.docs3, file = "proc.docs3.Rdata")

## too expensive to save together. 
# processed.docs = list()
# load("proc.docs.Rdata")
# for(i in 1:18150) {
#   if (!is.null(proc.docs[[i]])) {
#     processed.docs[[i]] = proc.docs[[i]]
#   }
#   if (i %% 10 == 0) {print(i)}
# } 
# rm(proc.docs)
# 
# load("proc.docs2.Rdata")
# for(i in 18151:34840) {
#   if (!is.null(proc.docs2[[i]])) {
#     processed.docs[[i]] = proc.docs2[[i]]
#   }
#   if (i %% 10 == 0) {print(i)}
# }
# rm(proc.docs2)
# 
# load("proc.docs3.Rdata")
# for(i in 34841:99996) {
#   if (!is.null(proc.docs3[[i]])) {
#     processed.docs[[i]] = proc.docs3[[i]]
#   }
#   if (i %% 10 == 0) {print(i)}
# }
# rm(proc.docs3)
# 
# save(processed.docs, file = "processed.docs.Rdata")


load("proc.docs.Rdata")
# play with docs
system.time(
  for(i in 1:250) {
    if (!is.null(proc.docs[[i]])) {
      test = proc.docs[[i]]
      combine_two(test[,2], test[,5])
#|    ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--
      
    }
  }
)

################!!! Copied 3/24/2014
# combined tags: 
# ORDER in proc.docs :
# Text ST:PERSON ST:ORGANIZATION ST:LOCATION LP:PERSON LP:ORGANIZATION LP:LOCATION
combine.order = c("ST:PERSON", "LP:PERSON", "ST:ORGANIZATION", "LP:ORGANIZATION")
combine.index = match(combine.order, colnames(proc.docs[[1]]))
combined.tags = list()
for(i in 1:18150) {
  if (!is.null(proc.docs[[i]])) {
    temp = proc.docs[[i]][,combine.index[1]]
    for(j in 2:length(combine.index)) {
      temp = combine_two(a=temp, b=proc.docs[[i]][,combine.index[j]])
#|           ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--
    }
    combined.tags[[i]] = cbind(proc.docs[[i]][,1],temp)
  }
  print(i)
}

###################################################3endcopy

rm(proc.docs)

load("proc.docs2.Rdata")
for(i in 18151:34840) {
  if (!is.null(proc.docs2[[i]])) {
    temp = proc.docs2[[i]][,combine.index[1]]
    for(j in 2:length(combine.index)) {
      temp = combine_two(a=temp, b=proc.docs2[[i]][,combine.index[j]])
#|           ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--
    }
    combined.tags[[i]] = cbind(proc.docs2[[i]][,1],temp)
  }
  print(i)
}
rm(proc.docs2)

load("proc.docs3.Rdata")
for(i in 34841:99996) {
  if (!is.null(proc.docs3[[i]])) {
    temp = proc.docs3[[i]][,combine.index[1]]
    for(j in 2:length(combine.index)) {
      temp = combine_two(a=temp, b=proc.docs3[[i]][,combine.index[j]])
#|           ***********
#|----##replace period with _ --Tue Sep  2 09:31:55 2014--
    }
    combined.tags[[i]] = cbind(proc.docs3[[i]][,1],temp)
  }
  print(i)
}
rm(proc.docs3)

save(combined.tags, file = "comb.tags.Rdata") #9/5, 2:48 AM

# Testing: 
# Man.tag .mtag: 7951, 12359, 13771, 16359, 27671
man.tagged = c(7951, 12359, 13771, 16359, 27671)
man.tagged.names = c("", "", "", "", "Francis Tresham")
load("comb.tags.Rdata")
load("ODNBdatav1.Rdata")

k=5
j = man.tagged[k]
text = readLines(paste("dataman/mantag/",j,".mtag.txt", sep = ""))
true.text = combined.tags[[ODNB.nums[j]]][,1]
fix.text = fix_tagtext(text = text, true.text = combined.tags[[ODNB.nums[j]]][,1], type = "MT")
#|         ***********
#|----##replace period with _ --Tue Sep  2 09:43:54 2014--

mt = proc_tagtext(fix.text, type = "ST")[,1]
#|   ************
#|----##replace period with _ --Tue Sep  2 09:49:17 2014--
at = combined.tags[[ODNB.nums[j]]][,2]
pt = improve_pred(comb.tag = cbind(true.text, at), main.person = man.tagged.names[k],
#|   ************
#|----##replace period with _ --Mon Sep  1 16:46:42 2014--
                  regex.words = c("Commonwealth", "£", "Catholic", "Greek", 
                                  "Roman", "Describe", "Treatise"),
                  exact.words = c("Abbey", "House", "Island",
                                  "University", "College", "Cathedral",
                                  "Pamphlet", "Religion", "Country", 
                                  "Act", "Restoration", "Empire",
                                  "Government", "State", "Lawgiving",
                                  "Palace", "Petition", "Parliament", "Death"))[[3]][,2]
compute.error(tr = mt, pr = at)
compute.error(tr = mt, pr = pt)

# Not all these docs fixed: j=2 has problem, only tried up to j= 4. 
man.tagged2 = c(570, 990, 3210, 4697, 7044, 8148, 14072, 15506, 24288, 25519)
j = man.tagged2[4]
text = readLines(paste("dataman/mantag/",j,".proc.txt", sep = ""))
true.text = combined.tags[[j]][,1]
fix.text = fix_tagtext(text = text, true.text = combined.tags[[j]][,1], type = "MT")
#|         ***********
#|----##replace period with _ --Tue Sep  2 09:43:54 2014--

mt = proc_tagtext(fix.text, type = "ST")
#|   ************
#|----##replace period with _ --Tue Sep  2 09:49:17 2014--
at = combined.tags[[j]][,2]
pt = improve_pred(comb.tag = at)
#|   ************
#|----##replace period with _ --Mon Sep  1 16:46:42 2014--
compute.error(tr = mt, pr = at)

#
find_tagpositions(combined.tags[[28]])
#|*****************
#|----##replace period with _ --Mon Sep  1 16:49:37 2014--

#test for punctuation
lens = sapply(combined.tags, dim)
lens2 = sapply(lens, length)
exists.doc.comb = which(lens2 == 2)


i = sample(exists.doc.comb, size = 130)

for(j in 1:130) {
  a = find_tagpositions(combined.tags[[i[j]]])
#|    *****************
#|----##replace period with _ --Mon Sep  1 16:49:37 2014--
  b = grep("[^[:alnum:] '.&-]", a[,1])  # - needs to be last
  #print(j)
  if (length(b) > 0) {print(a[b,])}
}
i[60]

# -'s, &'s are acceptable in entity name
# need to remove: ending ',', ';', 'ending . except in case of prev capt. 
# remove 's at end, remove ' at beginning/ends of words

# Test all: 
source("s2ODNBProcFx.R")
load("comb.tags.Rdata")

lens = sapply(combined.tags, dim)
lens2 = sapply(lens, length)
exists.doc.comb = which(lens2 == 2)

for (j in exists.doc.comb) {
  a = find_tagpositions(combined.tags[[j]])
#|    *****************
#|----##replace period with _ --Mon Sep  1 16:49:37 2014--
  b = grep("[^[:alnum:] '.&/$+*=-]", a[,1])  # - needs to be last
  if (length(b) > 0) {print(a[b,])}  
  if (j %% 1000 == 0) { print(j) }
}

# Anything with \\, /, $, +, *, =, # is not going to be an entity. 
# Anything with numbers is not going to be an entity

# 68575 test
a = find_tagpositions(combined.tags[[68575]])
#|  *****************
#|----##replace period with _ --Mon Sep  1 16:49:37 2014--

remove_badentities(combined.tags[[68575]], position.df = a)
#|******************
#|----##replace period with _ --Mon Sep  1 16:55:56 2014--
table(combined.tags[[68575]][a[,2],2])

a = improve_pred(comb.tag = combined.tags[[1]], main.person = "Jacques Abbadie")
#|  ************
#|----##replace period with _ --Mon Sep  1 16:46:42 2014--
# Testing final: 
source("s2ODNBProcFx.R")
load("comb.tags.Rdata")
load("nameslist.Rdata")

lens = sapply(combined.tags, dim)
lens2 = sapply(lens, length)
exists.doc.comb = which(lens2 == 2)

end.result = list()

source("s2ODNBProcFx.R")
for (k in exists.doc.comb) {
  print(k)
  if (is.null(name.list[[k]])) {
    main.person = NULL
  } else {
    main.person = paste(name.list[[k]], collapse = " ")
  }
  a = improve_pred(comb.tag = combined.tags[[k]], main.person = main.person,
#|    ************
#|----##replace period with _ --Mon Sep  1 16:46:42 2014--
                   regex.words = c("Commonwealth", "Catholic", "Greek", 
                                   "Roman", "Describe", "Treatise"),
                   exact.words = c("Abbey", "House", "Island",
                                   "University", "College", "Cathedral",
                                   "Pamphlet", "Religion", "Country", 
                                   "Act", "Restoration", "Empire",
                                   "Government", "State", "Lawgiving",
                                   "Palace", "Petition", "Parliament", "Death")) 
  end.result[[k]] = a
}
save(end.result, file = "end.Rdata")

# compile everything into large matrix. 
load("end.Rdata")
lens = sapply(end.result, length)
exists.doc.comb = which(lens == 3)

count.words = rep(NA, times = length(exists.doc.comb))
for(i in exists.doc.comb) {
  count.words[i] = dim(end.result[[i]][[1]])[1]
  if (i %% 1000 == 0) {print(i)}
}

NN = sum(count.words, na.rm = 1)
final.data = data.frame(MatchName=rep("", times = NN), DocNum = rep(0, times = NN),
                        ID=rep(0, times = NN), Count = rep(0, times = NN), 
                        stringsAsFactors = FALSE)
ind.start = 1
for(i in exists.doc.comb) {
  mmat = end.result[[i]][[1]]
  idlist = end.result[[i]][[2]]
  idcounts = table(idlist$ID)
  match.ind = match(mmat$ID, as.numeric(names(idcounts)))
  inds.adding = ind.start:(ind.start+count.words[i]-1)
  final.data$MatchName[inds.adding] = mmat$MatchName
  final.data$DocNum[inds.adding] = i
  final.data$ID[inds.adding] = mmat$ID
  final.data$Count[inds.adding] = idcounts[match.ind]
  ind.start = ind.start + count.words[i]
  
  if (i %% 10 == 0) {print(i)}
}

save(final.data, file = "finaldata.Rdata")

# Error in doc when main.person has length > 2. 
load("end.Rdata")
load("finaldata.Rdata")

# fix errors in end.Rdata -> end2.Rdata
# fix errors in finaldata.Rdata -> finaldata2.Rdata
na.locs = which(is.na(final.data[,4]))
na.docs = final.data$DocNum[na.locs]

find.subsets <- function(full, remaining) {
  full.split = strsplit(full, split = " ")[[1]]
  remaining.split = strsplit(remaining, split = " ")
  
  subset.inds = rep(FALSE, times = length(remaining))
  for(j in 1:length(remaining.split)) {
    subset.inds[j] = !any(is.na(match(remaining.split[[j]], full.split)))
  }
  return(subset.inds)
}

for(k in na.docs) {
  mat.ind.torm = c(FALSE,find.subsets(full = end.result[[k]][[1]][1,2], 
                                      remaining = end.result[[k]][[1]][-1,2]))
  if (any(mat.ind.torm)) {
    ids.torm = end.result[[k]][[1]]$ID[mat.ind.torm]
    idlist.new = end.result[[k]][[2]]
    for(j in ids.torm) {
      idlist.new$ID[idlist.new$ID == j] = 1
    }
    end.result[[k]][[1]] = end.result[[k]][[1]][!mat.ind.torm,]
    end.result[[k]][[2]] = idlist.new
    
    FI = which(final.data$DocNum == k)
    total.count = sum(final.data$Count[FI][mat.ind.torm])
    final.data$Count[FI[1]] = total.count
    final.data$ID[FI[mat.ind.torm]] = -1
  } else {
    FI = which(final.data$DocNum == k)
    final.data$ID[FI[1]] = -1
  }
  final.data = final.data[final.data$ID != -1,]
  print(k)
}

save(end.result, file = "end2.Rdata")
save(final.data, file = "finaldata2.Rdata")

# Preparing data for Greg: 
load("final.namelist.Rdata")
load("end2.Rdata")
load("finaldata2.Rdata")

result.matrix = cbind(DocNum=0, Person=0, Location=0)
for(j in 1:length(upd.two.word.names)) {
  row.ind = which(final.data$MatchName == upd.two.word.names[j])
  for (k in row.ind) {
    doc = final.data$DocNum[k]
    id = final.data$ID[k]
    mat = end.result[[doc]][[2]]
    locs = mat$Position[mat$ID == id]
    temp = cbind(DocNum = doc, Person = j, Location=locs)
    result.matrix = rbind(result.matrix, temp)
  }
  print(j)
}

save(result.matrix, file="temp.result.greg.Rdata")
# Ask greg if he wants names in csv or just ID numbers, and then spearate id-name table
### TODO#### Still need to remove non-names, and combine same person's 

load("temp.result.greg.Rdata")
load("final.namelist.Rdata")
result.matrix = result.matrix[-1,]

# -1'ing bad persons
for (j in non.names.discovered) {
  inds = which(result.matrix[,2] == j)
  result.matrix[inds,2] = -1
  print(j)
}

# combining persons
for(k in 1:length(list.to.combine)) {
  to.use = list.to.combine[[k]][1]
  for(j in 2:length(list.to.combine[[k]])) {
    inds = which(result.matrix[,2] == list.to.combine[[k]][j])
    result.matrix[inds,2] = to.use
  }
  print(k)
}
inds = which(result.matrix[,2] == -1)
result.matrix = result.matrix[-inds,]

dm.docnums = sort(unique(result.matrix[,1])) 
dm.names = upd.two.word.names
for(k in 1:length(list.to.combine)) {
  inds = list.to.combine[[k]]
  new.name = paste(upd.two.word.names[inds], collapse = "|")
  dm.names[inds] = new.name
  print(k)
}


dm.datamat = matrix(0,nrow = length(dm.docnums), ncol = length(dm.names))
for(j in 1:2599) {
  inds = which(result.matrix[,2] == j)
  tab = table(result.matrix[inds,1])
  row.inds = match(as.numeric(names(tab)), dm.docnums)
  dm.datamat[row.inds,j] = tab
  print(j)
}

# 
# a = match(dm.docnums, upd.all.docnums)
# b = which(!is.na(a))
# d = a[!is.na(a)]
# 
# k = 1051
# e = cbind(dm.datamat[b,k], upd.data.matrix[d,k])
# e[apply(e, 1, sum) > 0,]


entries.percol = apply(dm.datamat > 0, 2, sum)
col.torm = which(entries.percol == 0)

for(j in 1:2599) {
  inds = which(result.matrix[,2] == j)
  adj = sum(col.torm < j)
  if (adj > 0) {
    result.matrix[inds,2] = j - adj
  }
  print(j)
}

dm.datamat = dm.datamat[,-col.torm]
dm.names = dm.names[-col.torm]



entries.perdoc = apply(dm.datamat > 0, 1, sum)
names.perdoc = apply(dm.datamat, 1, sum)

dm.datamat.red = dm.datamat[which(entries.perdoc > 3),]
entries.percolred = apply(dm.datamat.red > 0, 2, sum)
torm.again = which(entries.percolred < 4)

dm.datamat.red = dm.datamat.red[,-torm.again]
dm.names.red = dm.names[-torm.again]
dm.docnums.red = dm.docnums[entries.perdoc > 3]


save(result.matrix, dm.docnums, dm.names, dm.datamat,
     dm.datamat.red, dm.names.red, dm.docnums.red,
     file = "update3.datamat.Rdata")
write.csv(result.matrix, file = "matchlocations.csv")

# create name file
names.matrix = data.frame(ID=1:2521, Name=dm.names, stringsAsFactors = FALSE)
write.csv(names.matrix, file = "IdNames.csv")

# compare results to update2
#random i
i = sample(1:2517, size = 1)
dm.names.red[i]

# single name: 
corr.i = which(upd.two.word.names == dm.names.red[i])
goods = which(dm.datamat.red[,i] > 0)
corr.goods = match(dm.docnums.red[goods], upd.all.docnums)

cbind(dm.datamat.red[goods,i],upd.data.matrix[corr.goods,corr.i])


## search missing persons
uniq.names = unique(final.data$MatchName)

find.full.subset = function(query, names = uniq.names) {
  elements = strsplit(query, split = " ")[[1]]
  inds = grep(elements[1], names)
  for(j in 2:length(elements)) {
    inds = intersect(inds, grep(elements[j], names))
  }
  return(cbind(inds, uniq.names[inds]))
}

count.names = function(num) {
  # depends on final.data, uniq.names
  a = which(final.data$MatchName == uniq.names[num])
  print(length(a))
  print(sum(final.data$Count[a]))
}

# splitting longer documents
load("finaldata2.Rdata")
head(final.data)
load("end2.Rdata")

doc.total.words = rep(NA, times = 99996)
doc.matches = rep(NA, times = 99996)
for(j in 1:99996) {
  if (!is.null(end.result[[j]])) {
    doc.total.words[j] = dim(end.result[[j]][[3]])[1]
    doc.matches[j] = dim(end.result[[j]][[2]])[1]
  }
  if (j %% 1000 == 0) {
    print(j)
  }
}

save(doc.matches, doc.total.words, file = "doclengths.Rdata")
load("doclengths.Rdata")

hist(doc.matches, breaks = 200)
abline(v = mean(doc.matches, na.rm = 1), col = "blue")
abline(v = median(doc.matches, na.rm = 1), col = "red")
sd(doc.matches, na.rm = 1)

hist(doc.total.words, breaks = 50)
abline(v = mean(doc.total.words, na.rm = 1), col = "blue")
abline(v = median(doc.total.words, na.rm = 1), col = "red")
sd(doc.total.words, na.rm = 1)

mean(doc.total.words, na.rm = 1)
median(doc.total.words, na.rm = 1)
plot(doc.matches, doc.total.words)

find.splits = function(x, max=2500) {
  num.splits = 1 + floor(x / max)
  return(floor((1:num.splits-1) /num.splits * x + 1))
}
find.splits(15268)

load("doclengths.Rdata")
load("update3.datamat.Rdata")
head(result.matrix)
inds.to.proc = sort(unique(result.matrix[,1]))
new.result.matrix = result.matrix


for(j in inds.to.proc) {
  splits = find.splits(doc.total.words[j])
  if(length(splits) > 1) {
    inds = which(result.matrix[,1] == j)
    to.add = sapply(result.matrix[inds,3], function(x){sum(x >= splits)})
    new.docnums = j + to.add * 0.01
    new.result.matrix[inds,1] = new.docnums
  }
  print(j)
}

dm.names.sp = dm.names.red
dm.docnums.sp = sort(unique(new.result.matrix[,1]))
dm.datamat.sp = matrix(0, nrow = length(dm.docnums.sp), ncol = length(dm.names.sp))

factor.docs = as.factor(new.result.matrix[,1])

for(p in 1:2517) {
  sub = which(new.result.matrix[,2] == p)
  ones = rep(1, times = length(sub))
  
  tapply(ones, INDEX = factor.docs[sub], sum) -> test
  test[is.na(test)] = 0
  dm.datamat.sp[,p] = test
  print(p)
}

test = rowSums(dm.datamat.sp>0)
torm = which(test < 4)
dm.datamat.spred = dm.datamat.sp[-torm,]
dm.docnums.spred = dm.docnums.sp[-torm]

save(new.result.matrix, 
     dm.datamat.sp, dm.names.sp, dm.docnums.sp,
     dm.datamat.spred, dm.docnums.spred,
     file = "update3sp.datamat.Rdata")

# Checking update3.datamat.Rdata
load("update3.datamat.Rdata")
load("finaldata2.Rdata")

tests = 1:25 * 100 - rpois(25, 15)

k=tests[1]
for(j in 1:25) {
  k = tests[j]
  match.inds = which(final.data[,1] == dm.names[k])
  
  doc.inds = match(final.data$DocNum[match.inds], dm.docnums)
  print(sum(final.data$Count[match.inds] !=  dm.datamat[doc.inds,k]))
}

mults = grep("\\|",dm.names)

j = 21 #to 24
all.names = strsplit(dm.names[mults[j]], split = "\\|")[[1]]

match.inds = NULL
for(k in 1:length(all.names)) {
  match.inds = c(match.inds,which(final.data[,1] == all.names[k]))
}

doc.inds = match(final.data$DocNum[match.inds], dm.docnums)
temp = cbind(doc.inds, final.data$Count[match.inds], dm.datamat[doc.inds,mults[j]])
temp = temp[order(temp[,1]),]
cbind(temp, 999999 * (temp[,2] != temp[,3]))

# dm.names is right. 
match.loc = match(dm.names.red,dm.names)
doc.loc = match(dm.docnums.red,dm.docnums)

j = 2322
nonz = which(dm.datamat.red[,j] > 0)
print(sum(dm.datamat.red[nonz,j] != dm.datamat[doc.loc[nonz],match.loc[j]]))

#dm.datamat.red is right...
load("update3sp.datamat.Rdata")
match.loc = match(dm.names.sp,dm.names)

j = 580
nonz.sp = which(dm.datamat.sp[,j] > 0)
nonz.or = which(dm.datamat[,match.loc[j]] > 0)
temp = rbind(cbind(dm.docnums[nonz.or],dm.datamat[nonz.or,match.loc[j]] ,"--"),
cbind(dm.docnums.sp[nonz.sp], "--", dm.datamat.sp[nonz.sp,j]))
temp = temp[order(temp[,1]),]

which(dm.datamat[46473,] == 1)

j = 2517
nonz.sp = which(dm.datamat.sp[,j] > 0)
nonz.or = which(dm.datamat[,j] > 0)
temp = rbind(cbind(dm.docnums[nonz.or],dm.datamat[nonz.or,j] ,"--"),
             cbind(dm.docnums.sp[nonz.sp], "--", dm.datamat.sp[nonz.sp,j]))
temp = temp[order(temp[,1]),]
temp

# what i accidentally did with dm.sp...
dm.names.sp.fix = dm.names[1:2517]
save(new.result.matrix, 
     dm.datamat.sp, dm.names.sp, dm.docnums.sp,
     dm.datamat.spred, dm.docnums.spred, dm.names.sp.fix,
     file = "update3spFIX.datamat.Rdata")

## NEED TO FIX THIS STUFF...





### split into 500 word docs


# splitting longer documents


find.splits = function(x, max=500) {
  num.splits = 1 + floor(x / max)
  return(floor((1:num.splits-1) /num.splits * x + 1))
}

load("doclengths.Rdata")
load("update3.datamat.Rdata")
head(result.matrix)
inds.to.proc = sort(unique(result.matrix[,1]))
new.result.matrix = result.matrix


for(j in inds.to.proc) {
  splits = find.splits(doc.total.words[j])
  if(length(splits) > 1) {
    inds = which(result.matrix[,1] == j)
    to.add = sapply(result.matrix[inds,3], function(x){sum(x >= splits)})
    new.docnums = j + to.add * 0.01
    new.result.matrix[inds,1] = new.docnums
  }
  print(j)
}

dm.names.sp = dm.names
dm.docnums.sp = sort(unique(new.result.matrix[,1]))
dm.datamat.sp = matrix(0, nrow = length(dm.docnums.sp), ncol = length(dm.names.sp))

factor.docs = as.factor(new.result.matrix[,1])

for(p in 1:2521) {
  sub = which(new.result.matrix[,2] == p)
  ones = rep(1, times = length(sub))
  
  tapply(ones, INDEX = factor.docs[sub], sum) -> test
  test[is.na(test)] = 0
  dm.datamat.sp[,p] = test
  print(p)
}

save(new.result.matrix, 
     dm.datamat.sp, dm.names.sp, dm.docnums.sp,
     file = "update3sp500.datamat.Rdata")







#file new.datenameproc.R replaced:12/30 
ZZ.oldprocdatename = function () {
  
  
  ### getting dates
  # 10/29
  
  load("ODNBdatav1.Rdata")
  
  dates.char = rep("", times = length(ODNB.names))
  
  for (j in 1:length(ODNB.names)) {
    dates.char[j] = ODNB.data[[j]]$dates
    print(j)
  }
  
  dates.matrix = matrix(NA, nrow = length(ODNB.names), ncol = 2)
  
  type1 = grep("^[[:digit:]]{4}-[[:digit:]]{4}$", dates.char)
  for(j in 1:length(type1)) {
    W = strsplit(dates.char[type1[j]], "-")[[1]]
    dates.matrix[type1[j],] = as.numeric(W)
    if (j %% 100 == 1) { print(j) }
  }
  
  rem1 = dates.char
  rem1[type1] = ""
  no.date = setdiff(1:length(rem1), grep("[[:digit:]]", rem1))
  rem1[no.date] = ""
  
  rem2 = gsub("<em>c.</em>", "", rem1)
  rem2 = gsub("<em>fl. </em>", "", rem2)
  rem2 = gsub("\\?", "", rem2)
  grep("/[[:digit:]]+[-]", rem2)
  rem2 = gsub("/[[:digit:]]+", "", rem2)
  
  type2 = grep("^[[:digit:]]{4}-[[:digit:]]{4}$", rem2)
  for(j in 1:length(type2)) {
    W = strsplit(rem2[type2[j]], "-")[[1]]
    dates.matrix[type2[j],] = as.numeric(W)
    if (j %% 100 == 1) { print(j) }
  }
  
  rem3 = rem2
  rem3[type2] = ""
  head(rem3[which(rem3 != "")], 150)
  rem3 = gsub("<em>bap. </em>", "", rem3)
  rem3 = gsub("<em>d.</em>", "", rem3)
  rem3 = gsub("<em>b. </em>", "", rem3)
  rem3 = gsub(" ", "", rem3)
  
  type3 = grep("^[[:digit:]]{4},[[:digit:]]{4}$", rem3)
  for(j in 1:length(type3)) {
    W = strsplit(rem3[type3[j]], ",")[[1]]
    dates.matrix[type3[j],] = as.numeric(W)
    if (j %% 100 == 1) { print(j) }
  }
  
  save(dates.matrix, file = "alldates.Rdata")
  
  
  # generating new names
  
  load("alldates.Rdata")
  
  load("names.raw.Rdata")
  load("doc.totals.Rdata")
  find.match <- function(target) {
    # relies on existence of 'names'
    split = strsplit(target, split = " ")[[1]]
    res = 1:58265
    for(j in 1:length(split)) {
      res = intersect(res, grep(gsub("[.]", "", split[j]), names))
    }
    return(res)
  }
  
  already.used = read.csv("words.edit.csv")
  table.names = table(final.data[,1])
  
  # >= 20 names, not processed
  geq = names(table.names)[table.names > 4]
  rem = setdiff(geq, already.used[,1])
  length(grep( " ", rem))
  
  m = find.match(rem[1])
  dates.matrix[m,]
  
  count.proper.date <- function(matches) {
    # returns TRUE if entry should be kept
    # keep if any extracted date fits. If NA, ignore for now...
    mat = dates.matrix[matches,,drop = FALSE]
    if (any(is.na(mat))) {
      is.nas = apply(is.na(mat), 1, any)
    } else {
      is.nas = rep(FALSE, times = dim(mat)[1])
    }
    if (any(mat[!is.nas,1] <= 1700 & mat[!is.nas,2] >= 1550)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  count.proper.date(find.match(rem[6]))
  
  maybe.add = grep(" ", rem)
  keep = rep(FALSE, times = length(maybe.add))
  
  for(j in 1:length(maybe.add)) {
    match = find.match(rem[maybe.add[j]])
    if (length(match) > 0) {
      keep[j] = count.proper.date(match)
    }
    if (j %% 10 == 0) { print(j) }
  }
  
  save(geq, rem, maybe.add, keep, file = "names.testing.Rdata")
}

#made old split-500, but replaced:12/30
ZZ.oldsp500datagen = function() {
  
  
  ## construction of new columns from this
  
  load("end2.Rdata")
  load("finaldata2.Rdata")
  
  load("names.testing.Rdata")
  
  upd.two.word.names = rem[maybe.add[keep]]
  for(k in 1:10) {
    print("-------------------------------------------------")  
  }
  
  print(paste("---- THERE ARE A TOTAL OF :",length(upd.two.word.names), "NAMES TO PROCESS ----"))
  
  
  result.matrix = cbind(DocNum=0, Person=0, Location=0)
  for(j in 1:length(upd.two.word.names)) {
    row.ind = which(final.data$MatchName == upd.two.word.names[j])
    for (k in row.ind) {
      doc = final.data$DocNum[k]
      id = final.data$ID[k]
      mat = end.result[[doc]][[2]]
      locs = mat$Position[mat$ID == id]
      temp = cbind(DocNum = doc, Person = j, Location=locs)
      result.matrix = rbind(result.matrix, temp)
    }
    print(j)
  }
  save(upd.two.word.names, result.matrix, file = "new.names.resmatrix.Rdata")
  
  
  ######## make new datamat file without splits
  
  load("doclengths.Rdata")
  load("new.names.resmatrix.Rdata")
  
  result.matrix = result.matrix[-1,]
  
  inds.to.proc = sort(unique(result.matrix[,1]))
  new.result.matrix = result.matrix
  
  dmN.names = upd.two.word.names
  dmN.docnums = sort(unique(new.result.matrix[,1]))
  dmN.datamat = matrix(0, nrow = length(dmN.docnums), ncol = length(dmN.names))
  
  factor.docs = as.factor(new.result.matrix[,1])
  
  for(p in 1:length(dmN.names)) {
    sub = which(new.result.matrix[,2] == p)
    ones = rep(1, times = length(sub))
    
    tapply(ones, INDEX = factor.docs[sub], sum) -> test
    test[is.na(test)] = 0
    dmN.datamat[,p] = test
    print(p)
  }
  
  save(new.result.matrix, 
       dmN.datamat, dmN.names, dmN.docnums,
       file = "nosp.add.datamat.Rdata")
  
  load("nosp.add.datamat.Rdata")
  load("update3.datamat.Rdata")
  
  cbn.names.nosp = unique(c(dm.names, dmN.names))
  first.half = 1:2521
  second.half = 2522:6767
  cbn.docnums.nosp = sort(unique(c(dm.docnums, dmN.docnums)))
  
  cbn.datamat.nosp = matrix(0, ncol = 6767, nrow = 41722)
  
  first.match = match(dm.docnums, cbn.docnums.nosp)
  second.match = match(dmN.docnums, cbn.docnums.nosp)
  
  cbn.datamat.nosp[first.match,first.half] = dm.datamat
  cbn.datamat.nosp[second.match,second.half] = dmN.datamat
  
  save(cbn.datamat.nosp, cbn.docnums.nosp, cbn.names.nosp, file = "cbn.nosp.datamat.Rdata")
  
  
  
  ######## make new datamat file with splits
  
  
  find.splits = function(x, max=500) {
    num.splits = 1 + floor(x / max)
    return(floor((1:num.splits-1) /num.splits * x + 1))
  }
  
  load("doclengths.Rdata")
  load("new.names.resmatrix.Rdata")
  
  result.matrix = result.matrix[-1,]
  
  head(result.matrix)
  inds.to.proc = sort(unique(result.matrix[,1]))
  new.result.matrix = result.matrix
  
  
  for(j in inds.to.proc) {
    splits = find.splits(doc.total.words[j])
    if(length(splits) > 1) {
      inds = which(result.matrix[,1] == j)
      to.add = sapply(result.matrix[inds,3], function(x){sum(x >= splits)})
      new.docnums = j + to.add * 0.01
      new.result.matrix[inds,1] = new.docnums
    }
    print(j)
  }
  
  dmN.names.sp = upd.two.word.names
  dmN.docnums.sp = sort(unique(new.result.matrix[,1]))
  dmN.datamat.sp = matrix(0, nrow = length(dmN.docnums.sp), ncol = length(dmN.names.sp))
  
  factor.docs = as.factor(new.result.matrix[,1])
  
  for(p in 1:length(dmN.names.sp)) {
    sub = which(new.result.matrix[,2] == p)
    ones = rep(1, times = length(sub))
    
    tapply(ones, INDEX = factor.docs[sub], sum) -> test
    test[is.na(test)] = 0
    dmN.datamat.sp[,p] = test
    print(p)
  }
  
  save(new.result.matrix, 
       dmN.datamat.sp, dmN.names.sp, dmN.docnums.sp,
       file = "sp500.add.datamat.Rdata")
  
  
  
}

# combining
load("sp500.add.datamat.Rdata")
load("update3sp500.datamat.Rdata")

cbn.names = unique(c(dm.names.sp, dmN.names.sp))
first.half = 1:2521
second.half = 2522:6767
cbn.docnums = sort(unique(c(dm.docnums.sp, dmN.docnums.sp)))

cbn.datamat = matrix(0, ncol = 6767, nrow = 95206)

first.match = match(dm.docnums.sp, cbn.docnums)
second.match = match(dmN.docnums.sp, cbn.docnums)

cbn.datamat[first.match,first.half] = dm.datamat.sp
cbn.datamat[second.match,second.half] = dmN.datamat.sp

save(cbn.datamat, cbn.docnums, cbn.names, file = "comb500.datamat.Rdata")

load("comb500.datamat.Rdata")


### iteration 1: remove columns that are incorrect from analysis of John Milton
## takes either 2521 -> smaller OR
## combined (since 1:2521 is first anyway) -> smaller

load("comb500.datamat.Rdata")

library(Matrix)

rowSums(cbn.datamat > 0) -> a
dm = Matrix(cbn.datamat[a>2,], sparse = TRUE) #greater than 2
rm(cbn.datamat)

to.remove = c(2234, 148, 70, 965, 1856, 2126, 850, 2017, 2116, 847, 1745, 181, 490)
to.combine = c(217, 256, 1401, 2862)

# combine cols 1st
s = apply(dm[,to.combine], 1, sum)
dm[,217] = s
cbn.names[217] = paste(cbn.names[to.combine], collapse = "|")

fin.toremove = sort(c(to.remove, to.combine[2:4]))
dm = dm[,-fin.toremove]
cbn.names = cbn.names[-fin.toremove]

save(dm, cbn.names, cbn.docnums, file = "cb500g2i1.dm.Rdata")

# no g2
load("comb500.datamat.Rdata")

library(Matrix)

dm = Matrix(cbn.datamat, sparse = TRUE)
rm(cbn.datamat)

to.remove = c(2234, 148, 70, 965, 1856, 2126, 850, 2017, 2116, 847, 1745, 181, 490)
to.combine = c(217, 256, 1401, 2862)

# combine cols 1st
s = apply(dm[,to.combine], 1, sum)
dm[,217] = s
cbn.names[217] = paste(cbn.names[to.combine], collapse = "|")

fin.toremove = sort(c(to.remove, to.combine[2:4]))
dm = dm[,-fin.toremove]
cbn.names = cbn.names[-fin.toremove]

save(dm, cbn.names, cbn.docnums, file = "cb500i1.dm.Rdata")


# for nosplit docs

load("cbn.nosp.datamat.Rdata")

library(Matrix)

rowSums(cbn.datamat.nosp > 0) -> a
dm = Matrix(cbn.datamat.nosp, sparse = TRUE)


to.remove = c(2234, 148, 70, 965, 1856, 2126, 850, 2017, 2116, 847, 1745, 181, 490)
to.combine = c(217, 256, 1401, 2862)

# combine cols 1st
s = apply(dm[,to.combine], 1, sum)
dm[,217] = s
cbn.names.nosp[217] = paste(cbn.names.nosp[to.combine], collapse = "|")

fin.toremove = sort(c(to.remove, to.combine[2:4]))
dm = dm[,-fin.toremove]
cbn.names.nosp = cbn.names.nosp[-fin.toremove]

save(dm, cbn.names.nosp, cbn.docnums.nosp, file = "cbnospi1.dm.Rdata")

