
###################### OLD CODE

read.csv(file = "words.edit.csv") -> wds2
test = wds2[wds2[,2] == 1,1]
test = as.character(test)
res = finalist[finalist[,2] == test[1],]
for(j in 2:length(test)) {
  res = rbind(res, finalist[finalist[,2] == test[j],])
  print(j)
}

doc.cts = table(res[,7])
ids.great10 = names(which(doc.cts > 10))

rowind = sort(ids.great10)
an7 = as.numeric(res[,7])
an6 = as.numeric(res[,6])

dat.mat = matrix(0,nrow = length(rowind), ncol = length(test))

for(k in 1:length(rowind)) {
  print(paste(k,"of",length(rowind)))
  matches = which(an7 == rowind[k])
  print(matches)
  for(m in 1:length(matches)) {
    dat.mat[k,which(test == res[matches[m],2])] = an6[m]
  }
}

for(j in 1:dim(res)[1]) {
  if(j %% 10 == 0) { print(paste(j,"of",dim(res)[1])) }
  dat.mat[which(rowind == as.numeric(res[j,7])),which(test == res[j,2])] = res[j,6]
}

s.cov = t(dat.mat) %*% dat.mat



