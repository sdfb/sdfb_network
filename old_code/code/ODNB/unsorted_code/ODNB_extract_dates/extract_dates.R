#@S Code that extracts dates and names of ODNB articles



# TODO: Move this segment to separate file. [ it uses functions from somewhere... ]
# relies on stuff from new.datenameproc.R
# NOTE ## This is only existence of code... ? cant find it elsewhere for now. 
load("1230sp.datamat.sparse.Rdata")
dmname.bdate = rep(0, times = 6294)
for (j in 1:6294) {
  nam.match = find.match(dm.names[j])
  good.match = which.proper.date(nam.match)
  nam.match = nam.match[good.match]
  relevant = round(new.result.matrix[which(new.result.matrix[,2] == j),])
  match(relevant[,1], nam.match)
  a = table(match(relevant[,1], ODNB.nums[nam.match]))
  best = which(a == max(a))
  if(length(best) == 0) {
    best = sample(1:length(nam.match), size = 1)
  }
  dmname.bdate[j] = dates.matrix[nam.match[best], 1]
  print(j)
}
save(dmname.bdate, dm.names, file = "est.birthdates.Rdata")
