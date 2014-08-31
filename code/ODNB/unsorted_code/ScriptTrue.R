## Temporary script; source is in s2ODNBproc.code.R

source("s2ODNBProcFx.R")
load("ODNBdatav1.Rdata")

true.docs = list()
exists.truedoc = rep(FALSE, times = 99999)

for(j in which(!ODNB.cosubject)) {
  true.docs[[ODNB.nums[j]]] = relabel.html(ODNB.data[[j]]$text)
  exists.truedoc[ODNB.nums[j]] = TRUE
  print(j)
}
save(true.docs, exists.truedoc, file = "truedocs.Rdata")
