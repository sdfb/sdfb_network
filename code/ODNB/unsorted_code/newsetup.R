## For a random sample of some 1-word appearances, search 
#  through all the documents to find undetected appearances. 

#load("truedocs.Rdata")

load("curnums.Rdata")
load('nums.Rdata')
rm(is.checkCO, is.name, is.wordCO)

load("countss.Rdata")

ones = which(word.counts == 1)
one.names = names(word.counts[ones])
head(one.names)
samp = sample(one.names, size = 100)
to.proc = recordt[ODNB.nums[uses]]

search.text = function(words) {
  word.split = strsplit(words, split = " ")[[1]]
  count = 0
  for(j in 1:49504) {
    text = c(strsplit(true.docs[[to.proc[j]]][[2]], split = " "), recursive = 1)
    apps = list()
    for(k in 1:length(word.split)) {
      apps[[k]] = grep(word.split[k], text)
    }
    if (length(word.split) == 1) {
      count = count + length(apps[[1]])
    } else {
      if (length(apps[[1]]) > 0) {
        for( i in 1:length(apps[[1]])) {
          pos = apps[[1]][i]
          matches = 0
          for (m in 2:length(word.split)) {
            if(length(apps[[m]]) > 0) {
            if (sum((pos+m-1) == apps[[m]]) == 1) {
              matches = matches + 1
            }
            }
          }
          if (matches == (length(word.split) - 1)) {
            count = count + 1
          }
        }
      }
    }
  if (j %% 5000 == 0) { print(j)}
  }
  return(count)
}
counts.vec = rep(NA, times = 100)
for (n in 1:100) {
  print(paste("Trial:", n, sep = " "))
  counts.vec[n] = search.text(samp[n])
}

# Start time: 1:18 AM

load("count.list.Rdata")

mins = c(5, 25, 100, 250)
word.sample = matrix("", ncol = 8, nrow = 200)
for(j in 1:4) {
  uniqs = which(word.counts > mins[j])
  uniqs.names = names(word.counts[uniqs])
  word.sample[,j] = sample(uniqs.names, size = 200)
  
  cts = which(total.counts > mins[j])
  cts.names = names(total.counts[uniqs])
  word.sample[,j+4] = sample(cts.names, size = 200)
}

m = 25
wds = names(word.counts[word.counts > 25])

write.csv(wds, file = "words.csv")
wds2









