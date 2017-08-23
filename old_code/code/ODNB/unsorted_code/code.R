zz = which(finalist[,2] == "Warrington Academy")
yy = as.numeric(finalist[zz,6])
per = as.numeric(finalist[zz,6]) * as.numeric(finalist[zz,4])
org = as.numeric(finalist[zz,6]) * as.numeric(finalist[zz,5])
sum(per)/sum(yy)
sum(org)/sum(yy)

zz = which(finalist[,2] == "Nithsdale")
yy = as.numeric(finalist[zz,6])
per = as.numeric(finalist[zz,6]) * as.numeric(finalist[zz,4])
org = as.numeric(finalist[zz,6]) * as.numeric(finalist[zz,5])
sum(per)/sum(yy)
sum(org)/sum(yy)


z = names(sep.words.tab.sorted)[sample(which(sep.words.tab.sorted == 1), 500)]
y = rep(0, times = 500)
for(j in 1:500) {
 y[j] = finalist[which(finalist[,2] == z[j], 6)]
print(j)
}

z = names(sep.words.tab.sorted)[sample(which(sep.words.tab.sorted == 3), 250)]
y = NULL
for(j in 1:250) {
 y = c(y,finalist[which(finalist[,2] == z[j], 6)])
print(j)
}



# 1   2   3   4   5 
# 35 149  52  11   3 


# 424269 unique words
# 1569410 total words
uniq.words = unique(finalist[,2])
word.counts = table(finalist[,2])

table(word.counts) # This is column indicators: # words w/ x appearances. 
total.word.counts = rep(0, times = 424269)
system.time(
for(j in 1:100) {
 total.word.counts[j] = sum(as.numeric(finalist[finalist[,2] == uniq.words[j],6]))
 print(j)
}
)

word.fact = as.factor(finalist[,2])
word.count = as.numeric(finalist[,6])

system.time(b <- tapply(X = word.count, INDEX = word.fact, sum))

# total appearance count of words
total.counts = tapply(X = word.count, INDEX = word.fact, sum)


doc.lengths = tapply(rep(1, times = dim(finalist)[1]), finalist[,7], sum)
which.bad = names(which(doc.lengths < 11))

subfinalist = finalist
for(j in 1:length(which.bad)) {
 subfinalist = subfinalist[subfinalist[,7] != which.bad[j],]
 print(j)
}

sub.wordcount = as.numeric(subfinalist[,6])
sub.wordfact = as.factor(subfinalist[,2])
sub.totalcounts = tapply(X = sub.wordcount, INDEX = sub.wordfact, sum)
sub.word.counts = table(subfinalist[,2])



#




load("countss.Rdata")
hist(total.counts[total.counts < 20], breaks = 0:20)

hist(word.counts[word.counts < 20], breaks = 0:20)
total.table = sort(table(total.counts))

xs = as.numeric(names(total.table))
plot(as.numeric(names(total.table)), log(total.table), cex = .4)
xx = which(as.numeric(names(total.table)) < 500)
plot(as.numeric(names(total.table)[xx]), log(total.table[xx]), cex = .4)

fit = loess(log(total.table) ~ as.numeric(names(total.table)) )
points(test[,1], fit$fitted[test[,2]], type = "l", col = "green", lwd = 2)
test = cbind(xs, 1:872)
test = test[order(xs),]


