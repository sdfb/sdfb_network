load("toplot.Rdata")


word.count.over.doc = tapply(finalist[,6], finalist[,2], sum)


lengths.table.id.tab
par(mfrow = c(2,1))
hist(lengths.table.id, breaks = 20, xlab = "Number of Unique Words", ylab = "Number of Documents", main = "Histogram of Number of Documents with n Unique Words")
hist(lengths.table.id[lengths.table.id<100], breaks = 20, , xlab = "Number of Unique Words", ylab = "Number of Documents", main = "Histogram of # Docs with n Unique Words (Truncated to n <= 100)")

test = matrix(0, nrow = 5, ncol = 45)
for(i in 1:5) {
b = 1:44321
agg = NULL
for(j in 1:44) {
  r = sample(1:length(b), 1000)
  res = list()
  for(k in 1:1000) {
    res[[k]] = finalist[finalist[,7] == r[k],2]
  }
  agg = unique(c(agg, c(res, recursive= 1)))
  test[i,j+1] = length(agg)
  b = b[-r]
  print(j)
}
print(i)
}

subsamp.y1 = as.vector(test)
subsamp.x1 = rep(0:44, each = 20)*1000
plot(subsamp.x1, subsamp.y1)
abline(a = 0, b = 35)

length(main.names.vec)
length(unique(test))
table(table(test))
which(table(test) > 8) -> b
torm = c(grep("family", names(b)), grep("saints", names(b)))

test = main.names.vec[ODNB.nums[aa]]

sort(sep.words.tab) -> sep.words.tab.sorted

numss = c(570, 3210, 7044, 8148, 14072, 15506)
namess = list()
namess[[1]] = c("Charles","Ansell")
  namess[[2]] = c("Dudley","Bradstreet")
  namess[[3]] = c("Alexander","Dalrymple")
  namess[[4]] = c("Edward","Sutton")
  namess[[5]] = c("Hugh","Hughes")
  namess[[6]] = c("Thomas", "Keyworth")


find.err.rates.simple <- function(j) {
  ## Input      File number
  ## Output     num matrix
  ## 
  ## Takes a file number, and outputs error rates. 
  
  man.fn = ".proc.txt"
  read = readLines(paste(numss[j], man.fn, sep = ""))
  true = get.truetext(numss[j])
  tt = fix_tagtext(text = read, true.text = true, type = "ST")
#|     ***********
#|----##replace period with _ --Tue Sep  2 09:43:54 2014--
  true.tag = proc_tagtext(tt, tag = "PERSON", type = "ST")
#|           ************
#|----##replace period with _ --Tue Sep  2 09:49:17 2014--
  pred.tag = find.tags(num = numss[j], which.types = cur.types)
  
  d = find.wordlists(main.person = namess[[j]], dat.mat = pred.tag)
  pred.tag = d[[3]]
  
  toreturn = matrix(0, ncol = 5, nrow = 2)
  
  colnames(toreturn) = c("Total Names", "Correct", "Partial", "Missed", "Extra")
  toreturn[1,] = error.rate(true.tag, pred.tag)
  
  return(toreturn)
}

for(j in 1:6) {
  print(find.err.rates.simple(j))
}

16 + 13 + 100 + 111 + 26 + 4

3.5 + 25 + 106 + 96.5 + 11.5 + 15
8 + 12 + 85 + 87 + 11 + 16

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

