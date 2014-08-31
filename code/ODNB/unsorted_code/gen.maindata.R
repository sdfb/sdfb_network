#--#--#--#--#
# Takes finalist, and obtains all instances of >25 appearances for top-25 words. 

# Loading data
load("finalist.Rdata")

# Finding two-word names from manual picked list (> 25 different doc. appearances)
words.two = read.csv(file = "good.names.csv")
all.words = as.character(words.two[words.two[,2] == 1,1])
two.word.names = all.words[grep(" ", all.words)]

# Creating matrix of document / matches
all.docnums = sort(as.numeric(unique(finalist[,7]))) # Document number (ODNB index)
data.matrix = matrix(0, nrow = length(all.docnums), 
                        ncol = length(two.word.names))

# Fill in matrix, name by name (instead of row by row, i guess it can be done either way)
for(i in 1:length(two.word.names)) {
  matches = finalist[finalist[,2] == two.word.names[i],] # Find which rows of data frame are matches
  match.index = match(as.numeric(matches[,7]), all.docnums) # Find which document numnber they are from
  data.matrix[match.index,i] = as.numeric(matches[,6]) # Fill in corresponding rows for column/word i
  print(i)
}

save(data.matrix, two.word.names, all.docnums, file = "update.datamat.Rdata")
## File modified 8/4 11:16 PM

## Checking data: requires two.word.names, data.matrix, all.docnums, finalist
random.pick <- function() {
  r.index = sample(1:length(two.word.names), size = 1)
  r.nam = two.word.names[r.index]
  all.matches = which(finalist[,2] == r.nam)
  temp = finalist[all.matches,c(-4,-5,-3)]  
  matches = match(as.numeric(temp[,4]), all.docnums)
  print(cbind(UPD=data.matrix[matches,r.index],FIN=as.numeric(  temp[,3])))
  rem = setdiff(1:(dim(data.matrix)[1]), matches)
  print(sum(data.matrix[rem,r.index]))
}

random.pick()


#--#--#--#--# -------------------
# Generate smaller data set, to use in modeling
# remove documents that have very few unique mentions, because they do not really 
# contribute to correlation matrix?

load("update.datamat.Rdata")
# Number of nonzero entries per row (document)
row.counts = apply(data.matrix > 0, 1, sum) 
table(row.counts)
hist(row.counts)
sum(row.counts > 3)

# Total number of name mentions per document (only these 2600 names)
row.sums = apply(data.matrix, 1, sum) 

# Use only 15436 documents with at least 4 different document mentions
use = which(row.counts > 3)
col.counts = apply(data.matrix > 0, 2, sum)

# Trying column counts on subset
test.data = data.matrix[use,]
col.counts.test = apply(test.data > 0, 2, sum)
which(col.counts.test == 0)

## Note column 2300 has no data (all 0's). In this case, it has to be dropped (nothing can be
#   learned about it based on this data...
## Also, a few other columns have small values (counts of 3,4,5). 
#   I will leave them in (in 10-fold cv, there is a 
#   ~ 1/1000 chance of it causing a problem...

upd.data.matrix = data.matrix[use,-2300]
upd.two.word.names = two.word.names[-2300]
upd.all.docnums = all.docnums[use]

save(upd.data.matrix, upd.two.word.names, upd.all.docnums, file = "update2.datamat.Rdata")
## File modified 8/5 4:45 PM
load("update2.datamat.Rdata")


#--#--#--#--# -------------------
# Fit gaussian model
library(huge)

cov.matrix1 = cov(data.matrix)
cov.matrix2 = cov(upd.data.matrix)

save(cov.matrix1,cov.matrix2, file = "new.covmats.Rdata")
# File modified 8/5 5:54 PM

# graphical lasso 
model1 = huge(cov.matrix1, lambda = c(0.5, 0.25, 0.1, 0.05, 0.02))
model2 = huge(cov.matrix2, lambda = c(0.5, 0.25, 0.1, 0.05, 0.02))

save(model1, model2, file = "new.graphical.lasso.Rdata")
load("new.graphical.lasso.Rdata")

# poisson graphical lasso
source("llgm.R")
lambda = c(0.4, seq(0.2, 0.1, by = -0.025), 0.05)
pmodel1 = glmpois(X = t(data.matrix), lambda = lambda)      # does not converge.
pmodel2 = glmpois(X = t(upd.data.matrix), lambda = lambda)  # does not converge.

# save(pmodel1, pmodel2, file = "new.poisson.lasso.Rdata")  

# Exploring results of these models: loading processing functions from separate file
source("fx.model.explore.R")

# General run, testing outcome of grpahical lasso
output.csv.all.edges(list.edgemats = list(model1$path[[3]], model2$path[[3]]), 
                     list.namevecs = list(two.word.names, two.word.names), 
                     each.edge.twice = FALSE, model.ID = c("gl1", "gl2")) -> test

# Top 50 names
col.sums = apply(data.matrix, 2, sum)
sort(col.sums, dec = TRUE)[50:51] # Result: 801, 794
top.names = two.word.names[which(col.sums > 800)]

# Which index for top 50 names
matches1 = match(top.names, two.word.names)
matches2 = match(top.names, upd.two.word.names)

# Test: 
find.nodenum.from.name(query = "Charles", names = two.word.names)
# Charles I is 220. 
find.names.from.nodenum(edgemat.small = model1$path[[3]], edgemat.big = model1$path[[4]],
                        names = two.word.names, nodenum = 220, two.lists = TRUE)

# Data for only top 50, trying to get poisson version to work. 
sub.data = data.matrix[,matches1]
apply(sub.data>0, 1, sum) -> sub.rs
bigs = which(sub.rs > 1)
sub.data2 = data.matrix[bigs, matches1]

lambda = c(0.4, 0.3, 0.2, 0.1)
pmodel1 = glmpois(X = t(sub.data2), lambda = lambda) # Still no luck. 

sub.data3 = sub.data2 > 0
pmodel1 = glmpois(X = t(sub.data3), lambda = lambda) # Works

#replace top 3 with third largest, use larger data set (smaller data set doesnt work)
sub.data4 = sub.data[which(sub.rs > 0),]
for(j in 1:50) {
  col = sub.data4[,j]
  sort(col, decreasing = TRUE)[3] ->third.largest
  sub.data4[sub.data4[,j] > third.largest,j] = third.largest
}
lambda = c(0.4, 0.3, 0.2, 0.1)
pmodel2 = glmpois(X = t(sub.data4), lambda = lambda)


# Top 50 tests
m1 = model1$path[[4]][matches1, matches1]
m2 = model1$path[[5]][matches1, matches1]

find.nodenum.from.name(query = "Charles", names = top.names)
find.names.from.nodenum(edgemat.small = pmodel1[,,7] != 0, two.lists = FALSE ,
                        names = top.names, nodenum = 3)

model1$sparsity
model2$sparsity

m1d = model1$path[[5]]
m2d = model2$path[[5]] # lambda = 0.02

# Removing stuff in lower triangle
for(i in 1:2600) {
  m1d[i,i:2600] = 0
  print(i)
}

for(i in 1:2599) {
  m2d[i,i:2599] = 0
  print(i)
}

# fitting new models to have similar edge counts
mod1.new = huge(cov.matrix1, lambda = c(0.017, 0.0168, 0.0166, 0.0164, 0.0162))
m1d = mod1.new$path[[2]] #lambda = 0.0168

pmod1.new = glmpois(X = t(upd.data.matrix[1:100, 1:10]), lambda = 0.3)


model.comparison(m2d, m2d,need.diag = FALSE)


## Old code; redo here. 
# 
# hist(apply(a$path[[2]], 1, sum), main = "Distribution of number of edges/node (lambda = 0.1)", breaks = 20,
#      xlab = "Number of Edges in Node")


## trying poisson fits: UNDOCUMENTED
X = t(upd.data.matrix)
i = 4
test = glmnet(t(X[-i,]),X[i,],family="poisson", lambda = c(.5, 0.3, 0.2, 0.1, 0.05), maxit = 100000, thres = 10^(-6))

test = glmnet(t(X[-i,]),X[i,],family="poisson", lambda = c(0.5, 0.1, 0.05, 0.045, 0.04, 0.035), maxit = 100000, thres = 10^(-6))

library(glmnet)
X = t(upd.data.matrix)
i = 4
test = glmnet(t(X[-i,]),X[i,],family="poisson", lambda = c(0.5, 0.1, 0.05, 0.021, seq(from = 0.02, to = 0.006, by = -.0005), seq(from = 0.006, to = 0.001, by = -.00025 )), maxit = 100000, thres = 10^(-7) * 3)

library(glmnet)
X = t(upd.data.matrix)
i = 4
test = glmnet(t(X[-i,]),X[i,],family="poisson", lambda = c(0.5, 0.1, 0.05, 0.02, 0.019, 0.018,
0.0175, 0.017), maxit = 100000, thres = 10^(-6))


rm(data.matrix, upd.data.matrix)




library(glmnet)
starts = list()
ends = list()
X = t(upd.data.matrix)
test = list()
for(i in 1:100) {
starts[[i]] = date()
test[[i]] = try(glmnet(t(X[-i,]),X[i,],family="poisson", lambda = c(0.5, 0.1, 0.05, 0.021, seq(from = 0.02, to = 0.006, by = -.0005), seq(from = 0.006, to = 0.001, by = -.00025 )), maxit = 2000, thres = 10^(-6)) )
ends[[i]] = date()
print(i)
}
save(test,starts, ends, file = "test2.Rdata")


library(glmnet)
starts = list()
ends = list()
X = t(upd.data.matrix)
test = list()
for(i in 301:400) {
starts[[i]] = date()
test[[i]] = try(glmnet(t(X[-i,]),X[i,],family="poisson", lambda = c(0.5, 0.1, 0.05, 0.021, seq(from = 0.02, to = 0.006, by = -.0005)), maxit = 5000, thres = 3*10^(-7)) )
ends[[i]] = date()
print(i)
}
save(test2,starts, ends, file = "test3.Rdata")

ss = strptime(c(starts, recursive = TRUE), format = "%a %b %d %X %Y" )
ee = strptime(c(ends, recursive = TRUE), format = "%a %b %d %X %Y" )
dd = ee - ss

load("update2.datamat.Rdata")


# 
# 
# head(upd.data.matrix)
# table(upd.data.matrix[,120])
# 
# 
# is.skewed.column = function(mat = upd.data.matrix, col,skew = 1, bigger.by = 2, thres = 4) {
#   # Returns TRUE only if column's 'skew'th largest value is bigger than the next largest
#   # value by "bigger.by" times, 'skew'th value is at least the 'thres'.
#   # data is 'mat'
#   vals = as.numeric(names(table(mat[,col])))
#   n.vals = length(vals)
#   if(n.vals > (skew + 2)) {
#     if (vals[n.vals-skew+1] >= thres & vals[n.vals] >= bigger.by*vals[n.vals-skew]) {
#       return (TRUE)
#     } else {
#       return(FALSE)
#     }
#   } else {
#     return(FALSE)
#   }
# }
# 
# 
# is.skewed.column(col=1)
# col.skew.1 = rep(NA, times = dim(upd.data.matrix)[2])
# for(i in 1:length(col.skew.1)) {
#   col.skew.1[i] = is.skewed.column(col=i)
#   if(i %% 10 == 0) {print(i)}
# }
# 
# col.skew.try = rep(NA, times = dim(upd.data.matrix)[2])
# for(i in 1:length(col.skew.1)) {
#   col.skew.try[i] = is.skewed.column(col=i, thres = 20)
#   if(i %% 10 == 0) {print(i)}
# }
# 
# bb = which(col.skew.try)
# table(upd.data.matrix[,sample(bb, size = 1)])
# 
# cc = setdiff(which(col.skew.1), bb)
# table(upd.data.matrix[,sample(cc, size = 1)])
# 


## Figuring out what the dist of top1 / top2, top1/top3, top1/top4 are. 
load("update2.datamat.Rdata")
load("ODNBdatav1.Rdata")

n = dim(upd.data.matrix)[2] # number names
r = dim(upd.data.matrix)[1] # number data points
top.matrix = matrix(NA, ncol = 8, nrow = n)

for(i in 1:n) {
  top.matrix[i,] = sort(upd.data.matrix[,i])[r:(r-7)]
  if(i %% 10 == 0) {print(i)}
}

first.div = top.matrix[,1] / top.matrix[,2]
second.div = top.matrix[,2] / top.matrix[,3]
third.div = top.matrix[,3] / top.matrix[,4]
fourth.div = top.matrix[,4] / top.matrix[,5]
fifth.div = top.matrix[,5] / top.matrix[,6]

which(fourth.div > 2 & top.matrix[,4] > 20)
which(fifth.div > 2 & top.matrix[,5] > 20)

which(first.div > 30)
# [1]  258  571  600 1245 1730 1813 1936 2024 2099 2176 2341 2427 2457
# [14] 2491
examine.node <- function(dat = upd.data.matrix, nodenum, 
                         nams = upd.two.word.names, top = 1, exclude = NULL) {
  # Prints out variety of information about a node (to help justify 
  # normalization to poisson data)
  print(paste("This is node for:", nams[nodenum]))
  print("------------------------------------------")
  print("Table of counts in all documents for this node")
  print(table(upd.data.matrix[,nodenum]))
  last.est = tail(strsplit(nams[nodenum], split = " ")[[1]], 1)
  print("------------------------------------------")
  print("Searching last name:")
  print(find.nodenum.from.name(query = last.est, names = nams, exclude = exclude))
  print("------------------------------------------")
  print("Looking at top matches: biography subject:")
  top.val = sort(dat[,nodenum], decreasing = TRUE)[top]
  a = upd.all.docnums[which(dat[,nodenum] >= top.val)]
  print("Docnums:")
  print(a)
  print(ODNB.names[ODNB.nums.inv[a]])
  return("")
}
search.name = function(nams = upd.two.word.names, nodenum, exclude = NULL) {
  split.name = strsplit(nams[nodenum], " ")[[1]]
  for (i in 1:length(split.name))
  print(find.nodenum.from.name(query = split.name[i], names = nams, exclude = exclude))
}
examine.node(nodenum = 571)
examine.node(nodenum = 2569)

intersect(which(second.div > 2), which(top.matrix[,2] > 5 ) ) -> d
# [1]   11   80  826 1161 1874 2221 2351
examine.node(nodenum = 80, top = 2)
examine.node(nodenum = 2351, top = 2)

intersect(which(third.div > 2), which(top.matrix[,3] > 5)) -> e
# [1]  335  439  828  895 1147 1386 1440 1999 2031 2131 2205 2271
# [1]  335  439  743  828  895  897 1147 1309 1386 1440 1851 1867 1886
# [14] 1959 1999 2031 2131 2205 2220 2271 2283 2287 2407
examine.node(nodenum = 335, top = 3)
examine.node(nodenum = 439, top = 3)
examine.node(nodenum = 1440, top = 3)
examine.node(nodenum = 34, top = 3)

intersect(which(first.div > 2), which(top.matrix[,1] > 5)) -> f

# 
# hist(log(first.div, base = 10), breaks = 20)
# 
plot(top.matrix[,1], log(first.div, base = 10), xlab = "Max Count", 
     ylab = "log_10 of max/2nd-largest")
abline(h = log(2, base = 10), col = "blue", lwd = 3)
# 
# hist(top.matrix, breaks = 20) 
# 
# plot(first.div, second.div)
# 
# hist(third.div)
# 
# which(third.div > 4)
# which(second.div > 4)
# which(first.div > 4)
# 
# table(upd.data.matrix[,1171])
# 
# for(j in which(third.div > 4)) {
#   print(table(upd.data.matrix[,j]))
# }
# 
# apply(upd.data.matrix, 1, sum) -> test 
# table(test)
# which(test == 1083)
# table(upd.data.matrix[3199,])
# 
# which(upd.data.matrix[3199,] == 449)
# upd.all.docnums[3199]
# upd.two.word.names[492]
# 
# which(ODNB.nums == 8636)
# ODNB.data[[8622]]
# 
# gregexpr(" Elizabeth'*s*", ODNB.data[[8622]]$text) -> ta
# sum(sapply(ta, length))
# 
# load("stage2/odnbdata.Rdata")
# 
# 

##################### Manual deduplication
source("fx.model.explore.R")
list.to.combine = list()
# examined.nodes = NULL

# # Try first: 
# to.test = which(first.div > 30)
# # > which(first.div > 30)
# # [1]   86  179  258  310  510  565  571  600  613  672  758  824  985 1043
# # [15] 1065 1117 1183 1201 1245 1328 1333 1346 1384 1730 1768 1813 1814 1936
# # [29] 1976 2024 2099 2176 2341 2349 2371 2427 2457 2491
# 
# for(j in 1:10) {
#   print(" *#*#*#*#*#*#*#*#*#*#*#*#*#*")
# }
# i = i + 1
# examine.node(nodenum = to.test[i], top = 3, exclude = examined.nodes)
# search.name(nodenum = to.test[i], exclude = examined.nodes)
# 
# examined.nodes = unique(c(15:24,to.test,non.names.discovered,
#                                 c(list.to.combine, recursive = TRUE)))
# #257/258 same?
list.to.combine[[1]] = c(301, 310) # Dante Gabriel Rossetti
list.to.combine[[2]] = c(157, 551, 571) # George Bernard Shaw
list.to.combine[[3]] = c(421, 1533) #Lord Herbert (Edward via wikipedia)
list.to.combine[[4]] = c(2081, 2176, 2192) #Thomas Henry Huxley (Thomas Huxley, via wiki)
list.to.combine[[5]] = c(841, 1183) # (John) Horne Tooke 
list.to.combine[[6]] = c(885, 1245) #John Maynard Keynes
list.to.combine[[7]] = c(888, 1346) # John Stuart Mill
list.to.combine[[8]] = c(2078, 2099) # Thomas Babington Macaulay
list.to.combine[[9]] = c(2321, 2427, 2437) # William Ewart Gladstone
list.to.combine[[10]] = c(2457, 836) # William Holman Hunt
list.to.combine[[11]] = c(2326, 2491) #William Makepeace Thackeray


# 
# examined.nodes = unique(c(examined.nodes, c(list.to.combine, recursive = TRUE)))
# 
# # 1 hour by here
# 
# to.test = setdiff(which(first.div > 20),examined.nodes)
# i = 0
# 
# for(j in 1:10) {
#   print(" *#*#*#*#*#*#*#*#*#*#*#*#*#*")
# }
# i = i + 1
# examine.node(nodenum = to.test[i], top = 3, exclude = examined.nodes)
# search.name(nodenum = to.test[i], exclude = examined.nodes)

list.to.combine[[12]] = c(94, 564) #George Abbot
list.to.combine[[13]] = c(555, 612) # George Henry Lewes
list.to.combine[[14]] = c(2224, 2296) #Thomas Paine (Tom Paine)
# examined.nodes = unique(c(examined.nodes, c(list.to.combine, recursive = TRUE),
#                         to.test))
# 
# to.test = setdiff(grep("[.]", upd.two.word.names), examined.nodes)
# i = 0
# 
# for(j in 1:10) {
#   print(" *#*#*#*#*#*#*#*#*#*#*#*#*#*")
# }
# i = i + 1
# #examine.node(nodenum = to.test[i], top = 3, exclude = examined.nodes)
# search.name(nodenum = to.test[i], exclude = examined.nodes)



list.to.combine[[15]] = c(4, 108) #Arthur Balfour
list.to.combine[[16]] = c(221, 220) #weird artifact Charles I
list.to.combine[[17]] = c(501, 544) #Frederick Denison Maurice
list.to.combine[[18]] = c(175, 882, 1172) #John Henry Newman
list.to.combine[[19]] = c(1606, 1608) # Mahatma Gandhi
# examined.nodes = unique(c(examined.nodes, c(list.to.combine, recursive = TRUE),
#                           to.test))
# 
# find.subsets <- function(full, remaining) {
#   full.split = strsplit(full, split = " ")[[1]]
#   remaining.split = strsplit(remaining, split = " ")
#   
#   subset.inds = rep(FALSE, times = length(remaining))
#   for(j in 1:length(remaining.split)) {
#     subset.inds[j] = !any(is.na(match(remaining.split[[j]], full.split))) ## BAD? see down
#   }
#   return(subset.inds)
# }
# 
# to.test = setdiff(1:2599, examined.nodes)
# 
# # Checking names that are subsets of other names. 
# for(j in 1:length(to.test)) {
#   a = which(find.subsets(full = upd.two.word.names[to.test[j]], 
#                          remaining = upd.two.word.names[setdiff(to.test[-j], examined.nodes)]))
#   if (length(a) > 0) { print(j)}
# }
# 
# to.try = c(60, 71, 106, 178, 195, 197, 294, 393, 410, 419, 428, 436, 
#            466, 548, 600, 682, 699, 703, 733, 746, 828, 830, 845,
#            968, 1105, 1193, 1275, 1287, 1337, 1339, 1340, 1341, 1343, 
#            1348, 1350, 1519, 1520, 1605, 1676, 1809, 1848, 2009,
#            2136, 2209, 2273, 2291, 2310, 2317, 2380, 2388, 2421, 2422, 2423)
# for(i in 1:length(to.try)) {
# a = find.subsets(full = upd.two.word.names[to.test[to.try[i]]], 
#                  remaining = upd.two.word.names)
#  print(to.test[to.try[i]])
#  print(upd.two.word.names[a])
# }

list.to.combine[[20]] = c(1796, 2302) # Ralph Vaughan Williams, Vaughan Williams
list.to.combine[[21]] = c(682, 1718) # Owain Glyn Dwr
list.to.combine[[22]] = c(1448, 656) #King George V
list.to.combine[[23]] = c(1449, 657) # King George VI
list.to.combine[[24]] = c(89, 2301) #Van Dyck

# #print.node.results
# for(i in 1:length(list.to.combine)) {
#   print("------------------------")
#   print(cbind(Node=list.to.combine[[i]], Name=upd.two.word.names[list.to.combine[[i]]]))
# }
grep("Athenae Oxonienses", upd.two.word.names)
upd.two.word.names[2001:2599]
non.names.discovered = c(867, 987,1975, 1976,1296,1803,1804,1805,124,133,277,
                         278,341:345, 507,696:699, 684, 834 ,869,
                         998, 1462,1465,1603, 1713, 1875,2034, 2035,
                         2048, 2054, 2059, 2306)

save(list.to.combine, non.names.discovered, upd.two.word.names, file = "final.namelist.Rdata")

upd.two.word.names[find.subsets(full = "Henry Neville", remaining = upd.two.word.names)]

find.nodenum.from.name(query="Wallis", names = upd.two.word.names)
find.nodenum.from.name(query="Thomas", names = upd.two.word.names)
find.nodenum.from.name(query="Harvey", names = upd.two.word.names)
#####################
# Making data more poisson-ized: replace top3 with 4th #
pois.data.matrix = upd.data.matrix
for(i in 1:n) {
  res = sort(pois.data.matrix[,i], decreasing = TRUE)[4]
  pois.data.matrix[which(pois.data.matrix[,i] > res),i] = res
  if (i %% 10 == 0) { print(i) }
}

save(pois.data.matrix, top.matrix, file = "poissonize.Rdata")


## Trying on poissonized data. 


library(glmnet)
starts = list()
ends = list()
X = t(pois.data.matrix)

i=2
system.time((test = glmnet(t(X[-i,]),X[i,],family="poisson", 
              lambda = unique(c(seq(from=0.9, to=0.1, by = -.1),
                         seq(from=0.1, to=0.02, by = -.005), 
                         seq(from=0.02, to=0.006, by = -.0005))),
              maxit = 1000, thres = 10^(-7))))


test = list()
for(i in 301:400) {
  starts[[i]] = date()
  test[[i]] = try(glmnet(t(X[-i,]),X[i,],family="poisson", 
                         lambda = c(0.5, 0.1, 0.05, 0.021, 
                                    seq(from = 0.02, to = 0.006, by = -.0005)), 
                         maxit = 1000, thres = 3*10^(-7)) )
  ends[[i]] = date()
  print(i)
}
save(test2,starts, ends, file = "test3.Rdata")


aa = sort(sample(1:2599, size = 20))
paste(aa, collapse = ",")

#script
load("poissonize.Rdata")
library(glmnet)
X = t(pois.data.matrix)

samp = c(33,119,208,250,385,683,946,988,1076,1100,1125,1232,
         1342,1471,1770,1856,1911,2031,2080,2409)
res = list()
mod = list()
user = matrix(nrow = 20, ncol = 12)
npass = matrix(nrow = 20, ncol = 12)
df = matrix(nrow = 20, ncol = 12)
lambda.list = list()
lambda.list[[1]] = unique(c(seq(from=1, to=0.2, by = -.05)))
lambda.list[[2]] = unique(c(seq(from=1, to=0.2, by = -.1)))
lambda.list[[3]] = unique(c(seq(from=1, to=0.2, by = -.2)))
lambda.list[[4]] = unique(c(seq(from=0.8, to=0.2, by = -.3)))

lambda.list[[5]] = unique(c(seq(from=1, to=0.2, by = -.2),
                            seq(from=0.2, to=0.05, by= -.025)))
lambda.list[[6]] = unique(c(seq(from=1, to=0.2, by = -.2),
                            seq(from=0.2, to=0.05, by= -.05)))
lambda.list[[7]] = unique(c(seq(from=1, to=0.2, by = -.2),
                            0.2, 0.1, 0.05))
lambda.list[[8]] = unique(c(seq(from=1, to=0.2, by = -.2),
                            seq(from=0.2, to=0.05, by= -.15)))

lambda.list[[9]] = unique(c(seq(from=1, to=0.2, by = -.2),
                            0.2, 0.1, 0.05,
                           seq(from=0.04, to=0.006, by = -.001)))
lambda.list[[10]] = unique(c(seq(from=1, to=0.2, by = -.2),
                           0.2, 0.1, 0.05,
                           seq(from=0.04, to=0.006, by = -.002)))
lambda.list[[11]] = unique(c(seq(from=1, to=0.2, by = -.2),
                           0.2, 0.1, 0.05,0.04, 0.03,
                           seq(from=0.02, to=0.006, by = -.002)))
lambda.list[[12]] = unique(c(seq(from=1, to=0.2, by = -.2),
                           0.2, 0.1, 0.05,0.04, 0.03,
                           seq(from=0.02, to=0.006, by = -.005)))


for(i in 1:20) {
  for(j in 1:12) {
    res[[j]] = system.time((mod[[j]] = glmnet(t(X[-i,]),X[i,],family="poisson", 
                                              lambda = lambda.list[[j]],
                                              maxit = 1000, thres = 10^(-6))))
    user[i,j] = res[[j]][1]
    npass[i,j] = mod[[j]]$npasses
    df[i,j] = tail(mod[[j]]$df,1)
    print(paste(i,j, sep = ","))
  }
  
}
save(user,npass,df,file = "test1.Rdata")








# #
# > which(first.div > 30)
# [1]   86  179  258  310  510  565  571  600  613  672  758  824  985
# [14] 1043 1065 1117 1183 1201 1245 1328 1333 1346 1384 1730 1768 1813
# [27] 1814 1936 1976 2024 2099 2176 2341 2349 2371 2427 2457 2491
# testing node 86, 179 ...
# i =- 258, 310 does not run at all (very extreme: 1's, one 213.)
load("update2.datamat.Rdata")
library(glmnet)
X = t(upd.data.matrix)

i = 600
# test 600 specially
table(upd.data.matrix[,i])
model = glmnet(t(X[-i,]),X[i,],family="poisson", 
               lambda = unique(c(seq(from=.9, to=.35, by = -.05),
                                 seq(from=.35, to=.3, by = -.0005),
                                 seq(from=.3, to=.05, by = -.005),
                                 seq(from=.05, to = .02, by = -.002),
                                 seq(from=.02, to = .01, by = -.0005))), 
               maxit = 10000, thres = 10^(-6))
#######


i = 258
# test 258 specially
table(upd.data.matrix[,i])
model = glmnet(t(X[-i,]),X[i,],family="poisson", 
               lambda = unique(sort(c(seq(from=.9, to=.3, by = -.05),
                                      seq(from=1.35, to=1, by = -.005),
                                      seq(from=.3, to=.05, by = -.005),
                                      seq(from=.05, to = .02, by = -.002),
                                      seq(from=.02, to = .01, by = -.0005)),
                                    decreasing = TRUE)), 
               maxit = 10000, thres = 10^(-6))
#######

i = 310
# test 310 specially
table(upd.data.matrix[,i])
model = glmnet(t(X[-i,]),X[i,],family="poisson", 
               lambda = unique(sort(c(seq(from=.9, to=.3, by = -.05),
                                      seq(from=.7, to=.6, by = -.005),
                                      seq(from=.3, to=.05, by = -.005),
                                      seq(from=.05, to = .02, by = -.002),
                                      seq(from=.02, to = .01, by = -.0005)),
                                    decreasing = TRUE)), 
               maxit = 10000, thres = 10^(-6))
#######


i = 1
table(upd.data.matrix[,i])
time = system.time((
model = glmnet(t(X[-i,]),X[i,],family="poisson", 
               lambda = unique(sort(c(seq(from=.9, to= .8, by = -.1),
                                      seq(from=2, to=.9, by = -.05),
                                      seq(from=.9, to=.3, by = -.04),
                                      seq(from=.3, to=.21, by = -.005),
                                      seq(from=.21, to=.15, by = -.003),
                                      seq(from=.15, to=.1, by = -.002),
                                      seq(from=.1, to=.05, by = -.001) ),
                                      
#                                       seq(from=.9, to=.3, by = -.05),
#                                       seq(from=.3, to=.05, by = -.005),
#                                       seq(from=.05, to = .02, by = -.002),
#                                       seq(from=.02, to = .01, by = -.0005),
#                                       seq(from=.01, to = .005, by = -.0002),
#                                       seq(from=.005, to = .001, by = -.00005),
#                                       seq(from=.001, to = .0005, by = -.00002)),
                                    decreasing = TRUE)),
               maxit = 5000, thres = 10^(-6))
))

predict(object=model, newx = t(X[-i,]), s = 0.1, type = "response") -> a

compute.deviance = function(true, predict) {
  a = 2 * (predict - true) 
  if (true > 0) {
    a = a + 2*true*(log(true) - log(predict))
  }
  return(a)
}

deviances = rep(NA, times = length(a))
for(j in 1:length(a)) {
  deviances[j] = compute.deviance(true = X[i,j], predict = a[j])
  if (j %% 100 == 0) {print(j)}
}

which(deviances > 1) -> b
cbind(Deviance=deviances[b], Prediction=round(a[b], 4), True=upd.data.matrix[b,i])

# try timing again
unique(sort(c(seq(from=.9, to= .8, by = -.1),
              seq(from=.9, to=.3, by = -.05),
              seq(from=.3, to=.05, by = -.005),
              seq(from=.05, to = .02, by = -.002),
              seq(from=.02, to = .01, by = -.0005),
              seq(from=.01, to = .005, by = -.0002),
              seq(from=.005, to = .001, by = -.00005),
              seq(from=.001, to = .0005, by = -.00002)),
            decreasing = TRUE))


load("update2.datamat.Rdata")
library(glmnet)
X = t(upd.data.matrix)


resample = TRUE
reset = TRUE
auto.add.new.lambda = TRUE
lambda.list.start = 1 #1 unless continuing a simulation

samp.size = 50
if (resample) {
  samp = sort(sample(1:2599, size = samp.size)) # save sample
}
max.iter = c(200, 2000, 5000)

lambda.base = unique(c(seq(from=2, to=.9, by = -.05),
                       seq(from=.9, to=.3, by = -.04)))

lambda.list = list()
lambda.list[[1]] = lambda.base
lambda.list[[2]] = unique(c(lambda.base, 
                            seq(from=.3, to=.15, by = -.005),
                            seq(from=.15, to=.1, by = -.0025),
                            seq(from=.1, to=.05, by = -.002)))
lambda.list[[3]] = unique(c(lambda.list[[2]],
                            seq(from=.05, to = .02, by = -.002),
                            seq(from=.02, to = .01, by = -.001),
                            seq(from=.01, to = .005, by = -.0005)))

# lambda.list[[2]] = unique(c(lambda.base, 
#                             seq(from=.01, to = .005, by = -.00025)))
# lambda.list[[3]] = unique(c(lambda.base, 
#                             seq(from=.01, to = .005, by = -.0001)))

NN = length(lambda.list)

if (reset) {
  res = list()
  mod = list()
  user = matrix(nrow = samp.size, ncol = NN)
  npass = matrix(nrow = samp.size, ncol = NN)
  df = matrix(nrow = samp.size, ncol = NN)
  n.success = matrix(nrow = samp.size, ncol = NN)
} else {
  temp.user = user
  temp.npass = npass
  temp.df = df
  temp.n.success = n.success
  user = matrix(nrow = samp.size, ncol = NN)
  npass = matrix(nrow = samp.size, ncol = NN)
  df = matrix(nrow = samp.size, ncol = NN)
  n.success = matrix(nrow = samp.size, ncol = NN)
  user[, 1:dim(temp.user)[2]] = temp.user
  npass[, 1:dim(temp.npass)[2]] = temp.npass
  df[, 1:dim(temp.df)[2]] = temp.df
  n.success[, 1:dim(temp.n.success)[2]] = temp.n.success
}

start.time = format(Sys.time(), "%a%b%d %H%M%S")
for(i in 1:samp.size) {
  for(j in lambda.list.start:NN) {
    if (i == 1) {
      res[[j]] = list()
      mod[[j]] = list()
    }
    if (auto.add.new.lambda & j>1) {
      if (n.success[i,j-1] < length(lambda.list[[j-1]])) {
        start = max(n.success[i,j-1]-1, 1)
        end = n.success[i,j-1] + 1
        to.add = seq(from = lambda.list[[j-1]][start],
                     to = lambda.list[[j-1]][end],
                     length.out = 11)
        lamb = unique(sort(c(to.add, lambda.list[[j]]), decreasing = TRUE))
      } else {
        lamb = lambda.list[[j]]
      }
    } else {
      lamb = lambda.list[[j]]
    }
    
    res[[j]][[i]] = system.time((
      mod[[j]][[i]] = glmnet(t(X[-samp[i],]),X[samp[i],],family="poisson", 
                        lambda = lamb,
                        maxit = max.iter[j], thres = 10^(-6))))
    user[i,j] = res[[j]][[i]][1]
    npass[i,j] = mod[[j]][[i]]$npasses
    df[i,j] = tail(mod[[j]][[i]]$df,1)
    n.success[i,j] = length(mod[[j]][[i]]$df)
    print(paste(i,j, sep = ","))
  }
  
  save(samp.size, samp, max.iter, lambda.list, user,npass,df,n.success,
       file = paste("newtest", start.time ,".Rdata", sep = "") )
}

cbind(user, df, npass, n.success)

###############################################
######### Algorithm for fitting models to keep (test version)
start.time = format(Sys.time(), "%a%b%d %H%M%S")
load("update2.datamat.Rdata")
library(glmnet)
X = t(upd.data.matrix)

resample = TRUE
samp.size = 50

if (resample) {
  samp = sort(sample(1:2599, size = samp.size)) # save sample
}

max.iter = c(200, 2000, 5000)
iter.add = c(200, 1000, 2500)

lambda.list = list()
lambda.list[[1]] = unique(c(seq(from=2, to=.9, by = -.05),
                            seq(from=.9, to=.3, by = -.04)))
lambda.list[[2]] = unique(c(seq(from=.3, to=.15, by = -.005),
                            seq(from=.15, to=.1, by = -.0025),
                            seq(from=.1, to=.05, by = -.002)))
lambda.list[[3]] = unique(c(seq(from=.05, to = .02, by = -.002),
                            seq(from=.02, to = .01, by = -.001),
                            seq(from=.01, to = .005, by = -.0005)))

num.stages = length(lambda.list)

# setup storage: LISTs arelists of lists (for each stage)
res = list() #timings for models [last model time kept]
mod = list() #models: full model for all kept. 
lambdas = list() # lambdas for best kept
diag.results = array(0,dim = c(samp.size, 6, 3),
                     dimnames = list(
                       samp,
                       c("Time(sec)", "NumPasses", "Num.stageruns","DF", 
                         "succesful.lambdas", "total.lambdas"),
                       paste("Stage",1:3, sep =":"))) 

for(SA in 1:samp.size) { #sample number
  res[[SA]] = list()
  mod[[SA]] = list()
  lambdas[[SA]] = list()
  for(ST in 1:num.stages) { #stage num
    not.done = TRUE
    elapsed.time = 0
    if (ST == 1) {
    lambdas[[SA]][[ST]] = lambda.list[[1]]
    } else {
      lambdas[[SA]][[ST]] = unique(sort(c(lambda.list[[ST]], lambdas[[SA]][[ST-1]]), decreasing = TRUE))
    }
    allowed.iter = max.iter[ST]
    
    while(not.done) {
      res[[SA]][[ST]] = system.time((
        mod[[SA]][[ST]] = glmnet(t(X[-samp[SA],]),X[samp[SA],],family="poisson", 
                                 lambda = lambdas[[SA]][[ST]],
                                 maxit = allowed.iter, thres = 10^(-6)) ))
      elapsed.time = elapsed.time + res[[SA]][[ST]][1]
      diag.results[SA,1,ST] = elapsed.time
      diag.results[SA,2,ST] = mod[[SA]][[ST]]$npasses
      diag.results[SA,3,ST] = diag.results[SA,3,ST] +1
      diag.results[SA,4,ST] = tail(mod[[SA]][[ST]]$df,1)
      diag.results[SA,5,ST] = length(mod[[SA]][[ST]]$df)
      diag.results[SA,6,ST] = length(lambdas[[SA]][[ST]])
      
      if(diag.results[SA,5,ST] == diag.results[SA,6,ST]) {
        print(paste("Node",SA,"Stage",ST,"is done!"))
        not.done = FALSE
      } else {
        print(paste("Node",SA,"Stage",ST,
                    "not done: try number",diag.results[SA,3,ST]+1))
        start = max((diag.results[SA,5,ST]-1), 1)
        end = diag.results[SA,5,ST]+1
        to.add = seq(from=lambdas[[SA]][[ST]][start],
                     to=lambdas[[SA]][[ST]][end],
                     length.out = 11)
        lambdas[[SA]][[ST]] = unique(sort(c(to.add, lambdas[[SA]][[ST]]), decreasing = TRUE))
        allowed.iter = allowed.iter + iter.add[ST]
      }
    }
  }
  save(samp.size, samp, max.iter, iter.add, lambda.list, start.time,
       res, mod, lambdas, diag.results,
       file = paste("newtest", start.time ,".Rdata", sep = ""))
}

###############################################
######### Algorithm for fitting models to keep (actual version)
start.time = format(Sys.time(), "%a%b%d %H%M%S")
load("update2.datamat.Rdata")
library(glmnet)
library(multicore)
X = t(upd.data.matrix)

resample = TRUE
samp.size = 50

if (resample) {
  samp = sort(sample(1:2599, size = samp.size)) # save sample
}

max.is = dim(X)[1]
max.iter = 5000
lambdas.tokeep = c(0.5, 0.34, 0.27, 0.24, 0.22, 0.21, 0.2, 0.19, 0.18, 0.17,
                   0.16, 0.15, 0.14, 0.13, 0.12, 0.11, 0.1, 0.09, 0.08, 0.07,
                   0.06, 0.05, 0.04, 0.03, 0.02, 0.015, 0.01, 0.0085, 0.007,
                   0.006, 0.005, 0.0045, 0.004, 0.0035, 0.003, 0.0025, 0.002)

lambda.list = list()
lambda.list[[1]] = unique(c(seq(from=2, to=.9, by = -.05),
                            seq(from=.9, to=.3, by = -.04)))
lambda.list[[2]] = unique(c(seq(from=.3, to=.15, by = -.005),
                            seq(from=.15, to=.1, by = -.0025),
                            seq(from=.1, to=.05, by = -.002)))
lambda.list[[3]] = unique(c(seq(from=.05, to = .02, by = -.002),
                            seq(from=.02, to = .01, by = -.001),
                            seq(from=.01, to = .005, by = -.0005),
                            seq(from=.005, to = .002, by = -.00025)))
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))


helper <- function(i, lambda.tokeep) {
  not.done = TRUE
  lambdas = lambda.touse
  allowed.iter = max.iter
  try.number = 1
  iter.over = FALSE
  
  while(not.done) {
    mod = glmnet(t(X[-i,]), X[i,], family="poisson", 
                 lambda = lambdas, maxit = allowed.iter, thres = 10^(-6))
    n.proc = length(mod$df)
    
    if (n.proc == length(lambdas)) {
      not.done = FALSE
    } else if (try.number == 5)  { 
      iter.over = TRUE
      not.done = FALSE
    } else {
      try.number = try.number + 1
      start = max(n.proc - 1,1)
      end = n.proc + 1
      allowed.iter = round(allowed.iter + (min(lambdas)/lambdas[n.proc])^.2 * 4000, 0)
      to.add = seq(from=lambdas[start], to=lambdas[end], length.out = 11)
      lambdas = unique(sort(c(to.add, lambdas), decreasing = TRUE))
    }
  }
  mod = glmnet(t(X[-i,]), X[i,], family="poisson", 
               lambda = lambdas, maxit = allowed.iter, thres = 10^(-6))
  n.proc = length(mod$df)
  inds = match(lambda.tokeep, lambdas)
  inds = inds[inds <= n.proc]
  
  return(list(betas=mod$beta[,inds], dfs=mod$df[inds], 
              npasses=mod$npasses,n.iter = try.number))
}

test2 = system.time((test.mod2 = lapply(
              1:10, FUN = helper, lambda.tokeep = lambdas.tokeep)))
test3 = system.time((test.mod3 = mclapply(
  1:10, FUN = helper, lambda.tokeep = lambdas.tokeep, mc.cores = 2)))
save(test2, test3, test.mod2, test.mod3, 
     file = paste("newtest", start.time ,".Rdata", sep = ""))



test = system.time((test.mod = helper(i = 3, lambda.tokeep = lambdas.tokeep)))
test.mod3 = list()
a = system.time((
  for(i in 1:10) {
  test.mod3[[i]] = helper(i=i, lambda.tokeep = lambdas.tokeep)
  }
  ))

# comparison of data (newprocess vs existing)
load("update2.datamat.Rdata")
load("finaldata.Rdata")

j = sample(1:2599, size = 1)
test = upd.two.word.names[j]
names(final.data)
a = match(final.data[which(final.data$MatchName == test),2], upd.all.docnums)
b = final.data$Count[which(final.data$MatchName == test)]
cbind(a, b, upd.data.matrix[a,j])

# Setup CV 16220 rows. 
cv.samples = list()
full = 1:16220
used = NULL
for(i in 1:10) {
  cv.samples[[i]] = sort(sample(setdiff(full, used), size = 1622))
  used = c(used, cv.samples[[i]])
}
in.progress = rep(FALSE, times = 10)
progress.matrix = matrix(FALSE, nrow = 2517, ncol = 10)
save(cv.samples, in.progress, progress.matrix, file = "cv.samples.Rdata")

for(i in 1:10) {
  cvmod = list()
  save(cvmod, file = paste("cvmod",i,".Rdata", sep = ""))
}

# Run setup
which.fold = 1
num.models = 100

load("cv.samples.Rdata")
if (!in.progress[which.fold]) {
  in.progress[which.fold] = TRUE
  save(cv.samples, in.progress, progress.matrix, file = "cv.samples.Rdata")
  
  inds.tofit = which(!progress.matrix[,which.fold])
  if (length(inds.tofit) > 0) {
    inds.tofit = inds.tofit[1:min(num.models, length(inds.tofit))]
    load(paste("cvmod",which.fold,".Rdata", sep = ""))
    
    load("update3.datamat.Rdata")
    library(glmnet)
    start.time = format(Sys.time(), "%a%b%d %H%M%S")
    
    X = t(dm.datamat.red[-cv.samples[[which.fold]],])
    
    max.is = dim(X)[1]
    max.iter = 5000
    lambdas.tokeep = c(0.5, 0.34, 0.27, 0.24, 0.22, 0.21, 0.2, 0.19, 0.18, 0.17,
                       0.16, 0.15, 0.14, 0.13, 0.12, 0.11, 0.1, 0.09, 0.08, 0.07,
                       0.06, 0.05, 0.04, 0.03, 0.02, 0.015, 0.01, 0.009, 0.008, 0.007,
                       0.006, 0.005, 0.0045, 0.004, 0.0035, 0.003, 0.0025, 0.002)
    
    lambda.list = list()
    lambda.list[[1]] = unique(c(seq(from=4, to=2, by = -.1),
                                seq(from=2, to=.9, by = -.05),
                                seq(from=.9, to=.3, by = -.04)))
    lambda.list[[2]] = unique(c(seq(from=.3, to=.15, by = -.005),
                                seq(from=.15, to=.1, by = -.0025),
                                seq(from=.1, to=.05, by = -.002)))
    lambda.list[[3]] = unique(c(seq(from=.05, to = .02, by = -.002),
                                seq(from=.02, to = .01, by = -.001),
                                seq(from=.01, to = .005, by = -.0005),
                                seq(from=.005, to = .002, by = -.00025)))
    lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))
    
    
    helper <- function(i, lambda.tokeep) {
      not.done = TRUE
      lambdas = lambda.touse
      allowed.iter = max.iter
      try.number = 1
      iter.over = FALSE
      
      while(not.done) {
        mod = glmnet(t(X[-i,]), X[i,], family="poisson", 
                     lambda = lambdas, maxit = allowed.iter, thres = 10^(-6))
        n.proc = length(mod$df)
        
        if (n.proc == length(lambdas)) {
          not.done = FALSE
        } else if (try.number == 5)  { 
          iter.over = TRUE
          not.done = FALSE
        } else {
          try.number = try.number + 1
          start = max(n.proc - 1,1)
          end = n.proc + 1
          allowed.iter = round(allowed.iter + (min(lambdas)/lambdas[n.proc])^.2 * 4000, 0)
          to.add = seq(from=lambdas[start], to=lambdas[end], length.out = 11)
          lambdas = unique(sort(c(to.add, lambdas), decreasing = TRUE))
        }
      }
      mod = glmnet(t(X[-i,]), X[i,], family="poisson", 
                   lambda = lambdas, maxit = allowed.iter, thres = 10^(-6))
      n.proc = length(mod$df)
      inds = match(lambda.tokeep, lambdas)
      inds = inds[inds <= n.proc]
      
      return(list(a0s = mod$a0[inds], betas=mod$beta[,inds], dfs=mod$df[inds], 
                  npasses=mod$npasses,n.iter = try.number))
    }
    
    for(j in inds.tofit) {
      print(format(Sys.time(), "%a%b%d %H%M%S"))
      cvmod[[j]] = helper(j, lambda.tokeep = lambdas.tokeep)
      print(j)
      if (j %% 20 == 0) { 
        save(cvmod, file = paste("cvmod",which.fold,".Rdata", sep = ""))
      }
    }
    
    save(cvmod, file = paste("cvmod",which.fold,".Rdata", sep = ""))
    
    load("cv.samples.Rdata")
    in.progress[which.fold] = FALSE
    progress.matrix[inds.tofit,which.fold] = TRUE
    save(cv.samples, in.progress, progress.matrix, file = "cv.samples.Rdata")
    
  } else {
    print ("Fold done...")
  }
} else {
  print("In progress, exiting. ")
}

#################################################
## Fitting full model    
#### This uses the full dataset, but can change to smaller dataset by changing
####   the call inside helper. 
## Updated 10/9/2012

## running on hydra2, hydra7 (1, 2 respectively); eta ~ 10 pm. 

fullmodel = list()
NN = 1 # ranges 1 to 6
inds.tofit = intersect(1:425 + (425*(NN-1)), 1:2517)

load("update3sp.datamat.Rdata")
library(glmnet)
start.time = format(Sys.time(), "%a%b%d %H%M%S")
filename = paste("fullmodelsp",NN,".Rdata",sep = "")

max.iter = 3000

lambda.list = list()
lambda.list[[1]] = unique(c(seq(from=40, to=10, by = -5),
                            seq(from=10, to=6, by = -1),
                            seq(from=6, to=4, by = -.5),
                            seq(from=4, to=2, by = -.1),
                            seq(from=2, to=.9, by = -.05),
                            seq(from=.9, to=.3, by = -.04)))
lambda.list[[2]] = unique(c(seq(from=.3, to=.15, by = -.005),
                            seq(from=.15, to=.1, by = -.0025),
                            seq(from=.1, to=.05, by = -.002)))
lambda.list[[3]] = unique(c(seq(from=.05, to = .02, by = -.001),
                            seq(from=.02, to = .01, by = -.0002),
                            seq(from=.01, to = .005, by = -.0002)))
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))

helper <- function(i) {  ## version using dm.datamat.sp, no subsetting
  not.done = TRUE
  lambdas = lambda.touse
  allowed.iter = max.iter
  try.number = 1
 
  while(not.done) {
    print(paste("Trial number:", try.number))
    ti = system.time((mod = glmnet(dm.datamat.sp[,-i], dm.datamat.sp[,i],
                 family="poisson", lambda = lambdas, 
                 maxit = allowed.iter, thres = 10^(-6)) ))
    n.proc = length(mod$df)
    print(paste("Time taken:", ti[1]))
    
    if (n.proc == length(lambdas)) {
      not.done = FALSE
    } else if (try.number == 15)  { 
      not.done = FALSE
    } else {
      try.number = try.number + 1
      
      start = max(n.proc - 1,1)
      end = n.proc + 1
      allowed.iter = round(allowed.iter + (min(lambdas)/lambdas[n.proc])^.2 * 4000, 0)
      to.add = seq(from=lambdas[start], to=lambdas[end], length.out = 11)
      lambdas = unique(sort(c(to.add, lambdas), decreasing = TRUE))
    }
  }
  return(mod)
}
if (file.exists(filename)) {
  load(filename)
  already.done = 1:length(fullmodel)
  inds.tofit = setdiff(inds.tofit, fullmodel)
}
for(j in inds.tofit) {
  print(format(Sys.time(), "%a%b%d %H%M%S"))
  print(paste("Vertex Number -----", j))
  fullmodel[[j]] = helper(j)
  print(format(Sys.time(), "%a%b%d %H%M%S"))
  if (j %% 10 == 0) { 
    print("Saving...")
    save(fullmodel, file = filename)
  }
}

save(fullmodel, file = filename)

##############################################
## Combining full model separate files into one large file


# renaming files with fullmodel manually: -> fm#.Rdata
fm.full = list()
for(j in 0:5) {
  load(paste("fm",j+1,".Rdata", sep = ""))
  for(k in (425*j+1):length(fullmodel)) {
    fm.full[[k]] = fullmodel[[k]]
    print(k)
  }
}
save(fm.full, file = "fm.full.Rdata")

# processing model, looking at specific nodes. 
load("fm.full.Rdata")

# Fixing fm.full => bunch of matrices with weights as edges. 
# model.results = list of lists: 32 models. 
# model.results[[1]]$lambda, $edges 
# $edges is a 2517 * 2517 matrix; each row is the glm fit where the columns 
#   are predictors for that specific row. 
# $a0s is a vector (length 2517) which corresponds to the intercepts for
#   each of these models.

model.results = list()
for(j in 1:32) {
  model.results[[j]] = list()
  model.results[[j]]$lambda = lambdas.tokeep[j]
  model.results[[j]]$edges = Matrix(0, nrow = 2517, ncol = 2517, sparse = TRUE)
  model.results[[j]]$a0s = rep(0, times = 2517)
  for(i in 1:2517) {
    model.results[[j]]$a0s[i] = fm.full[[i]]$a0s[j]
  }
  model.results[[j]]$edges[1,] = c(0, fm.full[[1]]$betas[,j])
  model.results[[j]]$edges[2517,] = c(fm.full[[1]]$betas[,j], 0)
  for(i in 2:2516) {
    model.results[[j]]$edges[i,] = c(fm.full[[i]]$betas[1:(i-1),j], 0, fm.full[[i]]$betas[i:2516,j])
    if (i %% 100 == 0) {print(i)}
  }
  print(j)
}

node.names = dm.names.sp.fix
node.names[217] = "Charles I"

save(model.results, node.names, file = "modeldata.Rdata")



load("update3spFIX.datamat.Rdata")

exp.edge = read.csv("expertmatch.csv")

# Finding same.doc counts between columns
load("update3spFIX.datamat.Rdata")
# same.count = function(x,y) {
#   return(sum(x > 0 & y > 0))
# }
# 
# sdct.matrix = matrix(0, nrow = 2517, ncol = 2517)
# for(i in 1:2517) {
#   for(j in (i+1):2517) {
#     a = same.count(dm.datamat.sp[,i], dm.datamat.sp[,j])
#     sdct.matrix[i,j] = a
#     sdct.matrix[j,i] = a
#   }
#   print(i)
# }
# save(sdct.matrix, file = "samedoc.count.Rdata")
load("samedoc.count.Rdata")

#Finding correlations between columns
# simple.method = cor(dm.datamat.sp)
# save(simple.method, file = "cormatrix.Rdata")
# round(simple.method, 2) -> rd2
# system.time((table(rd2) -> tab2))
# save(tab2, rd2, file= "round.Rdata")
# 
# round(simple.method, 3) -> rd3
# system.time((table(rd3) -> tab3))
# save(rd3, tab3, file= "round3.Rdata")
# 
# cutoff = 0.01
# (sum(simple.method >= cutoff) - 2517)/2

load("cormatrix.Rdata")
find.matches.corr <- function(nodenum, cutoff = 0.01) {
  a = setdiff(which(simple.method[nodenum,] >= cutoff), nodenum)
  return(a)
}
# find.matches.corr(893)
# 
# system.time((simple.method = cor(dm.datamat.sp[,1:5])))

find.top.ranks <- function(ranks, k) {
  # takes ranks (where larger is higher), and takes the top k, 
  # averages out ties, zeroes others. 
  newranks = 0.0 * ranks
  ed.ranks = ranks
  i = k
  while(i > 0) {
    M = max(ed.ranks)
    loc = which(ranks == M)
    n = length(loc)
    if (n <= i) {
      newranks[loc] = mean(c(i, i-n+1))
    } else if (n > i) {
      a = i:(i-n+1)
      a[a<0] = 0
      newranks[loc] = mean(a)
    }
    i = i - n
    ed.ranks[loc] = 0
  }
  return(newranks)
}
# find.top.ranks(c(1,1,5,3,4,2,3,2,4,3,2,1,6,6,7), 5)
# 


combine.mult <- function(a,b) {
  test = sort(unique(c(a[,1], b[,1])))
  res = matrix(0, ncol = dim(a)[2] + dim(b)[2] - 1, nrow = length(test))
  res[,1] = test
  res[match(a[,1], test),2:(dim(a)[2])] = a[,-1, drop = FALSE]
  res[match(b[,1], test),(dim(a)[2]+1):(dim(res)[2])] = b[,-1, drop = FALSE]
  return(res)
}

find.rankings <- function(person.num) { # large numbers = 'closer'
  # finding for own edges: 

  res = as.matrix(exp.edge[exp.edge[,2] == person.num,c(4,5)])
  res[,2] = max(res[,2]) + 1 - res[,2]
  
  OwnEdgeWhen = matrix(0, ncol = 2, nrow = 1)
  own.model = fm.full[[person.num]]
  for(j in 32:1) {
    nonzero = which(own.model$betas[,j] != 0)
    adj.nonzero = nonzero
    adj.nonzero[adj.nonzero > person.num] = adj.nonzero[adj.nonzero > person.num] + 1
    if (length(adj.nonzero) > 0) {
    OwnEdgeWhen = rbind(OwnEdgeWhen, cbind(adj.nonzero,(33 - j)))
    }
  }
  temp = tapply(OwnEdgeWhen[,2], OwnEdgeWhen[,1], max)[-1]
  OwnEdgeWhen = cbind(NodeNum=as.numeric(names(temp)), OwnEdgeWhen=temp)
  res = combine.mult(res, OwnEdgeWhen)

  
  # own edge weights for full model
  nonzero = which(own.model$betas[,32] != 0)
  adj.nonzero = nonzero
  adj.nonzero[adj.nonzero > person.num] = adj.nonzero[adj.nonzero > person.num] + 1
  OwnEdgeWeight = cbind(adj.nonzero, OwnEdgeWeight=own.model$betas[nonzero,32])
  res = combine.mult(res, OwnEdgeWeight)
  
  # Finding other edges
  b = rep(0, times = 2517) # smallest lambda where fit
  d = rep(0, times = 2517) # beta val at 32
  for(n in 1:(person.num - 1)) {
    if (n %% 100 == 0) { print(n) }
    b[n] = 33 - min(c(which(fm.full[[n]]$betas[(person.num-1),] != 0), 33))
    d[n] = fm.full[[n]]$betas[(person.num - 1),32]
  }
  for(n in (person.num + 1):2517) {
    if (n %% 100 == 0) { print(n) }
    b[n] = 33 - min(c(which(fm.full[[n]]$betas[person.num,] != 0), 33))
    d[n] = fm.full[[n]]$betas[person.num,32]                   
  }
  b.mat = cbind(which(b != 0), b[b!=0])
  d.mat = cbind(which(d!=0), d[d!=0])
  temp = combine.mult(b.mat, d.mat)
  res = combine.mult(res, temp)
  
  # Correlation matrix
  temp = find.matches.corr(person.num)
  Simple.Corr = cbind(temp, simple.method[person.num,temp])
  res = combine.mult(res, Simple.Corr)
  
  res = cbind(dm.names.sp.fix[res[,1]], res)
  colnames(res) = c("Name", "NodeNum", "ExpertEdge", "OwnEdgeWhen", "OwnEdgeWeight",
                    "OtherEdgeWhen", "OtherEdgeWeight", "UncondCorr")
  return(res)
}

compute.top.rankings = function(nodenum) {
  a = find.rankings(nodenum)
  b = which(as.numeric(a[,3]) > 0)
  k = length(b)
  res = a
  for(j in 3:dim(a)[2]) {
    res[,j] = find.top.ranks(as.numeric(a[,j]), k)
  }
  print(apply(res[b,], 2, function(x) {mean(as.numeric(x))}))
  return(res[b,])
}

find.rankings(893) -> a
sort.by = function(dat, col) {
  return(dat[order(as.numeric(dat[,col])),])
}

# 893, 66, 1212, 2113
person.num = 2113
model.num = 30
which(fm.full[[person.num]]$betas[,32] != 0) -> nonzero

fm.full[[person.num]]$betas[nonzero, 32]
adj.nonzero = nonzero
adj.nonzero[adj.nonzero > person.num] = adj.nonzero[adj.nonzero > person.num] + 1

extras = NULL
for(j in setdiff(1:2517, person.num)) {
  if (j < person.num) {
    if (fm.full[[j]]$betas[person.num-1,model.num] != 0) {
      extras = c(extras,j)
    }
  } else {
    if (fm.full[[j]]$betas[person.num,model.num] != 0) {
      extras = c(extras,j)
    }
  }
}

dm.names.sp.fix[adj.nonzero]
dm.names.sp.fix[extras]
dm.names.sp.fix[intersect(adj.nonzero, extras)]


find.subsets <- function(full, remaining) {
  full.split = strsplit(full, split = " ")[[1]]
  remaining.split = strsplit(remaining, split = "[ |]")
  
  subset.inds = rep(FALSE, times = length(remaining))
  for(j in 1:length(remaining.split)) {
    subset.inds[j] = !any(is.na(match(full.split, remaining.split[[j]])))
  }
  return(subset.inds)
}

subset.matrix <- function(full) {
  a = which(find.subsets(full = full, remaining = dm.names.sp.fix))
  return(cbind(a, dm.names.sp.fix[a]))
}

# 
# 
# res1 = list()
# ts = format(Sys.time(), "%a%b%d %H%M%S")
# print(ts)
# for(i in 1:20) {
#   res1[[i]] = helper(i, lambda.tokeep = lambdas.tokeep)
#   print(i)
# }
# te = format(Sys.time(), "%a%b%d %H%M%S")
# print(te)
# save(ts,te, res1, file = paste("newtest", start.time ,".Rdata", sep = ""))

# 
# i = 1
# time = system.time((
#   model = glmnet(t(X[-i,]),X[i,],family="poisson", 
#                  lambda = unique(sort(c(seq(from=.9, to= .8, by = -.1),
#                                         seq(from=2, to=.9, by = -.05),
#                                         seq(from=.9, to=.3, by = -.04),
#                                         seq(from=.3, to=.21, by = -.005),
#                                         seq(from=.21, to=.15, by = -.005),
#                                         seq(from=.15, to=.1, by = -.005),
#                                         seq(from=.1, to=.05, by = -.005),
#                                         seq(from=.05, to=.01, by = -.0025),
#                                         seq(from=.01, to=.005, by = -.0005)),
#                                       decreasing = TRUE)),
#                  maxit = 5000, thres = 10^(-6))
# ))
# 
# predict(object=model, newx = t(X[-i,]), s = 0.005, type = "response") -> a
# 
# preds = as.vector(dm.datamat.red[,-1] %*% model$beta[,117,drop = FALSE]) + model$a0[117]
# head(cbind(exp(preds), a))
# sum(exp(preds) == a)
# which(exp(preds) != a) -> d
# cbind(exp(preds), a)[d,]
# 
# 
# compute.deviance = function(true, predict) {
#   a = 2 * (predict - true) 
#   if (true > 0) {
#     a = a + 2*true*(log(true) - log(predict))
#   }
#   return(a)
# }
# 
# deviances = rep(NA, times = length(a))
# for(j in 1:length(a)) {
#   deviances[j] = compute.deviance(true = X[i,j], predict = a[j])
#   if (j %% 100 == 0) {print(j)}
# }
# 
# which(deviances > 1) -> b
# cbind(Deviance=deviances[b], Prediction=round(a[b], 4), True=dm.datamat.red[b,i])





# 
# 
# test2 = system.time((test.mod2 = lapply(
#   1:10, FUN = helper, lambda.tokeep = lambdas.tokeep)))
# test3 = system.time((test.mod3 = mclapply(
#   1:10, FUN = helper, lambda.tokeep = lambdas.tokeep, mc.cores = 2)))
# save(test2, test3, test.mod2, test.mod3, 
#      file = paste("newtest", start.time ,".Rdata", sep = ""))
# 


## Data exploration for presentation

load("update3.datamat.Rdata")
row.counts = apply(dm.datamat.red > 0, 1, sum)
table(row.counts)
hist(row.counts, breaks = 50, xlab = "Number of Names/Document",
     main = "Histogram of Names/Document")
mean(row.counts)
col.counts = apply(dm.datamat.red > 0, 2, sum)


load("cv.samples.Rdata")
load("cvmod1.Rdata")


pred.dev.array = array(NA,dim = c(100,38,5))
pred.tdev.array = array(NA,dim = c(100,38,5))

k = 10 # node to predict
cv.it = 1 # cv iteration
model = cvmod[[k]]
preds = exp(dm.datamat.red[cv.samples[[cv.it]],-k] %*% model$beta + 
  matrix(model$a0s, nrow = 1622, ncol = 38, byrow = TRUE))
true = dm.datamat.red[cv.samples[[cv.it]], k]
dev = compute.deviance.vec(true = true, predict = preds[,38])

#weirdness at row 10, cv run 1, item 974
table(dm.datamat.red[9492,])
model$beta[,38]
ind.betas = which(model$beta[,38] > 0)
ind.data = which(dm.datamat.red[9492,-10] > 0)
ind.all = union(intersect(ind.betas, ind.data),union(ind.betas, ind.data))
r = cbind(Betas = model$beta[ind.all,38], Data = dm.datamat.red[9492,-10][ind.all])
sum(apply(r, 1, prod)) - 6.115993
exp(161.468)
dm.datamat.red[9492,10]
compute.deviance.vec(2, exp(161.468))


temp = cbind(preds[,38], true, dev)
inds = union(which(preds[,38] > min(tail(sort(preds[,38]), 21))),
                 which(true > min(tail(sort(true), 21))))
temp[inds,]

which(preds[,38] > 10)
cv.samples[[1]][878]
cv.samples[[1]][1028]
inds2 = which(dm.datamat.red[9997,-7] > 0)
cbind(dm.datamat.red[9997,-7], model$beta[,38])[inds2,]
sum(dm.datamat.red[9997,-7] * model$beta[,38])
exp(50.836-6.24)

table(model$beta[,38])

for(cv.it in 1:5) {
  load(paste("cvmod",cv.it,".Rdata", sep = ""))
  for(k in 1:100) {
    model = cvmod[[k]]
    preds = exp(dm.datamat.red[cv.samples[[cv.it]],-k] %*% model$beta + 
      matrix(model$a0s, nrow = 1622, ncol = 38, byrow = TRUE))
    true = dm.datamat.red[cv.samples[[cv.it]], k]
    for(j in 1:38) {
      pred.tdev.array[k,j,cv.it] = mean(compute.deviance.vec(true = true, predict = preds[,j]), trim = 0.002) # trims 3 values from each end
      pred.dev.array[k,j,cv.it] = mean(compute.deviance.vec(true = true, predict = preds[,j]))
    }
    print(k)
  }
}

save(pred.dev.array, pred.tdev.array, file = "testcv.Rdata")

load("testcv.Rdata")
devs = apply(pred.dev.array, 2, mean)
plot(lambdas.tokeep, log(devs, base = 10), xlab = "Lambda", ylab = "Log_10 Deviance",
     main = "Cross-validation subset results")

for(j in 1:20) {
  print(table(dm.datamat.red[,j])  )
}
table(dm.datamat.red[5029,])
which(dm.datamat.red[,7] == 53)
dm.names.red[7]
pred.dev.array[7,,]


tdevs = apply(pred.tdev.array, 2, mean)
plot(lambdas.tokeep, log(tdevs, base = 10), ylim = c(-1.56, -1.5), xlab = "Lambda",
     ylab = "Log_10 Deviance (trimmed)", main = "CV subset, trimmed", type = "b")
plot(lambdas.tokeep[-32:-38], tdevs[-32:-38])

which(tdevs == min(tdevs)) # 22nd
# lambda is 25th
lambdas.tokeep[c(22,25)]


load("fullmodel.Rdata")
lambda.22 = rep(0, times = 100)
lambda.25 = rep(0, times = 100)
for(j in 1:100) {
  lambda.22[j] = fullmodel[[j]]$dfs[22]
  lambda.25[j] = fullmodel[[j]]$dfs[25]
  print(j)
}

par(mfrow = c(1,2)) 
hist(lambda.22, main = "Edge counts (lambda = 0.05)", 
     xlim = c(0, 40), xlab = "Edges", ylim = c(0, 70))
hist(lambda.25, main = "Edge counts (lambda = 0.02)", 
     breaks = 20, xlab = "Edges", ylim = c(0, 70))



iggy = which(apply(pred.tdev.array > 100, 1, sum) > 0)
tdevs2 = apply(pred.tdev.array[-iggy,,], 2, mean)
plot(lambdas.tokeep[-32:-38], tdevs2[-32:-38])

plot(lambdas.tokeep, tdevs2)

sum(apply(pred.tdev.array > 10000, 1, sum) > 0)



a = compute.deviance.vec(true = true, predict = preds[,j])
b = which(a > 1)
true[b]
preds[b,j]


apply(pred.dev.array[1:10,,1], 1, mean)
plot(pred.dev.array[9,,1])



compute.deviance.vec = function(true, predict) {
  a = 2 * (predict - true) 
  b = true > 0
  a[b] = a[b] + 2*true[b]*(log(true[b]) - log(predict[b]))
  return(a)
}

for(j in 1:38) {
  dev.array[k,j,cv.it,] = compute.deviance.vec(true = true, predict = preds[,j])
  print(j)
}
apply(dev.array[1,,1,], 1, mean)

compute.deviance.vec(true = 1:8, pred = 2:9)

cvmod[[1]]$dfs



preds = as.vector(dm.datamat.red[,-1] %*% model$beta[,117,drop = FALSE]) + model$a0[117]
head(cbind(exp(preds), a))
sum(exp(preds) == a)
which(exp(preds) != a) -> d
cbind(exp(preds), a)[d,]


compute.deviance = function(true, predict) {
  a = 2 * (predict - true) 
  if (true > 0) {
    a = a + 2*true*(log(true) - log(predict))
  }
  return(a)
}

deviances = rep(NA, times = length(a))
for(j in 1:length(a)) {
  deviances[j] = compute.deviance(true = X[i,j], predict = a[j])
  if (j %% 100 == 0) {print(j)}
}

which(deviances > 1) -> b
cbind(Deviance=deviances[b], Prediction=round(a[b], 4), True=dm.datamat.red[b,i])

#testing deviance and prediction computation
x = matrix(rnorm(1000), ncol = 4)
means = 7*x[,1] - 3*x[,2] + 2*x[,3] -x[,4] + 30
y = rpois(n = 250, lambda = means)
model = glm(y ~ x, family = poisson)

names(model)
model$deviance
pred.loglin = model$coef[1] + x %*% model$coef[2:5]
preds = exp(pred.loglin)

cbind(compute.deviance.vec(true = y, predict = preds), residuals(model)^2)
compute.deviance(y[250], preds[250])
t1 = 1:25 * 1000  * c(1,0.00000000000001,1,1,0.000000001)
  t2 = 1:25 * 0.01

compute.deviance.vec (t2, t1)
# they work...


## Trying parallel computations


start.time = format(Sys.time(), "%a%b%d %H%M%S")

max.iter = 5000
lambdas.tokeep = c(0.5, 0.34, 0.27, 0.24, 0.22, 0.21, 0.2, 0.19, 0.18, 0.17,
                   0.16, 0.15, 0.14, 0.13, 0.12, 0.11, 0.1, 0.09, 0.08, 0.07,
                   0.06, 0.05, 0.04, 0.03, 0.02, 0.015, 0.01, 0.009, 0.008, 0.007,
                   0.006, 0.005)

lambda.list = list()
lambda.list[[1]] = unique(c(seq(from=40, to=10, by = -2),
                            seq(from=10, to=6, by = -.5),
                            seq(from=6, to=4, by = -.25),
                            seq(from=4, to=2, by = -.1),
                            seq(from=2, to=.9, by = -.05),
                            seq(from=.9, to=.3, by = -.04)))
lambda.list[[2]] = unique(c(seq(from=.3, to=.15, by = -.005),
                            seq(from=.15, to=.1, by = -.0025),
                            seq(from=.1, to=.05, by = -.002)))
lambda.list[[3]] = unique(c(seq(from=.05, to = .02, by = -.001),
                            seq(from=.02, to = .01, by = -.0002),
                            seq(from=.01, to = .005, by = -.0002)))
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))

load("dm.datamat.spred.Rdata")
dm.cv = dm.datamat.sp[rowSums(dm.datamat.sp>0) > 5,]

cv.samples = list()
remain = 1:11626

# 6 will never be in a training set. 
for(j in 1:10) {
  cv.samples[[j]] = sample(remain, size = 1162)
  remain = setdiff(remain, cv.samples[[j]])
}

save(dm.cv, cv.samples, remain, file = "dm.cv.Rdata")

helper <- function(x) {
  
  lambda.tokeep = intersect(lambdas.tokeep, lambda.touse)
  not.done = TRUE
  lambdas = lambda.touse
  allowed.iter = max.iter
  try.number = 1
  iter.over = FALSE
  
  while(not.done) {
    print(paste("Trial number:", try.number))
    ti = system.time((mod = glmnet(dm.datamat[,-x], dm.datamat[,x],
                                   family="poisson", lambda = lambdas, 
                                   maxit = allowed.iter, thres = 10^(-6)) ))
    n.proc = length(mod$df)
    print(paste("Time taken:", ti[1]))
    
    if (n.proc == length(lambdas)) {
      not.done = FALSE
    } else if (try.number == 8)  { 
      iter.over = TRUE
      not.done = FALSE
    } else {
      try.number = try.number + 1
      
      start = max(n.proc - 1,1)
      end = n.proc + 1
      allowed.iter = round(allowed.iter + (min(lambdas)/lambdas[n.proc])^.2 * 4000, 0)
      to.add = seq(from=lambdas[start], to=lambdas[end], length.out = 11)
      lambdas = unique(sort(c(to.add, lambdas), decreasing = TRUE))
    }
  }
  
  inds = match(lambda.tokeep, lambdas)
  inds = inds[inds <= n.proc]
  
  return(list(a0s = mod$a0[inds], betas=mod$beta[,inds], dfs=mod$df[inds], 
              npasses=mod$npasses,n.iter = try.number))
}

save(max.iter, lambdas.tokeep, lambda.touse, helper,
     file = "params.Rdata")

# pw fOgvo61C6Yw9H6h

trialnum = 1
  clust1 = makeSOCKcluster(c("node68","node69","node70", "node71", "node72",
                             "node73", "node74", "node75", "node76", "node77",
                             "node78","node79"))
clust1 = makeSOCKcluster(c("node68","node69","node70", "node71", "node72",
                           "node73", "node74", "node75", "node76"))

clust1 = makeSOCKcluster(c("node81","node82","node83", "node84", "node85",
                           "node86", "node87", "node88", "node89"))

results = list()
  
  # stopCluster(clust1)


clusterEvalQ(clust1, library(glmnet))
clusterEvalQ(clust1, load("dm.cv.Rdata"))
clusterEvalQ(clust1, load("params.Rdata"))

load("params.Rdata")
load("dm.cv.Rdata")

glm.x = function(x, cv.inds) {
  return(try(glmnet(dm.cv[cv.inds,-x], dm.cv[cv.inds,x],
                family="poisson", lambda = lambda.touse, 
                maxit = 2000 , thres = 10^(-6))))
}

#Iter 1
j = 51:80
snow.time(( results[j] = parLapply(clust1, x = j, fun = glm.x, cv.inds = cv.I) ))


trialnum = 1
for(k in 1:100) {
  j = intersect((length(results) + 1 ): (length(results) + 27), 1:2517)
  print(j)
  print(date())
  results[j] = parLapply(clust1, x = j, fun = glm.x, cv.inds = cv.I)
}

save(results, file = paste("cv.trial",trialnum,".Rdata", sep = ""))


load("cv.trial8.Rdata")
  trialnum = 8
  cv.I = setdiff(1:11626,cv.samples[[trialnum]])
  
  for(k in 1:100) {
    j = intersect((length(results) + 1 ): (length(results) + 24), 1:2517)
    print(trialnum)
    print(j)
    print(date())
    print(snow.time((results[j] = parLapply(clust1, x = j, fun = glm.x, cv.inds = cv.I))) )
  }
  save(results, file = paste("cv.trial",trialnum,".Rdata", sep = ""))



load("cv.trial9.Rdata")
trialnum = 9
cv.I = setdiff(1:11626,cv.samples[[trialnum]])
for(k in 1:100) {
  
  j = intersect((length(results) + 1 ): (length(results) + 18), 1:2517)
  print(trialnum)
  print(j)
  print(date())
  print(snow.time((results[j] = parLapply(clust1, x = j, fun = glm.x, cv.inds = cv.I))) )
}
save(results, file = paste("cv.trial",trialnum,".Rdata", sep = ""))

load("cv.trial7.Rdata")
trialnum = 7
cv.I = setdiff(1:11626,cv.samples[[trialnum]])
for(k in 1:20) {
  
  j = intersect((length(results) + 1 ): (length(results) + 18), 1:2517)
  print(trialnum)
  print(j)
  print(date())
  print(snow.time((results[j] = parLapply(clust1, x = j, fun = glm.x, cv.inds = cv.I))) )
}
save(results, file = paste("cv.trial",trialnum,".Rdata", sep = ""))


# re-running non-finished trials


find.done = function(x) {
  done = rep(-1, times = length(x))
  for(i in 1:length(x)) {
    if (length(x[[i]]) == 12) {
      done[i] = (x[[i]]$lambda[length(x[[i]]$df)] == 0.005)
    }
  }
  return(done)
}

glm.x.re <- function(x, cv.inds, prog) {
  start = prog[[x]]$lambda[length(prog[[x]]$lambda) - 1]
  matches = match(prog[[x]]$lambda,lambda.touse)
  end = lambda.touse[max(matches, na.rm = TRUE) + 1]
  
  to.add = seq(from=start, to=end, length.out = 11)
  new.lambda = unique(sort(c(to.add, prog[[x]]$lambda, lambda.touse), decreasing = TRUE))
  maxit2 = 2000 * (prog[[x]]$iters+1)
  #return(list(new.lambda, maxit2))
  return(try(glmnet(dm.cv[cv.inds,-x], dm.cv[cv.inds,x],
                    family="poisson", lambda = new.lambda, 
                    maxit = maxit2 , thres = 10^(-6))))
}

## For 12 nodes
trialnum = 10
load(paste("cv.trial",trialnum,".Rdata", sep = ""))
cv.I = setdiff(1:11626,cv.samples[[trialnum]])
nodes=12

for(run in 2:10) {
  is.done = find.done(results)
  to.do = which(is.done == 0)
  prog = list()
  
  for(j in 1:2517) {
    if (is.done[j] == 0) {
      prog[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
    }
  }
  
  to.run = NULL
  for(i in 1:100) {
    prog[to.run] = paste(run ,"<- iteration done")
    to.run = to.do[intersect((nodes*(i-1)+1):(nodes*(i)), 1:length(to.do))]
    to.run = to.run[!is.na(to.run)]
    if (length(to.run) > 0) {
      print(paste("CV fold:",trialnum, "re-iter:", run))
      print(to.run)
      print(date())
      print(snow.time((results[to.run] = parLapply(clust1, x = to.run, 
                                                   fun = glm.x.re, 
                                                   cv.inds = cv.I,
                                                   prog = prog))) )
    }
  }
  save(results,prog,is.done, file = paste("cv.trial.it",trialnum,".Rdata", sep = ""))
}
save(results,prog,is.done, file = paste("cv.trial.it",trialnum,".Rdata", sep = ""))



## For 9 nodes
trialnum = 3
load(paste("cv.trial",trialnum,".Rdata", sep = ""))
cv.I = setdiff(1:11626,cv.samples[[trialnum]])
nodes=9

for(run in 2:10) {
  is.done = find.done(results)
  to.do = which(is.done == 0)
  prog = list()
  
  for(j in 1:2517) {
    if (is.done[j] == 0) {
      prog[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
    }
  }
  
  to.run = NULL
  for(i in 1:100) {
    prog[to.run] = paste(run ,"<- iteration done")
    to.run = to.do[intersect((nodes*(i-1)+1):(nodes*(i)), 1:length(to.do))]
    to.run = to.run[!is.na(to.run)]
    if (length(to.run) > 0) {
      print(paste("CV fold:",trialnum, "re-iter:", run))
      print(to.run)
      print(date())
      print(snow.time((results[to.run] = parLapply(clust1, x = to.run, 
                                                   fun = glm.x.re, 
                                                   cv.inds = cv.I,
                                                   prog = prog))) )
    }
  }
  save(results,prog,is.done, file = paste("cv.trial.it",trialnum,".Rdata", sep = ""))
}
save(results,prog,is.done, file = paste("cv.trial.it",trialnum,".Rdata", sep = ""))


## run zeros on hydras, and any unfinished details...
find.done = function(x) {
  done = rep(-1, times = length(x))
  for(i in 1:length(x)) {
    if (length(x[[i]]) == 12) {
      done[i] = (x[[i]]$lambda[length(x[[i]]$df)] == 0.005)
    }
  }
  return(done)
}

glm.x.all <- function(x, cv.inds, tryn) {
  if (tryn > 1) {
    start = results[[x]]$lambda[length(results[[x]]$lambda) - 1]
    matches = match(results[[x]]$lambda,lambda.touse)
    end = lambda.touse[max(matches, na.rm = TRUE) + 1]
    
    to.add = seq(from=start, to=end, length.out = 11)
    new.lambda = unique(sort(c(to.add, results[[x]]$lambda, lambda.touse), decreasing = TRUE))
    maxit2 = 2000 * tryn
    #return(list(new.lambda, maxit2))
    return(try(glmnet(dm.cv[cv.inds,-x], dm.cv[cv.inds,x],
                      family="poisson", lambda = new.lambda, 
                      maxit = maxit2 , thres = 10^(-6)), silent = TRUE))
  } else {
    return(try(glmnet(dm.cv[cv.inds,-x], dm.cv[cv.inds,x],
                      family="poisson", lambda = lambda.touse, 
                      maxit = 2000 , thres = 10^(-6)), silent = TRUE))
    
  }
}

load("params.Rdata")
load("dm.cv.Rdata")
library(glmnet)

#trialnum = 1
for(trialnum in 4:10) {
  print(paste("TRIALNUM---------------", trialnum))
  load(paste("cv.trial.it",trialnum,".Rdata", sep = ""))
  cv.I = setdiff(1:11626,cv.samples[[trialnum]])
  tryerr = 0
  
  is.done = find.done(results)
  to.do = which(is.done != 1)
  for(i in to.do) {
    print(paste("Node",i," ::: " ,which(i == to.do), "of", length(to.do)))
    print(date())
    not.done = TRUE
    if (is.done[i] == 0) { tryn = 2 } else {tryn = 1}
    while(not.done) {
      print(paste("Trial", tryn))
      print(date())
      results[[i]] = glm.x.all(x = i, cv.inds = cv.I, tryn = tryn)
      not.done = (find.done(results[i]) != 1)
      tryn = tryn+1
      if (class(results[[i]]) == "try-error") {
        tryerr = 1
        not.done = FALSE
      }
    }
  }
  if(tryerr == 1) {print("ERROR FITTING AT LEAST ONE MODEL")}
  save(results, file = paste("cv.trial.it",trialnum,".Rdata", sep = ""))
}
# Finding unique lambdas
load("cv.trial.it1.Rdata")
is.done = function(x) {
  done = rep(FALSE, times = length(x))
  for(i in 1:length(x)) {
    if (length(x[[i]]) == 12) {
      done[i] = TRUE
    }
  }
  return(done)
}
temp = sample(1:2517[is.done(results)], size = 200) 
lambdas = results[[1]]$lambda
for(j in temp) {
  lambdas = intersect(lambdas, results[[j]]$lambda)
  print(j)
}
full.lambdas = lambdas
# finding unfinished numbers
bad.trials = NULL
for(j in 1:10) {
  load(paste("cv.trial.it",j,".Rdata", sep = ""))
  bad.trials = c(bad.trials,which(!is.done(results)))
}
bad.trials = unique(bad.trials)
save(bad.trials, full.lambdas, file = "cvproc.Rdata")

############## Simluation code
# lambdas to predict for
lambda.tosim = c(0.5, 0.3, 0.25, 0.2, 0.18, 0.16, 0.14, 0.12, 0.1,
                 0.09, 0.08, 0.07, 0.06, 0.05, 0.045, 0.04, 0.035, 0.03,
                 0.025, 0.02, 0.018, 0.016, 0.014, 0.012, 0.01, 0.009,
                 0.008, 0.007, 0.006, 0.005)

# computing deviances
library(glmnet)
compute.deviance.vec = function(true, predict) {
  a = 2 * (predict - true) 
  b = true > 0
  a[b] = a[b] + 2*true[b]*(log(true[b]) - log(predict[b]))
  return(a)
}

load("cvproc.Rdata")
av.errors = array(NA, dim = c(2517,10,30))
load("dm.cv.Rdata")
for(j in 1:10) {
  print(paste("---------FOLD NUMBER", j))
  load(paste("cv.trial.it",j,".Rdata", sep = ""))
  to.pred.ind = sort(c(cv.samples[[j]], remain))
  ln = length(to.pred.ind)
  
  for(n in setdiff(1:2517, bad.trials)) {
    print(paste("Node number:", n))
    lambda.loc = match(lambda.tosim, results[[n]]$lambda)
    as = results[[n]]$a0[lambda.loc]
    bs = results[[n]]$beta[, lambda.loc]
    
    preds = exp(dm.cv[to.pred.ind,-n] %*% bs + matrix(as, nrow = ln, ncol = 30, byrow = TRUE))
    true = dm.cv[to.pred.ind, n]
    
    for(k in 1:30) {
      av.errors[n,j,k] = mean(compute.deviance.vec(true = true, predict = preds[,k]))
    }
    
  }
}
save(av.errors, file = "avdevs.Rdata")

### plotting
lambda.tosim = c(0.5, 0.3, 0.25, 0.2, 0.18, 0.16, 0.14, 0.12, 0.1,
                 0.09, 0.08, 0.07, 0.06, 0.05, 0.045, 0.04, 0.035, 0.03,
                 0.025, 0.02, 0.018, 0.016, 0.014, 0.012, 0.01, 0.009,
                 0.008, 0.007, 0.006, 0.005)
apply(av.errors, 3, mean, na.rm = TRUE) -> a

plot(lambda.tosim, a)
plot(lambda.tosim, log(a))
sum(av.errors > 0, na.rm = TRUE)
sum(av.errors > 10^15, na.rm = TRUE) / 753000

match(lambda.tosim, lambdas.tokeep)
sum(av.errors[,,30] > 10^5, na.rm = 1) / sum(av.errors[,,30] > 0, na.rm = 1)
sum(pred.dev.array[,32,] > 10^5) / sum(pred.dev.array[,32,] > 0)

apply(av.errors, 1 ,mean) -> b
which(log(b, base = 10) > 60)
load("update3spFIX.datamat.Rdata")
dm.names.sp.fix[which(log(b, base = 10) > 60)]
dm.cv = dm.datamat.spred[rowSums(dm.datamat.spred > 0) > 5,]

par(mfrow = c(3,1))
hist(log(colSums(dm.cv > 0), base = 10), xlim = c(0, 3.5),
     main = "Log(nonempty.cells/column), All nodes")
hist(log(colSums(dm.cv > 0)[which(log(b, base = 10) > 30)], base = 10), 
     xlim = c(0, 3.5), main = "Log(nonempty.cells/column), Nodes: mean > 10^30")
hist(log(colSums(dm.cv > 0)[which(log(b, base = 10) > 60)], base = 10), 
     xlim = c(0, 3.5), main = "Log(nonempty.cells/column), Nodes: mean > 10^60")

par(mfrow = c(3,1))
hist(log(colSums(dm.cv), base = 10), xlim = c(0, 3.5),
     main = "Log(nonempty.cells/column), All nodes")
hist(log(colSums(dm.cv)[which(log(b, base = 10) > 30)], base = 10), 
     xlim = c(0, 3.5), main = "Log(nonempty.cells/column), Nodes: mean > 10^30")
hist(log(colSums(dm.cv)[which(log(b, base = 10) > 60)], base = 10), 
     xlim = c(0, 3.5), main = "Log(nonempty.cells/column), Nodes: mean > 10^60")

load("cv.trial.it1.Rdata")
which(av.errors[,1,30] > 10^30)
for(j in which(av.errors[,1,30] > 10^30)){
  print(results[[j]]$df[results[[j]]$lambda == 0.005])
}

n = 241
lambda.loc = which(results[[n]]$lambda == 0.005)
as = results[[n]]$a0[lambda.loc]
bs = results[[n]]$beta[, lambda.loc]

to.pred.ind = sort(c(cv.samples[[1]], remain))
ln = length(to.pred.ind)
preds = exp(dm.cv[to.pred.ind,-n] %*% bs + matrix(as, nrow = ln, ncol = 1, byrow = TRUE))
true = dm.cv[to.pred.ind, n]
compute.deviance.vec = function(true, predict) {
  a = 2 * (predict - true) 
  b = true > 0
  a[b] = a[b] + 2*true[b]*(log(true[b]) - log(predict[b]))
  return(a)
}


# preds = exp(dm.datamat.red[cv.samples[[cv.it]],-k] %*% model$beta + 
#   matrix(model$a0s, nrow = 1622, ncol = 38, byrow = TRUE))
# true = dm.datamat.red[cv.samples[[cv.it]], k]
# 
# compute.deviance.vec = function(true, predict) {
#   a = 2 * (predict - true) 
#   b = true > 0
#   a[b] = a[b] + 2*true[b]*(log(true[b]) - log(predict[b]))
#   return(a)
# }

#################################### random stuff


snow.time(( a = 
  clusterEvalQ(clust1, ((
    mod = glmnet(dm.cv[,-27], dm.cv[,27],
                 family="poisson", lambda = lambda.touse, 
                 maxit = 5000 , thres = 10^(-6)) )) )
))


clusterEvalQ(clust1, ((subset1 = dm.datamat[which(dm.datamat[,1] > 0),])) )
clusterEvalQ(clust1, ((subset1 = subset1[,which(subset1[1,] > 0)])) )
clusterEvalQ(clust1, ((
mod = glmnet(subset1[,-1], subset1[,1],
             family="poisson", lambda = lambda.touse, 
             maxit = 5000 , thres = 10^(-6)) )) ) 
 


snow.time(parLapply(clust1, x = 1:4, fun = helper) -> temp )

parSapply(clust1, X = 1:4, fun = helper.test) -> temp



parSapply(clust1, X = 1:4, fun = helper.test) -> temp



for(j in inds.tofit) {
  
  
  print(format(Sys.time(), "%a%b%d %H%M%S"))
  print(paste("Vertex Number -----", j))
  fullmodel[[j]] = helper(j, lambda.tokeep = lambdas.tokeep)
  print(format(Sys.time(), "%a%b%d %H%M%S"))
  
  
  if (j %% 20 == 0) { 
    print("Saving...")
    save(fullmodel, file = filename)
  }
}

save(fullmodel, file = filename)



## Time testing: 
# 
load("update3spFIX.datamat.Rdata")
load("timedata.Rdata")
library(glmnet)

max.iter = 5000
lambdas.tokeep = c(0.5, 0.34, 0.27, 0.24, 0.22, 0.21, 0.2, 0.19, 0.18, 0.17,
                   0.16, 0.15, 0.14, 0.13, 0.12, 0.11, 0.1, 0.09, 0.08, 0.07,
                   0.06, 0.05, 0.04, 0.03, 0.02, 0.015, 0.01, 0.009, 0.008, 0.007,
                   0.006, 0.005)

lambda.list = list()
lambda.list[[1]] = unique(c(seq(from=40, to=10, by = -2),
                            seq(from=10, to=6, by = -.5),
                            seq(from=6, to=4, by = -.25),
                            seq(from=4, to=2, by = -.1),
                            seq(from=2, to=.9, by = -.05),
                            seq(from=.9, to=.3, by = -.04)))
lambda.list[[2]] = unique(c(seq(from=.3, to=.15, by = -.005),
                            seq(from=.15, to=.1, by = -.0025),
                            seq(from=.1, to=.05, by = -.002)))
lambda.list[[3]] = unique(c(seq(from=.05, to = .02, by = -.001),
                            seq(from=.02, to = .01, by = -.0002),
                            seq(from=.01, to = .005, by = -.0002)))
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))


helper <- function(x, dat) {
  
  lambda.tokeep = intersect(lambdas.tokeep, lambda.touse)
  not.done = TRUE
  lambdas = lambda.touse
  allowed.iter = max.iter
  try.number = 1
  iter.over = FALSE
  
  while(not.done) {
    print(paste("Trial number:", try.number))
    ti = system.time((mod = glmnet(dat[,-x], dat[,x],
                                   family="poisson", lambda = lambdas, 
                                   maxit = allowed.iter, thres = 10^(-6)) ))
    n.proc = length(mod$df)
    print(paste("Time taken:", ti[1]))
    
    if (n.proc == length(lambdas)) {
      not.done = FALSE
    } else if (try.number == 8)  { 
      iter.over = TRUE
      not.done = FALSE
    } else {
      try.number = try.number + 1
      
      start = max(n.proc - 1,1)
      end = n.proc + 1
      allowed.iter = round(allowed.iter + (min(lambdas)/lambdas[n.proc])^.2 * 4000, 0)
      to.add = seq(from=lambdas[start], to=lambdas[end], length.out = 11)
      lambdas = unique(sort(c(to.add, lambdas), decreasing = TRUE))
    }
  }
  
  inds = match(lambda.tokeep, lambdas)
  inds = inds[inds <= n.proc]
  
  return(list(a0s = mod$a0[inds], betas=mod$beta[,inds], dfs=mod$df[inds], 
              npasses=mod$npasses,n.iter = try.number))
}


simulate.data <- function(data, nrow, ncol) {
  # nrow = target # rows
  # ncol = target # cols
  # if nrow < data.rows, no row simulation done (same w/ cols)
  # new rows = average of 2 random rows, rounded
  # new cols = average of 2 random cols, rounded
  target.data = matrix(0, nrow = nrow, ncol = ncol)
  rowv = 1:dim(data)[1]
  colv = 1:dim(data)[2]
  if (ncol <= dim(data)[2]) {
    cols.touse = sample(colv, size = ncol)
    if (nrow <= dim(data)[1]) {
      rows.touse = sample(rowv, size = nrow)
      target.data = data[rows.touse, cols.touse]
    } else {
      target.data[rowv,] = data[,cols.touse]
      print("Expanding Rows")
      for(j in (dim(data)[1] + 1):nrow) {
        samp = sample(rowv, size = 2)
        target.data[j,] = round(apply(target.data[samp,],2,mean)/1.98)
        if (j %% 5 == 0) {print(j)}
      }
    }
  } else { # ncol is too big
    if (nrow <= dim(data)[1]) {
      rows.touse = sample(rowv, size = nrow)
      target.data[,colv] = data[rows.touse,]
      print("Expanding Cols")
      for(j in (dim(data)[2] + 1):ncol) {
        samp = sample(colv, size = 2)
        target.data[,j] = round(apply(target.data[,samp],1,mean)/1.98)
        if (j %% 5 == 0) {print(j)}
      }
    } else {
      # expand cols to correct
      target.data[rowv, colv] = data
      print("Expanding Cols")
      for(j in (dim(data)[2] + 1):ncol) {
        samp = sample(colv, size = 2)
        target.data[,j] = round(apply(target.data[,samp],1,mean)/1.98)
        if (j %% 5 == 0) {print(j)}
      }
      # expand rows to correct
      print("Expanding Rows")
      for(j in (dim(data)[1] + 1):nrow) {
        samp = sample(rowv, size = 2)
        target.data[j,] = round(apply(target.data[samp,],2,mean)/1.98)
        if (j %% 5 == 0) {print(j)}
      }
    }
  }
  return(target.data)
}

crmat = matrix(0, ncol = 2, nrow = 19)
timings = matrix(-1, ncol = 25, nrow = 19)

crmat[6:19, 1] = rep(c(500, 1500, 2000, 2500, 3000, 4000, 5000), each = 2)
crmat[6:19, 2] = rep(c(10000, 30000), times = 7)
crmat[1:5, 1] = 1000
crmat[1:5, 2] = c(10000, 20000, 30000, 40000, 60000)

# Rest of trials
for(trs in 1:19) {
  print(paste("Overall trial # = ", trs))
  timings = rbind(timings, 0)
  
  print("Data simulation")
  dm.red = simulate.data(data = dm.datamat.sp, 
              nrow = crmat[trs,2], ncol = crmat[trs,1])
  
  print("Model fitting")
  samp = sample(1:dim(dm.red)[2], size = 25)
  for(j in 1:25) {
    print(paste("Sample TRIALNUMBER: ----", j))
    timings[trs,j] = system.time(helper(samp[j], dm.red))[1]
  }
  save(timings, file = "timedata.Rdata")
}


all = cbind(crmat, apply(timings, 1, mean))
colnames(all) = c("NumCols", "NumRows", "Mean Runtime")

ss = which(all[,2] == 10000)
plot(all[ss,1], all[ss,3], xlim = c(0, 5000), ylim = c(0, 100),
     main = "Runtime vs. Number of Columns", ylab = "Mean Runtime for 1 node (sec)",
     xlab = "Number of Columns (Vertices)")
abline(b = 0, a = 1)
abline(lm(all[ss,3]~all[ss,1] + 0))
ss2 = which(all[,2] == 30000)
points(all[ss2,1], all[ss2,3], xlim = c(0, 5000), col = "blue")
abline(lm(all[ss2,3]~all[ss2,1] + 0), col = "blue")

plot( all[,1]*all[,2], all[,3], main = "Mean Runtime vs. NCol*NRow",
      ylab = "Mean Runtime for 1 node (sec)", xlab = "NCol*NRow")
abline(lm(all[,3] ~ I(all[,1]*all[,2]) + 0))

#####################################################33
## Simple CV for 25 random rows
################################################## 10/15
load("update3sp500.datamat.Rdata")
library(glmnet)


rowSums(dm.datamat.sp > 0) -> a
dm.cv = dm.datamat.sp[a>2,]
remain = 1:dim(dm.cv)[1]
cv.samples = list()
N = length(remain)

# some will never be in a training set. 
for(j in 1:10) {
  cv.samples[[j]] = sample(remain, size = floor(N/10))
  remain = setdiff(remain, cv.samples[[j]])
}

nodes.samp = sample(1:2521, size = 25)
results = list()
for(j in 1:10) {
  results[[j]] = list()
}

save(cv.samples, remain, nodes.samp, results, file = "500cv.Rdata")

load("update3sp500.datamat.Rdata")
library(glmnet)

rowSums(dm.datamat.sp > 0) -> a
dm.cv = dm.datamat.sp[a>2,]
N = dim(dm.cv)[1]

load("500cv.Rdata")
lambda.list = list()
lambda.list[[1]] = unique(c(seq(from=40, to=10, by = -2),
                            seq(from=10, to=6, by = -.5),
                            seq(from=6, to=4, by = -.25),
                            seq(from=4, to=2, by = -.1),
                            seq(from=2, to=.9, by = -.05),
                            seq(from=.9, to=.3, by = -.04)))
lambda.list[[2]] = unique(c(seq(from=.3, to=.15, by = -.005),
                            seq(from=.15, to=.1, by = -.0025),
                            seq(from=.1, to=.05, by = -.002)))
lambda.list[[3]] = unique(c(seq(from=.05, to = .02, by = -.001),
                            seq(from=.02, to = .01, by = -.0002),
                            seq(from=.01, to = .005, by = -.0002),
                            seq(from=.005, to = .003, by = -.0001),
                            seq(from=.003, to = .002, by = -.00005),
                            seq(from=.002, to = .001, by = -.00002)))
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))

glm.x.all <- function(fold, x, cv.inds, tryn) {
  if (tryn > 1) {
    start = results[[fold]][[x]]$lambda[length(results[[fold]][[x]]$lambda) - 1]
    matches = match(results[[fold]][[x]]$lambda,lambda.touse)
    end = lambda.touse[max(matches, na.rm = TRUE) + 1]
    
    to.add = seq(from=start, to=end, length.out = 11)
    new.lambda = unique(sort(c(to.add, results[[fold]][[x]]$lambda, lambda.touse), decreasing = TRUE))
    maxit2 = 2000 * tryn
    #return(list(new.lambda, maxit2))
    return(try(glmnet(dm.cv[cv.inds,-x], dm.cv[cv.inds,x],
                      family="poisson", lambda = new.lambda, 
                      maxit = maxit2 , thres = 10^(-6)), silent = TRUE))
  } else {
    return(try(glmnet(dm.cv[cv.inds,-x], dm.cv[cv.inds,x],
                      family="poisson", lambda = lambda.touse, 
                      maxit = 2000 , thres = 10^(-6)), silent = TRUE))
  }
}

find.done = function(x) {
  done = rep(-1, times = length(x))
  for(i in 1:length(x)) {
    if (length(x[[i]]) == 12) {
      done[i] = (x[[i]]$lambda[length(x[[i]]$df)] == 0.001)
    }
  }
  return(done)
}

for(trialnum in 1:10) {
  print(paste("TRIALNUM---------------", trialnum))
  cv.I = setdiff(1:N,cv.samples[[trialnum]])
  to.do = nodes.samp
  for(i in to.do[1:10]) {
    print(paste("Node",i," ::: " ,which(i == to.do), "of", length(to.do)))
    print(date())
    tryn = 1
    not.done = TRUE
    while(not.done) {
      print(paste("Trial", tryn))
      print(date())
      results[[trialnum]][[i]] = glm.x.all(fold = trialnum, x = i, cv.inds = cv.I, tryn = tryn)
      not.done = (find.done(results[[trialnum]][i]) != 1)
      tryn = tryn+1
      if (class(results[[trialnum]][[i]]) == "try-error") {
        tryerr = 1
        not.done = FALSE
      }
    }
  }
  save(cv.samples, remain, nodes.samp, results, file = "500cv2.Rdata")
}

## Taking cv results, processing for deviances
devs = array(0, dim = c(10, 10, 36)) #devs[node,fold,lambda]
compute.deviance.vec = function(true, predict) {
  a = 2 * (predict - true) 
  b = true > 0
  a[b] = a[b] + 2*true[b]*(log(true[b]) - log(predict[b]))
  return(a)
}
load("500cv2.Rdata")
load("update3sp500.datamat.Rdata")
library(glmnet)
rowSums(dm.datamat.sp > 0) -> a
dm.cv = dm.datamat.sp[a>2,]
lambdas.tokeep = c(0.5, 0.34, 0.27, 0.24, 0.22, 0.21, 0.2, 0.19, 0.18, 0.17,
                   0.16, 0.15, 0.14, 0.13, 0.12, 0.11, 0.1, 0.09, 0.08, 0.07,
                   0.06, 0.05, 0.04, 0.03, 0.02, 0.015, 0.01, 0.009, 0.008, 0.007,
                   0.006, 0.005, 0.004, 0.003, 0.002, 0.001)

for(k in 1:10) {
  means = matrix(0, nrow = 10, ncol = 36)
  for(j in 1:10) {
    i = nodes.samp[k]
    lambda.loc = match(lambdas.tokeep, results[[j]][[i]]$lambda)
    as = results[[j]][[i]]$a0[lambda.loc]
    bs = results[[j]][[i]]$beta[, lambda.loc]
    
    to.pred.ind = sort(c(cv.samples[[j]], remain))
    ln = length(to.pred.ind)
    preds = exp(dm.cv[to.pred.ind,-i] %*% bs + matrix(as, nrow = ln, ncol = 36, byrow = TRUE))
    true = dm.cv[to.pred.ind,i]
    for(m in 1:36) {
      means[j,m] = mean(compute.deviance.vec(true = true, predict = preds[,m]))
    }
    print(j)
  }
  devs[k,,] = means
  print(k)
}
save(devs, file = "cvres.new.Rdata")

## Attempting to construct own-bio docs
load("ODNBdatav1.Rdata")
names = gsub("<.*?>", "", ODNB.names)
save(names, ODNB.nums, ODNB.nums.inv, file = "names.raw.Rdata")

load("finaldata2.Rdata")
tapply(final.data$Count, final.data$DocNum,  sum) -> doc.totals
save(doc.totals, file = "doc.totals.Rdata")

load("names.raw.Rdata")
load("doc.totals.Rdata")
load("update3.datamat.Rdata")
find.match <- function(target, name.vec) {
  split = strsplit(target, split = " ")[[1]]
  res = 1:58265
  for(j in 1:length(split)) {
    res = intersect(res, grep(gsub("[.]", "", split[j]), name.vec))
  }
  return(res)
}

rowsums = rowSums(dm.datamat)


find.count <- function(ind, name.vec) {
  #relies on existence of dm.docnums, dm.datamat, ODNB.nums
  # searches name 'ind', so needs dm.names
  # depends on rowsums
  if (length(grep("[|]", dm.names[ind])) > 0) {
    mult = strsplit(dm.names[ind], split = "[|]")[[1]]
    res = NULL
    for(k in 1:length(mult)) {
      temp = find.match(target = mult[k], name.vec= name.vec)
      res = c(res, temp)
    }
    res = sort(unique(res))
    temp = res
  } else {
    temp = find.match(target = dm.names[ind], name.vec= name.vec)
  }
  ## next line is usually off
  # temp = unique(c(temp, NA, ODNB.nums.inv[dm.docnums[intersect(which(dm.datamat[,ind] > 5), which(dm.datamat[,ind]/rowsums > 0.25))]]))
  to.search = ODNB.nums[temp]
  if (length(temp) > 0) {
    aa = match(to.search, dm.docnums)
    if (sum(!is.na(aa)) > 0) {
      return(cbind(to.search, name.vec[temp], dm.datamat[aa,ind],
                   apply(dm.datamat[aa,,drop = FALSE], 1, sum),
                   doc.totals[match(to.search, names(doc.totals))])) 
    } else {
      return(cbind(to.search, name.vec[temp], dm.datamat[aa,ind],
                   NA,
                   doc.totals[match(to.search, names(doc.totals))]))
    }
    
  } else {
    return(NULL)
  }
}


bio.matrix = matrix(0, ncol = 2521, nrow = length(dm.docnums))
for(i in 1:2521) {
  temp = find.count(ind = i, name.vec = names)
  if (!is.null(temp)) {
  goods = as.numeric(temp[,3])/as.numeric(temp[,4]) > .2 & as.numeric(temp[,3]) > 5
  j = match(temp[which(goods),1], dm.docnums)
    bio.matrix[j,i] = 1
  }
  print(i)
}

save(bio.matrix, file = "bio.selfid.Rdata")

load("bio.selfid.Rdata")
## checking 50 nodes
b = sample(1:2521, size = 50)

check.node = function(node) {
  print("Name is:----")
  print(dm.names[node])
  print("------------")
  a = find.count(ind = node, name.vec = names)
  if(!is.null(a)) {
    b = match(as.numeric(a[,1]), dm.docnums)
    print(cbind(a, bio.matrix[b,node]))
  } else {
    print(a)
  }
  return(invisible(1))
}

for(i in 1:50) {
  check.node(b[i])
}

names[find.match(target = "John. Abbot", name.vec = names)]
find.count(ind =4, name.vec = names)

##################
## trying to fit with bio indicators
## 10/16

load("bio.selfid.Rdata")
load("update3.datamat.Rdata")

load("update3sp500.datamat.Rdata")
library(glmnet)

rowSums(dm.datamat.sp > 0) -> a
dm.cv = dm.datamat.sp[a>2,]
N = dim(dm.cv)[1]
time.matrix = matrix(0, ncol = 4, nrow = 20)

lambda.list = list()
lambda.list[[1]] = unique(c(seq(from=100, to=10, by = -5),
                            seq(from=10, to=6, by = -.5),
                            seq(from=6, to=4, by = -.25),
                            seq(from=4, to=2, by = -.1),
                            seq(from=2, to=.9, by = -.05),
                            seq(from=.9, to=.3, by = -.04)))
lambda.list[[2]] = unique(c(seq(from=.3, to=.15, by = -.005),
                            seq(from=.15, to=.1, by = -.0025),
                            seq(from=.1, to=.05, by = -.002)))
lambda.list[[3]] = unique(c(seq(from=.05, to = .02, by = -.001),
                            seq(from=.02, to = .01, by = -.0002),
                            seq(from=.01, to = .005, by = -.0002),
                            seq(from=.005, to = .003, by = -.0001),
                            seq(from=.003, to = .002, by = -.00005),
                            seq(from=.002, to = .001, by = -.00002)))
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))

results = list()
for(j in 1:4) {
  results[[j]] = list()
}



adj.docnums = as.numeric(gsub("[.].*", "", dm.docnums.sp))[which(a>2)]
biom.matrix = matrix(0, nrow = dim(dm.cv)[1], ncol = 2521)
for(j in 1:dim(biom.matrix)[1]) {
  match = which(dm.docnums == adj.docnums[j])
  biom.matrix[j,] = bio.matrix[match,]
  if(j %% 500 == 0) {print(j) }
}
rscol = as.numeric(rowSums(biom.matrix) > 0)
cs = colSums(bio.matrix) > 0
b = sample(which(colSums(biom.matrix)> 0), size = 20)
time.sample = sort(b)
save(time.sample, time.matrix, results, file = "timetest.Rdata")

save(biom.matrix, file = "sp500biom.Rdata")

for(node in 1:20) {
  print(paste("Node: ", node))
  print(date())
  x = time.sample[node]
  time.matrix[node,1] = system.time(( 
    results[[1]][[node]] = 
      try(glmnet(dm.cv[,-x], dm.cv[,x],
                 family="poisson", lambda = lambda.touse, 
                 maxit = 3000 , thres = 10^(-6)), silent = 0) ))[1]
  print(date())
  time.matrix[node,2] = system.time(( 
    results[[2]][[node]] = 
      try(glmnet(cbind(dm.cv[,-x], biom.matrix[,x]),
                 dm.cv[,x], penalty.factor = c(rep(1, 2520), 0.02), 
                 family="poisson", lambda = lambda.touse, 
                 maxit = 3000 , thres = 10^(-6)), silent = TRUE) ))[1]
#   print(date())
#   time.matrix[node,3] = system.time(( 
#     results[[3]][[node]] = 
#       try(glmnet(cbind(dm.cv[,-x], 
#                        biom.matrix[,x],
#                        (rscol - biom.matrix[,x])),
#                  dm.cv[,x], penalty.factor = c(rep(1, 2520), c(0,0)), 
#                  family="poisson", lambda = lambda.touse, 
#                  maxit = 3000 , thres = 10^(-6)), silent = TRUE) ))[1]
#   
#   print(date())
#   time.matrix[node,4] = system.time(( 
#     results[[4]][[node]] = 
#       try(glmnet(cbind(dm.cv[,-x], biom.matrix[,cs]),
#                  dm.cv[,x], penalty.factor = c(rep(1, 2520), 
#                                                rep(0, times = length(cs))), 
#                  family="poisson", lambda = lambda.touse, 
#                  maxit = 3000 , thres = 10^(-6)), silent = TRUE) ))[1]
#   
  print("Saving....")
  save(time.sample, time.matrix, results, file = "timetest.Rdata")
}


## trying manual version of glmnet

load("update3sp500.datamat.Rdata")
source("srcfish.R")


rowSums(dm.datamat.sp > 0) -> a
dm.cv = dm.datamat.sp[a>2,]
N = dim(dm.cv)[1]
time.matrix = matrix(0, ncol = 4, nrow = 20)

lambda.list = list()
lambda.list[[1]] = unique(c(seq(from=100, to=10, by = -5),
                            seq(from=10, to=6, by = -.5),
                            seq(from=6, to=4, by = -.25),
                            seq(from=4, to=2, by = -.1),
                            seq(from=2, to=.9, by = -.05),
                            seq(from=.9, to=.3, by = -.04)))
lambda.list[[2]] = unique(c(seq(from=.3, to=.15, by = -.005),
                            seq(from=.15, to=.1, by = -.0025),
                            seq(from=.1, to=.05, by = -.002)))
lambda.list[[3]] = unique(c(seq(from=.05, to = .02, by = -.001),
                            seq(from=.02, to = .01, by = -.0002),
                            seq(from=.01, to = .005, by = -.0002),
                            seq(from=.005, to = .003, by = -.0001),
                            seq(from=.003, to = .002, by = -.00005),
                            seq(from=.002, to = .001, by = -.00002)))
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))

adj.docnums = as.numeric(gsub("[.].*", "", dm.docnums.sp))[which(a>2)]

load("sp500biom.Rdata")
rscol = as.numeric(rowSums(biom.matrix) > 0)
cs = colSums(biom.matrix) > 0
b = sample(which(cs), size = 20)
time.sample = sort(b)

x = 65
pr = cbind(dm.cv[,-x], biom.matrix[,x])


temp = unique(sort(c(which(dm.cv[,x] > 0), sample(1:35000, size = 15) )))
pr = dm.cv[temp,-x]
bob = colSums(pr)
pr = pr[,which(bob > 0),]

a = glmnet(pr,
           dm.cv[temp,x], penalty.factor = 1, 
           family="poisson", lambda = lambda.touse, 
           maxit = 1500 , thres = 10^(-6))





#####################################################33
## Simple CV for indicator vars 
################################################## 10/15
## SETUP
load("update3sp500.datamat.Rdata")
library(glmnet)

rowSums(dm.datamat.sp > 0) -> a
dm.cv = dm.datamat.sp[a>2,]
remain = 1:dim(dm.cv)[1]
cv.samples = list()
N = length(remain)

# some will never be in a training set. 
for(j in 1:10) {
  cv.samples[[j]] = sample(remain, size = floor(N/10))
  remain = setdiff(remain, cv.samples[[j]])
}

load("sp500biom.Rdata")
rscol = as.numeric(rowSums(biom.matrix) > 0)
cs = colSums(biom.matrix)

# find which columns have indicators split properly
temp = matrix(0, ncol = 2521, nrow = 10)
for(j in 1:10) {
  temp[j,] = colSums(biom.matrix[cv.samples[[j]],])
}
maxs = apply(temp, 2, max)

ok.to.sample = which(cs - maxs > 2)

sets.cv = list()
nodes.rm = ok.to.sample
for(s in 1:3) {
  sets.cv[[s]] = sort(sample(nodes.rm, size = 25))
  nodes.rm = setdiff(nodes.rm, sets.cv[[s]])
}

lambda.list = list()
lambda.list[[1]] = unique(c(seq(from=100, to=10, by = -5),
                            seq(from=10, to=6, by = -.5),
                            seq(from=6, to=4, by = -.25),
                            seq(from=4, to=2, by = -.1),
                            seq(from=2, to=.9, by = -.05),
                            seq(from=.9, to=.3, by = -.04)))
lambda.list[[2]] = unique(c(seq(from=.3, to=.15, by = -.005),
                            seq(from=.15, to=.1, by = -.0025),
                            seq(from=.1, to=.05, by = -.002)))
lambda.list[[3]] = unique(c(seq(from=.05, to = .02, by = -.001),
                            seq(from=.02, to = .01, by = -.0002),
                            seq(from=.01, to = .005, by = -.0002),
                            seq(from=.005, to = .003, by = -.0001),
                            seq(from=.003, to = .002, by = -.00005),
                            seq(from=.002, to = .001, by = -.00002)))
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))

glm.x.all <- function(fold, x, tryn) {
  if (tryn > 1) {
    start = results[[fold]][[x]]$lambda[length(results[[fold]][[x]]$lambda) - 1]
    matches = match(results[[fold]][[x]]$lambda,lambda.touse)
    end = lambda.touse[max(matches, na.rm = TRUE) + 1]
    
    to.add = seq(from=start, to=end, length.out = 11)
    new.lambda = unique(sort(c(to.add, results[[fold]][[x]]$lambda, lambda.touse), decreasing = TRUE))
    maxit2 = 2000 * tryn
    #return(list(new.lambda, maxit2))
    return(try(glmnet(dm.curind, dm.curind[,x],
                      family="poisson", lambda = new.lambda, penalty.factor = c(rep(1, 2520), 0.02),
                      maxit = maxit2 , thres = 10^(-6)), silent = TRUE))
  } else {
    return(try(glmnet(dm.curind, dm.curind[,x],
                      family="poisson", lambda = lambda.touse, penalty.factor = c(rep(1, 2520), 0.02),
                      maxit = 2000 , thres = 10^(-6)), silent = TRUE))
  }
}

find.done = function(x) {
  done = rep(-1, times = length(x))
  for(i in 1:length(x)) {
    if (length(x[[i]]) == 12) {
      done[i] = (x[[i]]$lambda[length(x[[i]]$df)] == 0.001)
    }
  }
  return(done)
}

save(cv.samples, remain, nodes.rm, sets.cv,
     lambda.touse, glm.x.all, find.done, file = "500indcvSETUP.Rdata")

## add new nodes to cv
load("500indcvSETUP.Rdata")

for(s in 4:10) {
  sets.cv[[s]] = sort(sample(nodes.rm, size = 25))
  nodes.rm = setdiff(nodes.rm, sets.cv[[s]])
}

save(cv.samples, remain, nodes.rm, sets.cv,
     lambda.touse, glm.x.all, find.done, file = "500indcvSETUP.Rdata")

### Code for CV
set.num = 1   # Do this from 1 to 3
filename = paste("cvres", set.num, ".Rdata", sep = "")

load("update3sp500.datamat.Rdata")
library(glmnet)
load("sp500biom.Rdata")
load("500indcvSETUP.Rdata")

rm(dm.docnums.sp, dm.names.sp, new.result.matrix)

rowSums(dm.datamat.sp > 0) -> a
dm.cv = dm.datamat.sp[a>2,]
rm(dm.datamat.sp, a)

N = dim(dm.cv)[1]

results = list()
for(j in 1:10) {
  results[[j]] = list()
}

for(trialnum in 1:10) {
  print(paste("TRIALNUM---------------", trialnum))
  cv.I = setdiff(1:N,c(cv.samples[[trialnum]], remain))
  
  dm.curfold = dm.cv[cv.I, ]
  
  to.do = sets.cv[[set.num]]
  for(i in 1:25) {
    dm.curind = cbind(dm.curfold[,-to.do[i]], biom.matrix[cv.I,to.do[i]])
    
    print(paste("Node",to.do[i]," ::: " ,i, "of", 25))
    print(date())
    tryn = 1
    not.done = TRUE
    while(not.done) {
      print(paste("Trial", tryn))
      print(date())
      results[[trialnum]][[to.do[i]]] = glm.x.all(fold = trialnum, x = to.do[i], tryn = tryn)
      not.done = (find.done(results[[trialnum]][to.do[i]]) != 1)
      tryn = tryn+1
      if (class(results[[trialnum]][[to.do[i]]]) == "try-error") {
        tryerr = 1
        not.done = FALSE
        print("TRYERROR")
      }
    }
  }
  save(results, file = filename)
}


## Computing test-set deviances

compute.deviance.vec = function(true, predict) {
  a = 2 * (predict - true) 
  b = true > 0
  a[b] = a[b] + 2*true[b]*(log(true[b]) - log(predict[b]))
  return(a)
}

load("update3sp500.datamat.Rdata")
library(glmnet)
load("sp500biom.Rdata")
load("500indcvSETUP.Rdata")

rm(dm.docnums.sp, dm.names.sp, new.result.matrix)

rowSums(dm.datamat.sp > 0) -> a
dm.cv = dm.datamat.sp[a>2,]
lambdas.tokeep = c(1, 0.9, 0.7, 0.5, 0.42, 0.38, 0.34, 0.3, 0.29, 0.28, 
                   0.27, 0.26, 0.25, 0.24, 0.23, 0.22, 0.21, 0.2, 0.19,
                   0.18, 0.17,
                   0.16, 0.15, 0.14, 0.13, 0.12, 0.11, 0.1, 0.09, 0.08, 0.07,
                   0.06, 0.05, 0.04, 0.03, 0.02, 0.015, 0.01, 0.009, 0.008, 0.007,
                   0.006, 0.005, 0.0045, 0.004, 0.0035, 0.003, 0.00275, 0.0025, 
                   0.00225, 0.002, 0.0018, 0.0016, 0.0014, 0.0012, 0.001)
rm(a, dm.datamat.sp)

devs2 = array(0, dim = c(125, 10, length(lambdas.tokeep))) #devs[node,fold,lambda]
# save(devs, file = "cvres500I.Rdata")
load(file = "cvres500I.Rdata")
devs2[1:75,,] = devs
devs = devs2

for (large in 3:4) { # large splits
  load(paste("cvres", (large+1), ".Rdata", sep = ""))
  
  for(k in 1:25) { # nodes counter
    means = matrix(0, nrow = 10, ncol = length(lambdas.tokeep))
    for(fold in 1:10) { #folds
      nodenum = sets.cv[[large+1]][k]
      
      lambda.loc = match(lambdas.tokeep, results[[fold]][[nodenum]]$lambda)
      as = results[[fold]][[nodenum]]$a0[lambda.loc]
      bs = results[[fold]][[nodenum]]$beta[, lambda.loc]
      
      to.pred.ind = sort(c(cv.samples[[fold]], remain))
      ln = length(to.pred.ind)
      preds = exp(cbind(dm.cv[to.pred.ind,-nodenum],
                    biom.matrix[to.pred.ind,nodenum]) %*% bs + 
        matrix(as, nrow = ln, ncol = length(lambdas.tokeep), byrow = TRUE))
      true = dm.cv[to.pred.ind,nodenum]
      for(m in 1:length(lambdas.tokeep)) {
        means[fold,m] = mean(compute.deviance.vec(true = true, predict = preds[,m]))
      }
    }
    n = k + large*25
    devs[n,,] = means
    print(n)
  }
  save(devs, file = "cvres500I.Rdata")
}


### counting edges
edge.count = matrix(0, nrow = 75, ncol = 10)
lambda.val = 0.001
for (large in 1:3) { # large splits
  load(paste("cvres", large, ".Rdata", sep = ""))
  
  for(k in 1:25) { # nodes counter
    nodenum = sets.cv[[large]][k]
    for(j in 1:10) {
      lambda.loc = which(lambda.val == results[[j]][[nodenum]]$lambda)
      edge.count[(large-1)*25 + k, j] = results[[j]][[nodenum]]$df[lambda.loc]
    }
  }
}

# analysis
med.devs = apply(devs, c(1,3), median)
cvscores1 = apply(med.devs, 2, mean)
lambdas.tokeep = c(1, 0.9, 0.7, 0.5, 0.42, 0.38, 0.34, 0.3, 0.29, 0.28, 
                   0.27, 0.26, 0.25, 0.24, 0.23, 0.22, 0.21, 0.2, 0.19,
                   0.18, 0.17,
                   0.16, 0.15, 0.14, 0.13, 0.12, 0.11, 0.1, 0.09, 0.08, 0.07,
                   0.06, 0.05, 0.04, 0.03, 0.02, 0.015, 0.01, 0.009, 0.008, 0.007,
                   0.006, 0.005, 0.0045, 0.004, 0.0035, 0.003, 0.00275, 0.0025, 
                   0.00225, 0.002, 0.0018, 0.0016, 0.0014, 0.0012, 0.001)

plot(lambdas.tokeep, cvscores1, main = "CV Scores vs. Lambda", 
     ylab = "Deviance Score", xlab = "Lambda", ylim = c(0.075, 0.1),
     type = "b", pch = 19, cex = .5)
min.lamb = lambdas.tokeep[which(cvscores1 == min(cvscores1))]
abline(v = min.lamb, col = "darkgreen")

j = 52
plot(lambdas.tokeep, med.devs[j,], main = "CV Scores vs. Lambda", 
     ylab = "Deviance Score", xlab = "Lambda", 
     type = "b", pch = 19, cex = .5)
min.lamb = lambdas.tokeep[which(med.devs[j,] == min(med.devs[j,]))]
abline(v = min.lamb, col = "darkgreen")





########### Fitting full model for 500, without indicator (10/31, 2AM)
## run on hydra, setup new file as sparse matrix
load("comb500.datamat.Rdata")

rowSums(cbn.datamat > 0) -> a
dm = Matrix(cbn.datamat[a>2,], sparse = TRUE) #greater than 2
save(dm, cbn.docnums, cbn.names, file = "cb500g2.dm.Rdata")

## run on banshee
library(snow)
library(glmnet)

clust1 = makeSOCKcluster(c("node68","node69","node70", "node71", "node72",
                           "node73", "node74", "node75", "node76", "node77",
                           "node78","node79"))

# pw fOgvo61C6Yw9H6h
# stopCluster(clust1)

clusterEvalQ(clust1, library(glmnet))
clusterEvalQ(clust1, load("cb500g2i1.dm.Rdata"))
load("cb500g2i1.dm.Rdata")

lambda.list = list()
lambda.list[[1]] = unique(c(seq(from=100, to=10, by = -5),
                            seq(from=10, to=6, by = -.5),
                            seq(from=6, to=4, by = -.25),
                            seq(from=4, to=2, by = -.1),
                            seq(from=2, to=.9, by = -.05),
                            seq(from=.9, to=.3, by = -.04)))
lambda.list[[2]] = unique(c(seq(from=.3, to=.15, by = -.005),
                            seq(from=.15, to=.1, by = -.0025),
                            seq(from=.1, to=.05, by = -.002)))
lambda.list[[3]] = unique(c(seq(from=.05, to = .02, by = -.001),
                            seq(from=.02, to = .01, by = -.0002),
                            seq(from=.01, to = .005, by = -.0002),
                            seq(from=.005, to = .003, by = -.0001),
                            seq(from=.003, to = .002, by = -.00005),
                            seq(from=.002, to = .001, by = -.00002)))
lambda.touse = unique(round(sort(c(lambda.list, recursive = TRUE), decreasing = TRUE), 5))

glm.x = function(x) {
  return(try(glmnet(dm[,-x], dm[,x],
                    family="poisson", lambda = lambda.touse, 
                    maxit = 2000 , thres = 10^(-6))))
}


find.done = function(x) {
  done = rep(-1, times = length(x))
  for(i in 1:length(x)) {
    if (length(x[[i]]) == 12) {
      done[i] = (x[[i]]$lambda[length(x[[i]]$df)] == 0.001)
    }
  }
  return(done)
}

glm.x.re <- function(x, prog, lamb.adj = 11) {
  start = prog[[x]]$lambda[length(prog[[x]]$lambda) - 1]
  matches = match(prog[[x]]$lambda,lambda.touse)
  end = lambda.touse[max(matches, na.rm = TRUE) + 1]
  
  to.add = seq(from=start, to=end, length.out = lamb.adj)
  new.lambda = unique(sort(c(to.add, prog[[x]]$lambda, lambda.touse), decreasing = TRUE))
  maxit2 = 2000 * (prog[[x]]$iters+1)
  #return(list(new.lambda, maxit2))
  return(try(glmnet(dm[,-x], dm[,x],
                    family="poisson", lambda = new.lambda, 
                    maxit = maxit2 , thres = 10^(-6))))
}

save(lambda.touse, glm.x, find.done, glm.x.re, file = "helper.Rdata")
clusterEvalQ(clust1, load("helper.Rdata"))


snow.time(parLapply(clust1, x = 1:30, fun = glm.x))


results = list()
N.vertex = dim(dm)[2]

# First attempt at fit
## 6767 to do, do 24 at a time. 
for(k in 1:300) {
  j = intersect((length(results) + 1 ): (length(results) + 24), 1:N.vertex)
  if (length(j) > 0) {
    print(c(min(j), max(j)))
    print(date())
    print(snow.time(( results[j] = parLapply(clust1, x = j, fun = glm.x) )) )
    if (k %% 10 == 0) {
      print("Saving....")
      print(date())
      save(results, file = "cb500g2.fit.Rdata")
    }
  }
}
save(results, file = "cb500g2.fit.Rdata")

######### banshee ran up to here:

# re-running non-finished trials
nodes=12

for(run in 2:10) {
  is.done = find.done(results)
  to.do = which(is.done == 0)
  prog = list()
  
  if (length(to.do) > 0) {
    
    
    for(j in 1:N.vertex) {
      if (is.done[j] == 0) {
        prog[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
      }
    }
    
    to.run = NULL
    for(i in 1:500) {
      prog[to.run] = paste(run ,"<- iteration done")
      to.run = to.do[intersect((nodes*(i-1)+1):(nodes*(i)), 1:length(to.do))]
      to.run = to.run[!is.na(to.run)]
      if (length(to.run) > 0) {
        print(paste("Re-iter:", run))
        print(to.run)
        print(date())
        print(snow.time((results[to.run] = parLapply(clust1, x = to.run, 
                                                     fun = glm.x.re, 
                                                     prog = prog))) )
      }
    }
    save(results, file = "cb500g2.fit.test.Rdata")
    
    
  }
}

for(run in 11:20) {
  is.done = find.done(results)
  to.do = which(is.done == 0)
  prog = list()
  
  if (length(to.do) > 0) {
    
    
    for(j in 1:N.vertex) {
      if (is.done[j] == 0) {
        prog[[j]] = list(lambda = results[[j]]$lambda, iters=(run-1))
      }
    }
    
    to.run = NULL
    for(i in 1:500) {
      prog[to.run] = paste(run ,"<- iteration done")
      to.run = to.do[intersect((nodes*(i-1)+1):(nodes*(i)), 1:length(to.do))]
      to.run = to.run[!is.na(to.run)]
      if (length(to.run) > 0) {
        print(paste("Re-iter:", run))
        print(to.run)
        print(date())
        print(snow.time((results[to.run] = parLapply(clust1, x = to.run, 
                                                     fun = glm.x.re, 
                                                     prog = prog,
                                                     lamb.adj = 21))) )
      }
    }
    
  }
}
save(results, file = "cb500g2.fit.test.Rdata")



### computing bio matrix for combined docs
# 10/31 2:30 AM

load("names.raw.Rdata")
load("doc.totals.Rdata")

load("cbn.nosp.datamat.Rdata")


find.match <- function(target, name.vec) {
  split = strsplit(target, split = " ")[[1]]
  res = 1:58265
  for(j in 1:length(split)) {
    res = intersect(res, grep(gsub("[.]", "", split[j]), name.vec))
  }
  return(res)
}

find.count <- function(ind, name.vec) {
  #relies on existence of dm.docnums, dm.datamat, ODNB.nums
  # searches name 'ind', so needs dm.names
  # depends on rowsums
  if (length(grep("[|]", cbn.names.nosp[ind])) > 0) {
    mult = strsplit(cbn.names.nosp[ind], split = "[|]")[[1]]
    res = NULL
    for(k in 1:length(mult)) {
      temp = find.match(target = mult[k], name.vec= name.vec)
      res = c(res, temp)
    }
    res = sort(unique(res))
    temp = res
  } else {
    temp = find.match(target = cbn.names.nosp[ind], name.vec= name.vec)
  }
  ## next line is usually off
  # temp = unique(c(temp, NA, ODNB.nums.inv[dm.docnums[intersect(which(dm.datamat[,ind] > 5), which(dm.datamat[,ind]/rowsums > 0.25))]]))
  to.search = ODNB.nums[temp]
  if (length(temp) > 0) {
    aa = match(to.search, cbn.docnums.nosp)
    if (sum(!is.na(aa)) > 0) {
      return(cbind(to.search, name.vec[temp], cbn.datamat.nosp[aa,ind],
                   apply(cbn.datamat.nosp[aa,,drop = FALSE], 1, sum),
                   doc.totals[match(to.search, names(doc.totals))])) 
    } else {
      return(cbind(to.search, name.vec[temp], cbn.datamat.nosp[aa,ind],
                   NA,
                   doc.totals[match(to.search, names(doc.totals))]))
    }
    
  } else {
    return(NULL)
  }
}


bio.matrix = matrix(0, ncol = length(cbn.names.nosp), nrow = length(cbn.docnums.nosp))

for(i in 1:length(cbn.names.nosp)) {
  temp = find.count(ind = i, name.vec = names)
  if (!is.null(temp)) {
    goods = as.numeric(temp[,3])/as.numeric(temp[,4]) > .2 & as.numeric(temp[,3]) > 5
    j = match(temp[which(goods),1], cbn.docnums.nosp)
    bio.matrix[j,i] = 1
  }
  print(i)
}

save(bio.matrix, file = "comb.nosp.bio.Rdata")

load("comb500.datamat.Rdata")


adj.docnums = as.numeric(gsub("[.].*", "", cbn.docnums))

biom.matrix = matrix(0, nrow = length(cbn.docnums), ncol = 6767)

for(j in 1:dim(biom.matrix)[1]) {
  match = which(cbn.docnums.nosp == adj.docnums[j])
  biom.matrix[j,] = bio.matrix[match,]
  if(j %% 100 == 0) {print(j) }
}
library(Matrix)
biom.matrix = Matrix(biom.matrix, sparse = TRUE)

save(biom.matrix, file = "comb.sp500.bio.Rdata")




