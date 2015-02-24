





# analysis
tosave = read.csv("test2.csv")
load("cb500i1.dm.Rdata")

jh.dat = read.csv("datajh.csv")
jh.dat = jh.dat[,2:3]
jh = which(tosave[,2] == "James Harrington")
main.dat = tosave[jh,-1]
main.ranks = matrix(NA, nrow = 71, ncol = 24)
p.dat = jh.dat

jm.dat = read.csv("datajm.csv")
jm.dat = jm.dat[1:111,3:4]
jm = which(tosave[,2] == "John Milton")
main.dat = tosave[jm,-1]
main.ranks = matrix(NA, nrow = 111, ncol = 24)
p.dat = jm.dat

for(r in 1:dim(main.ranks)[1]) {
  for(c in 4:27) {
    if (!is.na(main.dat[r,c])) {
      if (main.dat[r,c] == 0) {
        main.dat[r,c] = NA
      }
    }
  }
}
rank(main.dat[,4], na.last = "keep", ties.method = "average")
for(j in 4:27) {
  main.ranks[,j-3] = rank(main.dat[,j], na.last = "keep", ties.method = "average")
  # making only top 30
  #   a = sort(main.ranks[,j-3], decreasing = TRUE)
  #   smallest.tokeep = a[min(30, length(a))]
  #   main.ranks[(main.ranks[,j-3] < smallest.tokeep),j-3] = NA
}
p.dat[match( main.dat[,3],p.dat[,1]),2]
true.ranks = rank( (101 - p.dat[match( main.dat[,3],p.dat[,1]),2]), na.last = "keep", ties.method = "average")

# 1st row = spearman corr for each method
cor.res.matrix = matrix(NA, nrow = 50, ncol = 24)
for(j in 1:24) {
  cor.res.matrix[1,j] = cor(true.ranks,main.ranks[,j], use = "pairwise.complete.obs")
}

# 2nd row = spearman cor for each method, ignoring negative final wtd edges
temp.dat = main.dat[,4:27]
temp.ranks = temp.dat
for(j in 1:4) {
  b = which(temp.dat[,5 + (j-1) * 6] < 0)
  temp.dat[b,(1:6) + 6*(j-1)] = NA
}
for(j in 1:24) {
  temp.ranks[,j] = rank(temp.dat[,j], na.last = "keep", ties.method = "average")
}
for(j in 1:24) {
  cor.res.matrix[2,j] = cor(true.ranks,temp.ranks[,j], use = "pairwise.complete.obs")
}

#3rd row = leave blank
#4th row = NA for 100s
#5th row = # of 100s in top 20
#6th row = # of 100s in top 30
a = p.dat[match( main.dat[,3],p.dat[,1]),]
for (k in which(a[,2] == 100)) {
  a[k,2] = NA
}
true.ranks.n100 = rank( (101 - a[,2]), na.last = "keep", ties.method = "average")
a = p.dat[match( main.dat[,3],p.dat[,1]),]
ohs = which(a[,2] == 100)

topk = function(dat, k) {
  k = min(k, sum(!is.na(dat)))
  return(order(dat,decreasing= TRUE)[1:k])
}
for(j in 1:24) {
  cor.res.matrix[4,j] = cor(true.ranks.n100,main.ranks[,j], use = "pairwise.complete.obs")
  cor.res.matrix[5,j] = length(intersect(ohs, topk(main.dat[,3+j], k = 20)))
  cor.res.matrix[6,j] = length(intersect(ohs, topk(main.dat[,3+j], k = 30)))
}


# 8-10: same except 80,100 instead of just 100. 
#3rd row = leave blank
#4th row = NA for 100s
#5th row = # of 100s in top 20
#6th row = # of 100s in top 30
a = p.dat[match( main.dat[,3],p.dat[,1]),]
for (k in which(a[,2] > 70)) {
  a[k,2] = NA
}
true.ranks.n100 = rank( (101 - a[,2]), na.last = "keep", ties.method = "average")
a = p.dat[match( main.dat[,3],p.dat[,1]),]
ohs = which(a[,2] > 70)

topk = function(dat, k) {
  k = min(k, sum(!is.na(dat)))
  return(order(dat,decreasing= TRUE)[1:k])
}
for(j in 1:24) {
  cor.res.matrix[8,j] = cor(true.ranks.n100,main.ranks[,j], use = "pairwise.complete.obs")
  cor.res.matrix[9,j] = length(intersect(ohs, topk(main.dat[,3+j], k = 20)))
  cor.res.matrix[10,j] = length(intersect(ohs, topk(main.dat[,3+j], k = 30)))
}



save(cor.res.matrix, file = "spear.cor.res.Rdata")


tosave = read.csv("test2.csv")
load("cb500i1.dm.Rdata")

to.analyze = 1:2
par(mfrow = c(1,2))

jh.dat = read.csv("datajh.csv")
jh.dat = jh.dat[,2:3]
jh = which(tosave[,2] == "James Harrington")
main.dat = tosave[jh,-1]
main.ranks = matrix(NA, nrow = 71, ncol = 24)
p.dat = jh.dat
p.dat = p.dat[match( main.dat[,3],p.dat[,1]),]


for(r in 1:dim(main.ranks)[1]) {
  for(c in 4:27) {
    if (!is.na(main.dat[r,c])) {
      if (main.dat[r,c] == 0) {
        main.dat[r,c] = NA
      }
    }
  }
}
rank(main.dat[,4], na.last = "keep", ties.method = "average")
for(j in 4:27) {
  main.ranks[,j-3] = rank(main.dat[,j], na.last = "keep", ties.method = "average")
  # making only top 30
  #   a = sort(main.ranks[,j-3], decreasing = TRUE)
  #   smallest.tokeep = a[min(30, length(a))]
  #   main.ranks[(main.ranks[,j-3] < smallest.tokeep),j-3] = NA
}
p.dat[match( main.dat[,3],p.dat[,1]),2]
true.ranks = rank( (101 - p.dat[match( main.dat[,3],p.dat[,1]),2]), na.last = "keep", ties.method = "average")

topk(main.dat[,3], k = 5)

# counting type of match vs. when they occur (top 10, top 20, etc)
ks = 1:50
cuts.s = c(0, 20, 50, 80, 100)
cuts.e = c(19, 49, 79, 99, 500)
count.res.matrix = array(NA, dim = c(length(ks), length(cuts.s)+1, 3))

# plot for nosp
to.try = 6
for(k in 1:length(ks)) {
  finds = topk(main.dat[,to.try], k = ks[k])
  assigns = p.dat[finds,2]
  for(c in 1:length(cuts.s)) {
    count.res.matrix[k,c,1] = sum(assigns >= cuts.s[c] & assigns <= cuts.e[c], na.rm = 1)
  }
  count.res.matrix[k,length(cuts.s)+1,1] = sum(is.na(assigns))
}


# plot for sp500
to.try = 12
negs = which(main.dat[,14] < 0)
temp = main.dat[,to.try]
temp[negs] = NA
for(k in 1:length(ks)) {
  finds = topk(temp, k = ks[k])
  assigns = p.dat[finds,2]
  for(c in 1:length(cuts.s)) {
    count.res.matrix[k,c,2] = sum(assigns >= cuts.s[c] & assigns <= cuts.e[c], na.rm = 1)
  }
  count.res.matrix[k,length(cuts.s)+1,2] = sum(is.na(assigns))
}


#plot for corr
to.try = 15
for(k in 1:length(ks)) {
  finds = topk(main.dat[,to.try], k = ks[k])
  assigns = p.dat[finds,2]
  for(c in 1:length(cuts.s)) {
    count.res.matrix[k,c,3] = sum(assigns >= cuts.s[c] & assigns <= cuts.e[c], na.rm = 1)
  }
  count.res.matrix[k,length(cuts.s)+1,3] = sum(is.na(assigns))
}

plot(x = -1,y = -100, xlim = c(0,50), ylim = c(0,40), xlab = "Top x", 
     ylab = "Number of Top x correct", main = "James Harrington")
#|----##../ --Tue Feb 24 17:29:13 2015--
for(k in 1:3) {
  
  xs = rowSums(count.res.matrix[,,k])
  ys = rowSums(count.res.matrix[,to.analyze,k, drop = FALSE])
  points(xs, ys, type = "l", col = k)
  
}
legend(x = "topleft", col = 1:3, lty = 1, 
       legend = c("PGL full docs", "PGL split-docs", "Corr split-docs"),
       cex = 0.7)





## John Milton




jm.dat = read.csv("datajm.csv")
jm.dat = jm.dat[1:111,3:4]
jm = which(tosave[,2] == "John Milton")
main.dat = tosave[jm,-1]
main.ranks = matrix(NA, nrow = 111, ncol = 24)
p.dat = jm.dat

for(r in 1:dim(main.ranks)[1]) {
  for(c in 4:27) {
    if (!is.na(main.dat[r,c])) {
      if (main.dat[r,c] == 0) {
        main.dat[r,c] = NA
      }
    }
  }
}
rank(main.dat[,4], na.last = "keep", ties.method = "average")
for(j in 4:27) {
  main.ranks[,j-3] = rank(main.dat[,j], na.last = "keep", ties.method = "average")
  # making only top 30
  #   a = sort(main.ranks[,j-3], decreasing = TRUE)
  #   smallest.tokeep = a[min(30, length(a))]
  #   main.ranks[(main.ranks[,j-3] < smallest.tokeep),j-3] = NA
}
p.dat[match( main.dat[,3],p.dat[,1]),2]
true.ranks = rank( (101 - p.dat[match( main.dat[,3],p.dat[,1]),2]), na.last = "keep", ties.method = "average")

topk(main.dat[,3], k = 5)

# counting type of match vs. when they occur (top 10, top 20, etc)
ks = 1:50
cuts.s = c(0, 20, 50, 80, 100)
cuts.e = c(19, 49, 79, 99, 500)
count.res.matrix = array(NA, dim = c(length(ks), length(cuts.s)+1, 3))

# plot for nosp
to.try = 6
for(k in 1:length(ks)) {
  finds = topk(main.dat[,to.try], k = ks[k])
  assigns = p.dat[finds,2]
  for(c in 1:length(cuts.s)) {
    count.res.matrix[k,c,1] = sum(assigns >= cuts.s[c] & assigns <= cuts.e[c], na.rm = 1)
  }
  count.res.matrix[k,length(cuts.s)+1,1] = sum(is.na(assigns))
}


# plot for sp500
to.try = 12
negs = which(main.dat[,14] < 0)
temp = main.dat[,to.try]
temp[negs] = NA
for(k in 1:length(ks)) {
  finds = topk(temp, k = ks[k])
  assigns = p.dat[finds,2]
  for(c in 1:length(cuts.s)) {
    count.res.matrix[k,c,2] = sum(assigns >= cuts.s[c] & assigns <= cuts.e[c], na.rm = 1)
  }
  count.res.matrix[k,length(cuts.s)+1,2] = sum(is.na(assigns))
}


#plot for corr
to.try = 15
for(k in 1:length(ks)) {
  finds = topk(main.dat[,to.try], k = ks[k])
  assigns = p.dat[finds,2]
  for(c in 1:length(cuts.s)) {
    count.res.matrix[k,c,3] = sum(assigns >= cuts.s[c] & assigns <= cuts.e[c], na.rm = 1)
  }
  count.res.matrix[k,length(cuts.s)+1,3] = sum(is.na(assigns))
}

plot(x = -1,y = -100, xlim = c(0,50), ylim = c(0,40), xlab = "Top x", 
     ylab = "Number of Top x correct", main = "John Milton")
#|----##../ --Tue Feb 24 17:29:13 2015--
for(k in 1:3) {
  
  xs = rowSums(count.res.matrix[,,k])
  ys = rowSums(count.res.matrix[,to.analyze,k, drop = FALSE])
  points(xs, ys, type = "l", col = k)
  
}

