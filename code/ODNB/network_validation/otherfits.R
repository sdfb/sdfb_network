
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



## plotting ranks vs. model results












# computing matches for split 500. 

load("cb500.fit.Rdata")
load("cb500i1.dm.Rdata")

edge.add = matrix(NA, nrow = 6751, ncol = 6751)


smallest.pos.ind <- function(x) {
  return(which(x > 0)[1])
}

for(k in 1:6751) { 
print(k)
  b = apply(results[[k]]$beta, 1, smallest.pos.ind)
  nas = rep(NA, times = 6750)
  d = which(!is.na(b))
  if (length(d) > 0) {
    nas[d] = results[[k]]$lambda[b[d]] 
  }
  edge.add[-k,k] = nas
}
  

# finding top k edges
save(edge.add, file = "edge.wt.Rdata")



# nodes.touse = NULL
# res = NULL
# for(j in nodes.touse) {
#   a = cbind(j, topk.rank(dat = edge.add[j,], k = 30))
#   res = cbind(res, a)
# }

topk.rank(dat=edge.add[523,], k = 30)

# computing node weights: 
load("edge.wt.Rdata")
library(Matrix)

topk.rank = function(dat, k) {
  b = which(!is.na(dat))
  k = min(k, length(b))
  if (k == 0) {return(NULL)}
  res = cbind(b, dat[b])
  res = res[order(res[,2], decreasing = TRUE),, drop = FALSE]
  return(cbind(res[1:k,1], rank(res[1:k,2])))
}

load("cb500i1.dm.Rdata")
colSums(dm) -> node.wts

# generating graph file: 
# ns = nodes
# 886 = JH
b = 886
ns = 1:6751

to.write = ""
to.write[1] = "graph testg {"
#to.write[2] = "graph [overlap=prism]"
to.write[2] = "node [shape=point]"
for(j in 1:length(ns)) {
  name = cbn.names[ns[j]]
  name = strsplit(name, "[|]")[[1]][1]
  to.write[j+1] = paste(ns[j]," [height=", round((log(node.wts[j])+1)/40,3), 
                        ", width=",round((log(node.wts[j])+2)/40,3) ,
                        ",label=\"\", shape=circle",
#                        ", label=\"",name,"\"", 
                        "]",
                        sep = "")
  print(j)
}

for(j in 1:length(ns)) {
  temp = NULL
  b = topk.rank(dat = edge.add[ns[j],], k = 10)
  if (length(b) > 0) {
  for(k in 1:dim(b)[1]) {
    if (length(intersect(b[k,1], ns)) > 0) {
      temp = c(temp, 
               paste(ns[j], " -- ", b[k,1],
                     " [weight=", round(b[k,2]),
                    ", color=lightcyan1]",
                     sep = "")    )
    }
  }
  }
  to.write = c(to.write, temp)
  print(j)
}
to.write = c(to.write, "}")

writeLines(to.write, con = "graphallp.txt")

# Correct naming scheme
corr.names = cbn.names

for(j in 1:length(cbn.names)) {
  corr.names[j] = strsplit(cbn.names[j], "[|]")[[1]][1]
}
corr.names[214] = "Charles I"


## James Harrington with no edges out, 1 degree away

# 886 = JH
b = c(886, topk.rank(edge.add[886,], k = 30)[,1])
ns = b

to.write = ""
to.write[1] = "graph testg {"
to.write[2] = "graph [overlap=prism, splines=true]"
for(j in 1:length(ns)) {
  to.write[j+2] = paste(ns[j]," [height=", round((log(node.wts[j])+1)/4,3), 
                        ", width=",round((log(node.wts[j])+2)/4,3) ,
                        ", label=\"",corr.names[ns[j]],"\"", 
                        ", fixedsize=true",
                        ", fillcolor=slategray1",
                        ", style=filled",
                        "]",
                        sep = "")
  print(j)
}

match.mat = matrix(NA, nrow = 10000, ncol = 3)
count = 1
for(j in 1:length(ns)) {
  
  b = topk.rank(dat = edge.add[ns[j],], k = 30)
  if (length(b) > 0) {
    for(k in 1:dim(b)[1]) {
      if (length(intersect(b[k,1], ns)) > 0) {
        if(count > 1) {
          a1 = which(match.mat[1:(count-1),1] == b[k,1])
          a2 = which(match.mat[1:(count-1),2] == ns[j])
          a = intersect(a1, a2)
          if(length(a) > 0) {
            print(a)
            match.mat[a,3] = (match.mat[a,3] + b[k,2]) / 1.6
          } else {
            match.mat[count,] = c(ns[j], b[k,1], b[k,2])
            count = count + 1
          }
        } else {
          match.mat[count,] = c(ns[j], b[k,1], b[k,2])
          count = count + 1
        }
      }
    }
  }
}
temp = NULL

for(j in 1:(count-1)) {
  if (match.mat[j,1] == 886) {
    temp = c(temp, 
             paste(match.mat[j,1], " -- ", match.mat[j,2],
                   " [len=", round(2.3 - (match.mat[j,3]/16), 3),
                   ", weight=10000",
                   ", penwidth=", round(match.mat[j,3]/16) + 1,
                   ", color=invis]",
                   sep = "")    )  
  } else {
    temp = c(temp, 
             paste(match.mat[j,1], " -- ", match.mat[j,2],
                   #" [weight=", round(match.mat[j,3]*2),
                   "[penwidth=", round(match.mat[j,3]/9) + 1,
                   ", weight=1",
                   ", color=springgreen4]",
                   sep = "")    )
  }
}
to.write = c(to.write, temp)
print(j)

to.write = c(to.write, "}")

writeLines(to.write, con = "graphjh1deg.txt")








#JH snowball: jh top 30, 
# 886 = JH
b = c(886, topk.rank(edge.add[886,], k = 30)[,1])
for(i in 1:length(b)) {
  b = c(b, topk.rank(edge.add[b[i],], k = 15)[,1])
}
ns = unique(b)

to.write = ""
to.write[1] = "graph testg {"
to.write[2] = "graph [overlap=prism, splines=true]"
for(j in 1:length(ns)) {
  to.write[j+2] = paste(ns[j]," [height=", round((log(node.wts[j])+1)/4,3), 
                        ", width=",round((log(node.wts[j])+2)/4,3) ,
                        ", label=\"",corr.names[ns[j]],"\"", 
                        ", fixedsize=true",
                        ", fillcolor=slategray1",
                        ", style=filled",
                        "]",
                        sep = "")
  print(j)
}

match.mat = matrix(NA, nrow = 10000, ncol = 3)
count = 1
for(j in 1:length(ns)) {
  
  b = topk.rank(dat = edge.add[ns[j],], k = 30)
  if (length(b) > 0) {
    for(k in 1:dim(b)[1]) {
      if (length(intersect(b[k,1], ns)) > 0) {
        if(count > 1) {
          a1 = which(match.mat[1:(count-1),1] == b[k,1])
          a2 = which(match.mat[1:(count-1),2] == ns[j])
          a = intersect(a1, a2)
          if(length(a) > 0) {
            print(a)
            match.mat[a,3] = (match.mat[a,3] + b[k,2]) / 1.6
          } else {
            match.mat[count,] = c(ns[j], b[k,1], b[k,2])
            count = count + 1
          }
        } else {
          match.mat[count,] = c(ns[j], b[k,1], b[k,2])
          count = count + 1
        }
      }
    }
  }
}
temp = NULL

for(j in 1:(count-1)) {
  if (match.mat[j,1] == 886) {
    temp = c(temp, 
             paste(match.mat[j,1], " -- ", match.mat[j,2],
                   " [weight=", round(match.mat[j,3]*4),
                   ", penwidth=", round(match.mat[j,3]/20) + 1,
                   ", color=wheat]",
                   sep = "")    )  
  } else {
    temp = c(temp, 
             paste(match.mat[j,1], " -- ", match.mat[j,2],
                   " [weight=", round(match.mat[j,3]*2),
                   ", penwidth=", round(match.mat[j,3]/13) + 1,
                   ", color=springgreen4]",
                   sep = "")    )
  }
}
to.write = c(to.write, temp)
print(j)

to.write = c(to.write, "}")

writeLines(to.write, con = "graphjh2deg.txt")



## Latentnet

library(latentnet)


# 886 = JH
b = c(886, topk.rank(edge.add[886,], k = 30)[,1])
ns = b

#make adj.matrix
jh.1.adj = matrix(0, nrow = length(ns), ncol = length(ns))

count = 1
for(j in 1:length(ns)) {
  b = topk.rank(dat = edge.add[ns[j],], k = 30)
  if (length(b) > 0) {
    for(k in 1:dim(b)[1]) {
      if (length(intersect(b[k,1], ns)) > 0) {
        jh.1.adj[j,which(ns == b[k,1])] = 1
      }
    }
  }
}
a = (jh.1.adj + t(jh.1.adj)) / 1.9
jh.1.adj = round(a)

adj.mat = network(x = jh.1.adj, vertex.attrnames = list("vertex.names"),
                  vertex.attr = list(corr.names[ns]), directed = FALSE)

try.fit = ergmm(adj.mat ~ euclidean(d=2))
try.fit = ergmm(adj.mat ~ euclidean(d=2, G=3))
plot(try.fit)

apply(try.fit$sample$Z, c(2,3), mean)

network.vertex.names(adj.mat)
pts = apply(try.fit$sample$Z, c(2,3), mean)
plot(pts)
identify(pts, labels = network.vertex.names(adj.mat))

