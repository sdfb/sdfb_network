## This contains code for some graphviz plots. This generates some james harrington plots. 



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


