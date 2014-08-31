## Code Content
# 1. file/package loading, and general functions
# 2. General/Specific Plot settings (and lots of examples, essentially all the plots I made)
# 3. Long section of code to run to generate plot, based on (2)


##### File/Package Loading #####

# These two files are just loading the graph data
id.name.count = read.csv("new.idnamecount.csv", stringsAsFactors = FALSE)[,-1]
load("sparse.sim.lambs.Rdata")

# these are needed for all plots
library(network)
library(ggplot2)
library(sna)
library(ergm)

# these two packages are for the curved-edges
library(Hmisc)
library(reshape2)

##### Functions #####

# given a vector, returns indices of the largest 'k' values (needed to find top k edges)
topk = function(dat, k) {
  k = min(k, sum(!is.na(dat)))
  return(order(dat,decreasing= TRUE)[1:k])
}

# given a 2-column matrix 'mat' of coordinates (x,y), re-adjusts coordinates so that 'center'
#  is now (0,0)
center.matrix = function(mat, center = c(0,0)) {
  cent.mat = mat
  cent.mat[,1] = center[1]
  cent.mat[,2] = center[2]
  return(mat - cent.mat)
}

# given a 2-column matrix 'mat' of coordinates (x,y), rotates the coordinates by 'rotate' radians
rotate.matrix = function(mat, rotate = 0) {
  rot.mat = matrix(0, nrow = 2, ncol = 2)
  rot.mat[1,1] = cos(rotate)
  rot.mat[1,2] = sin(rotate)
  rot.mat[2,1] = -sin(rotate)
  rot.mat[2,2] = cos(rotate)
  
  res = rot.mat %*% t(mat)
  return(t(res))
}

# Function pulled from 
# http://is-r.tumblr.com/post/38459242505/beautiful-network-diagrams-with-ggplot2
# to generate curved edges
edgeMaker <- function(whichRow, len = 100, curved = TRUE){
  fromC <- layoutCoordinates[adjacencyList[whichRow, 1], ]  # Origin
  toC <- layoutCoordinates[adjacencyList[whichRow, 2], ]  # Terminus
  
  # Add curve:
  graphCenter <- colMeans(layoutCoordinates)  # Center of the overall graph
  bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
  distance1 <- sum((graphCenter - bezierMid)^2)
  if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
    bezierMid <- c(toC[1], fromC[2])
  }  # To select the best Bezier midpoint
  bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
  if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
  
  edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
                            c(fromC[2], bezierMid[2], toC[2]),  # X & y
                            evaluation = len))  # Bezier path coordinates
  edge$Sequence <- 1:len  # For size and colour weighting in plot
  edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
  return(edge)
}


#####################
#####################
## For any plot, setting network.params will do the following: 
## 1. Find appropriate nodes/edges for the plot in question
## 2a. IF (file) does not exist: Generate node positions based on settings, save them to file
## 2b. If (file) exists: Use node position settings in file
## 3. Generate plot/edge coloring/width information
## 4. Plot picture, save output in 2500*2000 size png in working directory
##     to a filename based on current time
#####################
#####################

##### General Plot Settings #####



##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

# generating graph file: 
# ns = nodes

load("0325sp.datamat.sparse.Rdata")
load("SS.small.Rdata")

interest = c("John Bradford", "Nicholas Ridley", "Hugh Latimer", "Thomas Cranmer", "John Careless",
             "John Philpot", "John Hooper", "William Tyms", "Bartlett Green", "Augustine Bernher",
             "Rowland Taylor")

only.exist = match(interest, colnames(sparse.dm))
only.exist = only.exist[!is.na(only.exist)]

collect.nbrs = function(nodes, thres) {
  # Collects all 'k' degree nbrs at index in 'thres'
  # 'thres' must be an increasing vector... 
  picked = nodes
  node.list = list()
  node.list[[1]] = nodes
  for(j in 1:length(thres)) {
    temp = apply(total.lamb[picked,] >= thres[j], 2, sum)
    new.nodes = which(temp > 0)
    node.list[[j+1]] = setdiff(new.nodes, picked)
    picked = unique(c(picked, new.nodes))
  }
  
  return(node.list)
}
a = collect.nbrs(only.exist, thres = c(0.7, 0.9))

m = total.lamb >= 0.7
ns = sort(c(a, recursive = TRUE))


target.mat = total.lamb[ns,ns]
colnames(target.mat) = colnames(sparse.dm)[ns]
rownames(target.mat) = colnames(sparse.dm)[ns]

tm = target.mat >= 0.7

first.mat = (target.mat + 0.5)^2
second.mat = target.mat

helper = function(x,y) {
  s = sum(x & y)
  s1 = sum(x)
  s2 = sum(y)
  if (s > 0) {
    return(max(s/s1, s/s2) + 0.1)
  } else {
    return(.1)
  }
}
mat.list = list()
for(j in 1:230) {
  mat.list[[j]] = tm[,j]
  print(j)
}
for(j in 1:230) { 
  print(j)
  second.mat[,j] = sapply(mat.list, function(x) {helper(x, tm[,j])} ) 
}

dist.mat = first.mat * second.mat
for(j in 1:230) {
  dist.mat[j,j] = 0
}

closest = apply(dist.mat, 1, function(x) {which(x == max(x))[1]})

# compile clusters
clusters = matrix(0, ncol = 5, nrow = 230)
clusters[,1] = 1:230
for(j in 1:230) {
  base = clusters[j,1]
  target = clusters[closest[j],1]
  if (base != target) {
    clusters[which(clusters[,1] == base),1] = target
  }
  print(j)
}
###############################

uc =sort(unique(clusters[,1]))
c1dist = matrix(0, ncol = length(uc), nrow = length(uc))

for(j in 1:length(uc)) {
  print(j)
  for(i in 1:length(uc)) {
    c1dist[i,j] = mean(dist.mat[which(clusters[,1] == uc[j]),which(clusters[,1] == uc[i])])
    if (i == j) {c1dist[i,j] = 0}
  }
}

c1max = apply(c1dist, 1, function(x) {which(x == max(x))[1]})
c1max.adj = uc[c1max]
c1order = rev(order(apply(c1dist, 1, max)))

table(clusters[,1])[c1order]

clusters[,2] = clusters[,1]
for(j in 1:length(uc)) {
  base = uc[j]
  target = c1max.adj[j]
  if (base != target) {
    clusters[which(clusters[,2] == base),2] = target
  }
  print(j)
}
##########################

uc2 =sort(unique(clusters[,2]))
c2dist = matrix(0, ncol = length(uc2), nrow = length(uc2))

for(j in 1:length(uc2)) {
  print(j)
  for(i in 1:length(uc2)) {
    c2dist[i,j] = mean(dist.mat[which(clusters[,2] == uc2[j]),which(clusters[,2] == uc2[i])])
    if (i == j) {c2dist[i,j] = 0}
  }
}

c2max = apply(c2dist, 1, function(x) {which(x == max(x))[1]})
c2max.adj = uc[c2max]

table(clusters[,2])

clusters[,3] = clusters[,2]
for(j in 1:length(uc2)) {
  base = uc2[j]
  target = c2max.adj[j]
  if (base != target) {
    clusters[which(clusters[,3] == base),3] = target
  }
  print(j)
}

################

uc3 =sort(unique(clusters[,3]))
c3dist = matrix(0, ncol = length(uc3), nrow = length(uc3))

for(j in 1:length(uc3)) {
  print(j)
  for(i in 1:length(uc3)) {
    c3dist[i,j] = mean(dist.mat[which(clusters[,3] == uc3[j]),which(clusters[,3] == uc3[i])])
    if (i == j) {c3dist[i,j] = 0}
  }
}

clusters[,4] = clusters[,3]
clusters[clusters[,4] == 84,4] = 119
clusters[clusters[,4] == 118,4 ] = 88

table(clusters[,4])
clusters[,4] = match(clusters[,4], c(60,73,88,119,125))
c4dist = matrix(0, nrow = 5, ncol = 5)
for(j in 1:5) { for(i in 1:5) {
  c4dist[i,j] = mean(dist.mat[which(clusters[,4] == i), which(clusters[,4] == j)])
}}
num.link = c4dist
for(j in 1:5) { for(i in 1:5) {
  num.link[i,j] = sum(tm[which(clusters[,4] == i), which(clusters[,4] == j)])
}}
table(clusters[,4])
b = c(39,32,39,59,61)
num.link / b %*% t(b)

##############################################3
thickness.matrix = tm * target.mat
manual.CD = matrix(c(3,14,10,14,10,
                     14,3,10,20,10,
                     10,10,3,14,14,
                     14,20,14,3,20,
                     10,10,14,20,3), nrow =5 , ncol = 5)

length.matrix = matrix(0, nrow = 230, ncol = 230)
for(j in 1:5) {for(i in 1:5) {
  
    is = which(clusters[,4] == i)
    js = which(clusters[,4] == j)
    length.matrix[is,js] = matrix(manual.CD[i,j], nrow = length(is), ncol = length(js)) - matrix(2 * thickness.matrix[is,js], nrow = length(is), ncol = length(js))
  
}}






to.write = ""
to.write[1] = "graph testg {"
to.write[2] = "graph [overlap=prism]"
to.write[3] = "graph [model=mds]"
to.write[4] = "node [shape=point]"
for(j in 1:length(ns)) {
  name = colnames(sparse.dm)[ns[j]]
  to.write[j+3] = paste(ns[j]," [label=\"", name, "\", shape=circle", "]",
                        sep = "")
  print(j)
}
for(j in 1:(length(ns) - 1)) {
  print(j)
  for(k in (j+1):length(ns)) {
    if (tm[j,k]) {
      temp = c(temp,
               paste(ns[j], " -- ", ns[k],
                     " [color=darkgreen ","len=",length.matrix[j,k] ,
                     " penwidth=", round((target.mat[j,k]+0.3)^3,3),
                     "]",
                     sep = "") )
    } else {
      temp = c(temp,
               paste(ns[j], " -- ", ns[k],
                     " [style=\"invis\" ","len=",length.matrix[j,k] , "]",
                     sep = "") )
    }
  }
  to.write = c(to.write, temp)
}

# for(j in 1:length(ns)) {
#   b = which(m[,ns[j]] >= 0.7)
#   b = intersect(b, ns)
#   b = b[b>ns[j]]
#   temp = NULL
#   if (length(b) > 0) {
#     for(k in 1:length(b)) {
#       
#       temp = c(temp, 
#                paste(ns[j], " -- ", b[k],
#                      " [color=darkgreen ","len=", , "]",
#                      sep = "")    )
#     }
#     
#   }
#   to.write = c(to.write, temp)
#   print(j)
# }
to.write = c(to.write, "}")

writeLines(to.write, con = "earlygraph.txt")


# trying R
adj.mat = matrix(0, nrow = 230, ncol = 230) 
for(j in 1:230) {
  adj.mat[,j] = as.numeric(tm[,j])
}
net = as.network(x = adj.mat, matrix.type = "adjacency", directed = FALSE)

# set.vertex.attribute(net, "elements", classes)
set.vertex.attribute(net, "names", colnames(target.mat))

m <- as.matrix.network.adjacency(net)

net = as.network(x = adj.mat, matrix.type = "adjacency", directed = FALSE)

d = net
d <- symmetrize(d, return.as.edgelist = TRUE)
d[, 3] <- 1/d[, 3]
elen <- geodist(d, ignore.eval = FALSE)$gdist

for(i in 1:5) {
  is = which(clusters[,4] == i)
  length.matrix[is,is] = 0.5 + elen[is,is] * 1.5
}

plotcord <- data.frame(gplot.layout.kamadakawai(m, list(elen = length.matrix))) 

plotcord$names <- get.vertex.attribute(net, "names")
plotcord$color <- "darkgreen"

plot(plotcord)
interested.nodes = match(interest, plotcord$names)
plotcord$color[match(interest, plotcord$names)] = "darkblue"
plotcord$names = gsub(" ", "\n", plotcord$names)

interested.nodes = interested.nodes[!is.na(interested.nodes)]





## Create plotting matrix for edges
edglist <- as.matrix.network.edgelist(net)
edges <- data.frame(plotcord[edglist[,1],c(1,2)], plotcord[edglist[,2],c(1,2)])
colnames(edges) <-  c("X1","Y1","X2","Y2")
##############################

## Add Edge Weights (widths)
edges$weight <- rep(0, times = nrow(edges))

for(j in 1:length(edges$weight)) {
  edges$weight[j] = target.mat[edglist[j,1],edglist[j,2]]
}
edges$weight <-(edges$weight/max(edges$weight))*diff(range(edge.weight.range))+edge.weight.range[1]
##############################

## Edge Colors
edges$color <- rep("gray95", times = nrow(edges))
important = union(which(!is.na(match(edglist[,1],interested.nodes))),
      which(!is.na(match(edglist[,2], interested.nodes))))
edges$color[important] = 'lightblue'
important2 = intersect(which(!is.na(match(edglist[,1],interested.nodes))),
                  which(!is.na(match(edglist[,2], interested.nodes))))
edges$color[important2] = 'brown'
ss = as.numeric(edges$color != 'gray95')

##############################
## Note ss > 0 indicates that that specific edge has at least one end on target

## Edges connecting to target (curved/straight)
adjacencyList = edglist[ss > 0,]
layoutCoordinates <- as.matrix(plotcord[,1:2])
colnames(layoutCoordinates) = c("x", "y")

allEdges <- lapply(1:nrow(adjacencyList), edgeMaker, len = 200, curved = FALSE)
allEdges1 <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^
edg.color1 = rep(edges$color[ss > 0], each = 200)
edg.weight1 = rep(edges$weight[ss > 0], each = 200)
##############################

## Edges for others (curved/straight)
adjacencyList = edglist[ss == 0,,drop = FALSE]
layoutCoordinates <- as.matrix(plotcord[,1:2])
colnames(layoutCoordinates) = c("x", "y")

allEdges <- lapply(1:nrow(adjacencyList), edgeMaker, len = 200, curved = TRUE)
allEdges2 <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^
edg.color2 = rep(edges$color[ss == 0], each = 200)
edg.weight2 = rep(edges$weight[ss == 0], each = 200)
##############################


# Plot picture, save to .png. 
edge.weight.range = c(.1, 2)
name.size.range = 16

png(paste(format(Sys.time(), "%a%b%d%H%M%S"), ".png"), width=5000, height=4000, res=96)

ggplot() + 
  geom_segment(aes(x=1.2*min(edges$X1), y=1.2*min(edges$Y1), xend = 1.2*max(edges$X1), yend = 1.2*max(edges$Y2),size=5), colour="white") +
  geom_path(aes(x = x, y = y, group = Group), colour = edg.color2, 
            size = edg.weight2, data=allEdges2) +
  geom_path(aes(x = x, y = y, group = Group), colour = edg.color1, 
            size = edg.weight1, data=allEdges1) +
  # geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2,size=weight), data=edges, colour=edges$color) +
  #geom_point(aes(X1, X2,colour=elements,size=size), data=plotcord) + #puts the nodes where plotcord tell them to be and colors them based on plotcord$elements
  #geom_point(aes(X1, X2,size=size), data=plotcord,color="black",fill="white",pch=21) + #same as above but color is white
  geom_text(aes(x=X1,y=X2,label=names),colour=plotcord$color,data=plotcord) +
  scale_size(range=range(c(edge.weight.range,name.size.range))) +
  scale_colour_brewer(palette="Set1") +
  scale_x_continuous(breaks = NA) + scale_y_continuous(breaks = NA) +
  # discard default grid + titles in ggplot2 
  opts(panel.background = theme_blank()) + opts(legend.position="none")+
  opts(axis.title.x = theme_blank(), axis.title.y = theme_blank()) +
  opts( legend.background = theme_rect(colour = NA)) + 
  opts(panel.background = theme_rect(fill = "white", colour = NA)) + 
  opts(panel.grid.minor = theme_blank(), panel.grid.major = theme_blank())

dev.off()

