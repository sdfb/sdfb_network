tab = read.csv("tempmarv.csv", stringsAsFactors = FALSE)

nbrs = union(which(tab[,3] > 59),which(tab[,6] > 59))
library(gridExtra)


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


##### Specific Plot Settings #####
# Input for white JH edges
edge.weight.range = c(.5)
name.size.range = 5
network.params = list(type = "ego", 
                      topK = 40, threshold = .59,
                      node.col = c("blue", "brown"),
                      edge.col = c("white", "grey90"),
                      use.edge.wts = FALSE, 
                      curve.edge = c(FALSE, TRUE))

load("SS.small.Rdata")
sim.lambs = total.lamb

  ## Generate coordinates depending on type

ego.center.nodenum = which(id.name.count$Entity.Name == "Andrew Marvell")
ego.other.nodenum = match(tab$Name[nbrs], id.name.count$Entity.Name)
node.ids = c(ego.center.nodenum, ego.other.nodenum)
names = id.name.count[node.ids,2]

# fix sim.matrix

sim.lambs[ego.center.nodenum,] = 0
sim.lambs[ego.center.nodenum,match(tab$Name, id.name.count$Entity.Name)] = 1
sim.lambs[,ego.center.nodenum] = sim.lambs[ego.center.nodenum,]

  sub.matrix = sim.lambs[node.ids,node.ids] #subset of lambda matrix
  adj.mat = matrix(as.numeric(sim.lambs[node.ids,node.ids] > network.params$threshold), nrow = length(node.ids)) # checking vs $threshold
  
  sub.matrix = sub.matrix * adj.mat # keeping only nonzero if above threshold
  
  net = as.network(x = adj.mat, matrix.type = "adjacency", directed = FALSE)
  
  # set.vertex.attribute(net, "elements", classes)
  set.vertex.attribute(net, "names", names)
  
  m <- as.matrix.network.adjacency(net)


  plotcord <- data.frame(gplot.layout.kamadakawai(m, NULL)) ### Or you can use the method for generating coordinates you used to create the James Harrington graph with Harrington fixed in the center
  colnames(plotcord) = c("X1","X2")

##############################

## Rotate (and center) node coordinates as necessary
  plotcord[,1:2] = center.matrix(plotcord[,1:2], center = plotcord[1,1:2])

##############################

# UNUSED:
# plotcord$elements <- as.factor(get.vertex.attribute(net, "elements"))

## Assign node sizes (based on log(document count))
plotcord$size <- log(id.name.count[node.ids,3])
plotcord$size <-(plotcord$size/max(plotcord$size))*diff(range(name.size.range))+name.size.range[1]
##############################

## Assign node colors
plotcord$names <- get.vertex.attribute(net, "names")
plotcord$color <- "blue"

  plotcord$color = rep(network.params$node.col[2], times = length(names))
  plotcord$color[1] = network.params$node.col[1]
##############################

## Create plotting matrix for edges
edglist <- as.matrix.network.edgelist(net)
edges <- data.frame(plotcord[edglist[,1],c(1,2)], plotcord[edglist[,2],c(1,2)])
colnames(edges) <-  c("X1","Y1","X2","Y2")
##############################

## Add Edge Weights (widths)
edges$weight <- rep(0, times = nrow(edges))

for(j in 1:length(edges$weight)) {
  edges$weight[j] = log(sub.matrix[edglist[j,1],edglist[j,2]])
}
edges$weight = edges$weight + abs(min(edges$weight)) + 3
edges$weight <-(edges$weight/max(edges$weight))*diff(range(edge.weight.range))+edge.weight.range[1]
##############################

## Edge Colors
edges$color <- rep("black", times = nrow(edges))
  n = nrow(edges)
  s = NULL
  for(j in 1:n) {
    if (any(edglist[j,] == 1)) {
      edges$color[j] = network.params$edge.col[1]
      s = c(s, j)
    } else {
      edges$color[j] = network.params$edge.col[2]
    }
  }
  ss = rep(0, times = n)
  ss[s] = 1

##############################
## Note ss > 0 indicates that that specific edge has at least one end on target

## Edges connecting to target (curved/straight)
adjacencyList = edglist[ss > 0,]
layoutCoordinates <- as.matrix(plotcord[,1:2])
colnames(layoutCoordinates) = c("x", "y")

allEdges <- lapply(1:nrow(adjacencyList), edgeMaker, len = 200, curved = network.params$curve.edge[1])
allEdges1 <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^
edg.color1 = rep(edges$color[ss > 0], each = 200)
edg.weight1 = rep(edges$weight[ss > 0], each = 200)
##############################

## Edges for others (curved/straight)
adjacencyList = edglist[ss == 0,,drop = FALSE]
layoutCoordinates <- as.matrix(plotcord[,1:2])
colnames(layoutCoordinates) = c("x", "y")

allEdges <- lapply(1:nrow(adjacencyList), edgeMaker, len = 200, curved = network.params$curve.edge[2])
allEdges2 <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^
edg.color2 = rep(edges$color[ss == 0], each = 200)
edg.weight2 = rep(edges$weight[ss == 0], each = 200)
##############################



first.cols = rep("grey90", times = 38)
second.cols = first.cols
first.cols[tab[nbrs,3] > 59 & tab[nbrs,6] < 60] = "grey15"
first.cols[tab[nbrs,6] > 59 & tab[nbrs,3] < 60] = "white"

second.cols[tab[nbrs,3] > 59 & tab[nbrs,6] < 60] = "white"
second.cols[tab[nbrs,6] > 59 & tab[nbrs,3] < 60] = "grey15"

adjacencyList = edglist[ss > 0,]
layoutCoordinates <- as.matrix(plotcord[,1:2])
colnames(layoutCoordinates) = c("x", "y")

allEdges <- lapply(1:nrow(adjacencyList), edgeMaker, len = 200, curved = network.params$curve.edge[1])
allEdges1 <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^
edg.color1 = rep(first.cols, each = 200)
edg.weight1 = rep(edges$weight[ss > 0], each = 200)

edges.torm.B = which(c(FALSE, tab[nbrs,6] > 59 & tab[nbrs,3] < 60))
edges.torm.A = which(c(FALSE, tab[nbrs,3] > 59 & tab[nbrs,6] < 60))

plotcordB = plotcord
plotcordB$color[-1] = 'lightsalmon1'
plotcordB$color[edges.torm.A] = 'black'
plotcordB$color[edges.torm.B] = 'white'

plotcordA = plotcord
plotcordA$color[-1] = 'lightsalmon1'
plotcordA$color[edges.torm.B] = 'black'
plotcordA$color[edges.torm.A] = 'white'

ae1 = t(sapply(as.character(allEdges1$Group), function(x) {as.numeric(strsplit(x, ">")[[1]])}))
ae1 = union(which(!is.na(match(ae1[,1], edges.torm.B))), which(!is.na(match(ae1[,2], edges.torm.B))) )

ae2 = t(sapply(as.character(allEdges2$Group), function(x) {as.numeric(strsplit(x, ">")[[1]])}))
ae2 = union(which(!is.na(match(ae2[,1], edges.torm.B))), which(!is.na(match(ae2[,2], edges.torm.B))) )

before.plot <- ggplot() + 
  geom_segment(aes(x=1.2*min(edges$X1), y=1.2*min(edges$Y1), xend = 1.2*max(edges$X1), yend = 1.2*max(edges$Y2),size=5), colour="white") +
  geom_path(aes(x = x, y = y, group = Group), colour = edg.color2[-ae2], 
            size = edg.weight2[-ae2], data=allEdges2[-ae2,]) +
  geom_path(aes(x = x, y = y, group = Group), colour = edg.color1[-ae1], 
            size = edg.weight1[-ae1], data=allEdges1[-ae1,]) +
  # geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2,size=weight), data=edges, colour=edges$color) +
  #geom_point(aes(X1, X2,colour=elements,size=size), data=plotcord) + #puts the nodes where plotcord tell them to be and colors them based on plotcord$elements
  #geom_point(aes(X1, X2,size=size), data=plotcord,color="black",fill="white",pch=21) + #same as above but color is white
  geom_text(aes(x=X1,y=X2,label=names,size=size),colour=plotcordB$color,data=plotcordB) +
  scale_colour_brewer(palette="Set1") +
  scale_x_continuous(breaks = NA) + scale_y_continuous(breaks = NA) +
  # discard default grid + titles in ggplot2 
  opts(panel.background = theme_blank()) + opts(legend.position="none")+
  opts(axis.title.x = theme_blank(), axis.title.y = theme_blank()) +
  opts( legend.background = theme_rect(colour = NA)) + 
  opts(panel.background = theme_rect(fill = "white", colour = NA)) + 
  ggtitle("Ego-Centric Graph: First Result") +
  opts(panel.grid.minor = theme_blank(), panel.grid.major = theme_blank())

adjacencyList = edglist[ss > 0,]
layoutCoordinates <- as.matrix(plotcord[,1:2])
colnames(layoutCoordinates) = c("x", "y")

allEdges <- lapply(1:nrow(adjacencyList), edgeMaker, len = 200, curved = network.params$curve.edge[1])
allEdges1 <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^
edg.color1 = rep(second.cols, each = 200)
edg.weight1 = rep(edges$weight[ss > 0], each = 200)


ae1 = t(sapply(as.character(allEdges1$Group), function(x) {as.numeric(strsplit(x, ">")[[1]])}))
ae1 = union(which(!is.na(match(ae1[,1], edges.torm.A))), which(!is.na(match(ae1[,2], edges.torm.A))) )

ae2 = t(sapply(as.character(allEdges2$Group), function(x) {as.numeric(strsplit(x, ">")[[1]])}))
ae2 = union(which(!is.na(match(ae2[,1], edges.torm.A))), which(!is.na(match(ae2[,2], edges.torm.A))) )

after.plot <- ggplot() + 
  geom_segment(aes(x=1.2*min(edges$X1), y=1.2*min(edges$Y1), xend = 1.2*max(edges$X1), yend = 1.2*max(edges$Y2),size=5), colour="white") +
  geom_path(aes(x = x, y = y, group = Group), colour = edg.color2[-ae2], 
            size = edg.weight2[-ae2], data=allEdges2[-ae2,]) +
  geom_path(aes(x = x, y = y, group = Group), colour = edg.color1[-ae1], 
            size = edg.weight1[-ae1], data=allEdges1[-ae1,]) +
  # geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2,size=weight), data=edges, colour=edges$color) +
  #geom_point(aes(X1, X2,colour=elements,size=size), data=plotcord) + #puts the nodes where plotcord tell them to be and colors them based on plotcord$elements
  #geom_point(aes(X1, X2,size=size), data=plotcord,color="black",fill="white",pch=21) + #same as above but color is white
  geom_text(aes(x=X1,y=X2,label=names,size=size),colour=plotcordA$color,data=plotcordA) +
  scale_colour_brewer(palette="Set1") +
  scale_x_continuous(breaks = NA) + scale_y_continuous(breaks = NA) +
  # discard default grid + titles in ggplot2 
  opts(panel.background = theme_blank()) + opts(legend.position="none")+
  opts(axis.title.x = theme_blank(), axis.title.y = theme_blank()) +
  opts( legend.background = theme_rect(colour = NA)) + 
  opts(panel.background = theme_rect(fill = "white", colour = NA)) + 
  ggtitle("Ego-Centric Graph: After Expert Validation/Refit") +
  opts(panel.grid.minor = theme_blank(), panel.grid.major = theme_blank())

pdf(paste(format(Sys.time(), "%a%b%d%H%M%S"), ".pdf"), width=8, height=12)

grid.arrange(before.plot, after.plot, nrow = 2)
dev.off()
