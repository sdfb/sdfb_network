#@S Original network.plots.R sent by Mike Finegold
#@L This is not going to be documented any more than it currently is

library(network)
library(ggplot2)
library(sna)
library(ergm)

###Inputs
edge.weight.range <- c(0.1,1) ##Controls amount of edge ink
name.size.range <- c(2,3) ##Controls amount of text ink
#names <- c("Milton","Bacon","Charles","Richard","Marvel","James","Harrington","Percy","Chamberlain","Churchil","Essex","Norfolk","Suffolk","Dover","York")
names <- c("Milton","Bacon","Charles","Richard","Marvel","James","Harrington","Percy","Chamberlain","Churchil","Essex","Norfolk","Suffolk","Dover","York","Shakespeare","Henry","Falstaff","Louis","Frank","George","Tom","Dick","Jonathan","Hamlet","Othello","Macbeth","Julius","Antony","Cleopatra")
density <- 0.2 ## Controls edges in simulated network - we will use actual data


nodes <- length(names)
net <- network(nodes, directed=FALSE, density=density) #create netowrk object
classes <- rbinom(nodes,4,0.5) #Random labels, but a placeholder for future labelling of different types of actors

set.vertex.attribute(net, "elements", classes)
set.vertex.attribute(net, "names", names)

m <- as.matrix.network.adjacency(net)

###Create plotting coordinate matrix for nodes
plotcord <- data.frame(gplot.layout.kamadakawai(m, NULL)) ### Or you can use the method for generating coordinates you used to create the James Harrington graph with Harrington fixed in the center
colnames(plotcord) = c("X1","X2")
plotcord$elements <- as.factor(get.vertex.attribute(net, "elements"))
plotcord$size <- rgamma(nrow(plotcord),2,1)*2 ## Random node sizes, but we can either keep constant or use some actor attribute (eg, # mentions or #docs)
plotcord$size <-(plotcord$size/max(plotcord$size))*diff(range(name.size.range))+name.size.range[1]
plotcord$names <- get.vertex.attribute(net, "names")

###Create plotting matrix for edges
edglist <- as.matrix.network.edgelist(net)
edges <- data.frame(plotcord[edglist[,1],c(1,2)], plotcord[edglist[,2],c(1,2)])
colnames(edges) <-  c("X1","Y1","X2","Y2")
edges$weight <- rgamma(nrow(edges),2,1)/4 #add weights to each edge - done arbitrarily here - but could use regression coefficient or confidence level
edges$weight <-(edges$weight/max(edges$weight))*diff(range(edge.weight.range))+edge.weight.range[1]




ggplot() +
geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2,size=weight), data=edges, colour="grey") +
#geom_point(aes(X1, X2,colour=elements,size=size), data=plotcord) + #puts the nodes where plotcord tell them to be and colors them based on plotcord$elements
#geom_point(aes(X1, X2,size=size), data=plotcord,color="black",fill="white",pch=21) + #same as above but color is white
geom_text(aes(x=X1,y=X2,label=names,size=size),colour="blue",data=plotcord) +
scale_size(range=range(c(edge.weight.range,name.size.range))) +
scale_colour_brewer(palette="Set1") +
scale_x_continuous(breaks = NA) + scale_y_continuous(breaks = NA) +
# discard default grid + titles in ggplot2 
opts(panel.background = theme_blank()) + opts(legend.position="none")+
opts(axis.title.x = theme_blank(), axis.title.y = theme_blank()) +
opts( legend.background = theme_rect(colour = NA)) + 
opts(panel.background = theme_rect(fill = "white", colour = NA)) + 
opts(panel.grid.minor = theme_blank(), panel.grid.major = theme_blank())




