#@S Experimental code to test out manual distance setting in kamadakawai
#@L Warning: code is possibly quite raw / poorly documented / might not work (variables missing). 


names = 1:6

adj.mat = matrix(c(1,1,0,1,1,0,
                   1,1,0,0,0,1,
                   0,0,1,0,1,1,
                   1,0,0,1,0,0,
                   1,0,1,0,1,1,
                   0,1,1,0,1,1), nrow = 6)

net = as.network(adj.mat, matrix.type = "adjacency")

set.vertex.attribute(net, "names", names)

m <- as.matrix.network.adjacency(net)

dist.mat  = matrix(c(1,1,0,1,1,0,
                     1,1,0,0,0,1,
                     0,0,1,0,1,1,
                     1,0,0,1,0,0,
                     1,0,1,0,1,1,
                     0,1,1,0,1,1), nrow = 6)
for(j in 1:6) {
  dist.mat[j,dist.mat[j,] == 0] = Inf
}

###Create plotting coordinate matrix for nodes
plotcord <- data.frame(gplot.layout.kamadakawai(m, list(elen = dist.mat))) ### Or you can use the method for generating coordinates you used to create the James Harrington graph with Harrington fixed in the center
colnames(plotcord) = c("X1","X2")
plotcord$elements <- as.factor(get.vertex.attribute(net, "elements"))

plotcord$size <- rnorm(6)


plotcord$size <-(plotcord$size/max(plotcord$size))*diff(range(name.size.range))+name.size.range[1]
plotcord$names <- get.vertex.attribute(net, "names")

# center matrix
plotcord[,1:2] = center.matrix(plotcord[,1:2], center = apply(plotcord[1:2,1:2], 2, mean))

rot.deg = atan( (plotcord[1,2] - plotcord[2,2]) / (plotcord[1,1] - plotcord[2,1]) )
plotcord[,1:2] = rotate.matrix(plotcord[,1:2], rotate = rot.deg)

#plotcord[,1:2] = scale.matrix(plotcord[,1:2], scale = plot.scale)
#plotcord[,1:2] = rotate.matrix(plotcord[,1:2], rotate = plot.rotate)




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
