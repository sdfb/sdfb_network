library(huge)


aa$lambda

aa = huge(s.cov.subset, lambda = c(0.25, 0.2, 0.15,0.1))


aa$path[[3]]

find.nodenum.from.name(query = "Charles")
to.keep = find.names.from.nodenum(edge.mat.small = aa$path[[4]], nodenum = 235 )[[1]][[2]]
nodes = rbind(c("235", "Charles I"),to.keep)[c(-3,-10),]


grp = graph.adjacency( adjmatrix = aa$path[[4]][as.numeric(nodes[,1]),as.numeric(nodes[,1])], mode = "undirected")

save(aa,find.nodenum.from.name, find.names.from.nodenum, to.keep,
     nodes, grp, reordered.names, coords.totest, file = "newplots.Rdata")

load("newplots.Rdata")


?plot.igraph
tkplot(grp, canvas.width = 450, canvas.height = 450)
coords.totest = tkplot.getcoords(8)

pdf("test.pdf")
plot.igraph(grp, layout = coords.totest, vertex.label = nodes[,2], 
            vertex.label.cex = 0.7, vertex.frame.color = "white",
            vertex.label.color = "black")
dev.off()



