load("/pylon2/hm4s82p/walling/data/odnb_modern/dataset5_test_PGLFit-mc.Rdata")

library(igraph)
library(data.table)

# Need to reformat results into node and links data structures as expected by igraph
links.df = bootstrap_combined
nodes = as.data.frame(persons)


# igraph Graph
net <- graph_from_data_frame(d=links.df, vertices=nodes, directed=F)

######
# Export
######

source("/home/walling/sdfb_network/code/hpc/network_estimation/export_graph.R")

saveAsGEXF(net, file="/home/walling/dataset5-test.gexf")


#######
# Plots
#######

# igraph plot
# net.simple = simplify(net, remove.multiple = F, remove.loops = T) 
# plot(net.simple, 
#      layout = layout.reingold.tilford,
#      edge.width = 1,
#      edge.arrow.width = 0.3,
#      vertex.size = 1,
#      edge.arrow.size = 0.5,
#      vertex.size2 = 1,
#      vertex.label.cex = 1,
#      asp = 0.35, 
#      margin = -0.1)
# #tkplot(net.simple) # Not working on dev box, no TCL/TK support
# 
# # qgraph plots
# library(qgraph)
# g <- barabasi.game(355, directed=FALSE)
# g <- net
# e <- get.edgelist(g)
# 
# 
# l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(g))
# plot(g,layout=l,vertex.size=4,vertex.label=NA)
# mtext("qgraph.layout.fruchtermanreingold default", side=1)
# 
# l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(g),
#                                        area=8*(vcount(g)^2),repulse.rad=(vcount(g)^3.1))
# plot(g,layout=l,vertex.size=4,vertex.label=NA)
# mtext("qgraph.layout.fruchtermanreingold modified", side=1)
# 
# 
# # Clean up Isolated nodes
# iso <- V(net)[degree(net)==0]
# net.cleaned = delete.vertices(net, iso)


