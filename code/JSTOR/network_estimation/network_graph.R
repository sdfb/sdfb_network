load("jstor_PGLFit.Rdata")

library(igraph)
library(data.table)

# Need to reformat results into node and links data structures as expected by igraph
nodes = as.data.frame(persons)
links = lapply(1:length(results), function(i) {
  cat(i)
  result = results[i][[1]] # Unlist it
  node = persons[i]
  coefs = coef(result, s = max(result$lambda))
  coefs = coefs[-1] # Remove intercept
  relationships_idx = which(coefs > 0)
  relationships = persons[-i][relationships_idx] # Need to first remove the 'target' from the list so that the other idxs line up
  relationship_weights = coefs[relationships_idx]
  if(length(relationships>0)) {
    result.df = data.frame(a=node, b=relationships, w=relationship_weights)
    return(result.df)
  }
  else {
    return(NA)
  }  
})
links.df = rbindlist(links[!is.na(links)])

# igraph Graph
net <- graph_from_data_frame(d=links.df, vertices=nodes, directed=F)

# igraph plot
net.simple = simplify(net, remove.multiple = F, remove.loops = T) 
plot(net.simple, 
     layout = layout.reingold.tilford,
     edge.width = 1,
     edge.arrow.width = 0.3,
     vertex.size = 1,
     edge.arrow.size = 0.5,
     vertex.size2 = 1,
     vertex.label.cex = 1,
     asp = 0.35, 
     margin = -0.1)
#tkplot(net.simple) # Not working on dev box, no TCL/TK support

# qgraph plots
library(qgraph)
g <- barabasi.game(355, directed=FALSE)
g <- net
e <- get.edgelist(g)


l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(g))
plot(g,layout=l,vertex.size=4,vertex.label=NA)
mtext("qgraph.layout.fruchtermanreingold default", side=1)

l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(g),
                                       area=8*(vcount(g)^2),repulse.rad=(vcount(g)^3.1))
plot(g,layout=l,vertex.size=4,vertex.label=NA)
mtext("qgraph.layout.fruchtermanreingold modified", side=1)


# Clean up Isolated nodes
iso <- V(net)[degree(net)==0]
net.cleaned = delete.vertices(net, iso)


