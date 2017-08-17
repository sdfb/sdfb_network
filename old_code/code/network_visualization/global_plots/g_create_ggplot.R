#@S Code that extracts data from given coordinate file (graphvis text output) 
#@S    and generates ggplot object. 

# TODO: Eventually split this into multiple parts? 

# must load in vector of names from some source, that match IDs. 

# Old settings?
# edge.weight.range <- c(0.5,1.5) ##Controls amount of edge ink
# name.size.range <- c(15,30) ##Controls amount of text ink
# name.size.range <- c(10,20) ##Controls amount of text ink - FOR TOPIC MODEL

G_PLOTCORD = NULL
G_SETTINGS = NULL

g_create_ggplot <- function(node_names, node_sizes, graphviz_outfile,
                               node_size_range = c(5,8), color_function) {
  #@F ----------------------------------------
  #@F Function 'g_create_ggplot'
  #@F Args (Input): 
  #@F   node_names = vector of node names
  #@F   node_sizes = vector of node sizes
  #@F   graphviz_outfile = output from graphviz algorithm (node placements)
  #@F   node_size_range = size to rescale node sizes to
  #@F   color_function = function (of node_names) that provides sizes of each node. 
  #@F Purpose: Generates needed global varaibles in order to plot a specific graph
  #@F   visualization, based on given node names/sizes, and graphviz node locations. 
  #@F Output: none
  #@F Global Variables Needed: G_PLOTCORD, G_SETTINGS. These are assigned to. 
  #@F ----------------------------------------
  
  # TODO: Add actual ggplot code inside here?

  # TODO: Play with node_size_range
  # this ignores edge drawing for now
  
  NODES = length(node_names)
  graphdata = readLines(graphviz_outfile)
  
  graphsize = graphdata[1]
  nodelocs = strsplit(graphdata[2:(NODES + 1)], split = " ")
  edgelines = strsplit(graphdata[(NODES + 2):length(graphdata)], split = " ")
  
  plotcord = as.data.frame(t(
    sapply(nodelocs, function(x) {as.numeric(x[3:4])})
  ))
  
  edglist = as.data.frame(t(
    sapply(edgelines, function(x) {as.numeric(x[2:3])})
  ))
  
  colnames(plotcord) = c("x", "y")  # NOTE NOTE NOTE NOTE Changed from X1/X2 to x,y
  plotcord$size <- node_sizes
  plotcord$names <- node_names
  
  # Rescale node sizes
  # plotcord$size <- (plotcord$size/max(plotcord$size)) * diff(range(node_size_range)) + 
  #   node_size_range[1]
  
  plotcord = cbind(plotcord, color_function(node_names))
  
  G_PLOTCORD <<- plotcord
  G_SETTINGS <<- list(node_size_range = node_size_range)
  
  # Ignore edges?
  # edges1 = rep(-1, times = dim(edges)[1])
  # e1 = rep("grey", times = length(edges1))
  
  return(NULL)
}


generate_birthbased_colors = function(node_names) {
  #@F ----------------------------------------
  #@F Function 'generate_birthbased_colors'
  #@F Args (Input): node_names: identifies which names to look for dates of
  #@F Purpose: Provides information as to what colors to use
  #@F Output: data frame, columns: color, year of birth
  #@F ----------------------------------------
  

  
  load("network_visualization/est.birthdates.Rdata")
  ind = match(node_names, dm.names)
  breaks = 150:170 * 10  
  
  # give large #s to small dates
  cols = sapply(dmname.bdate[ind], function(x) {return(sum(x < breaks, na.rm = 1))})
  
  #cols_touse = topo.colors(n = 7)
  cols_touse = rainbow(n = (length(breaks) + 1), start = 3/6, e = 4/6)
  
  res = as.character(cols)
  for(j in 0:length(breaks)) {
    res[cols == j] = cols_touse[j+1]
  }
 
  return(data.frame(color = res, year = dmname.bdate[ind], stringsAsFactors = FALSE))
}

generate_graphdist_colors = function() {
  #@F ----------------------------------------
  #@F Function 'generate_graphdist_colors'
  #@F Args (Input):
  #@F Purpose:
  #@F Example:
  #@F Output:
  #@F Notes: 
  #@F Global Variables Needed:
  #@F ----------------------------------------
  
  # TODO: Write this based on existing code
  
  
#   #################################
#   # separate for many nodes: coloration, etc. 
#   
#   
#   ###Create plotting matrix for edges
#   edglist <- as.matrix.network.edgelist(net)
#   edges <- data.frame(plotcord[edglist[,1],c(1,2)], plotcord[edglist[,2],c(1,2)])
#   colnames(edges) <-  c("X1","Y1","X2","Y2")
#   edges$weight <- rep(1, times = nrow(edges))
#   
#   
#   ## Find color setting 1, Charles I dist. 
#   colors1 = rep(-1, times = N)
#   edges1 = rep(-1, times = dim(edges)[1])
#   start1 = 487 #Charles I
#   
#   used.nodes = NULL
#   nodes.left = 1:N
#   dist = 0
#   set = start1
#   nodes.left = setdiff(nodes.left, set)
#   while(length(set) > 0) {
#     colors1[set] = dist
#     new.edges = NULL
#     for(j in 1:length(set)) {
#       new.edges = c(new.edges, which(edglist[,1] == set[j]), 
#                     which(edglist[,2] == set[j]))
#     }
#     new.edges = intersect(new.edges, which(edges1 == -1))
#     edges1[new.edges] = dist
#     
#     new.nodes = intersect(nodes.left, unique(as.numeric(edglist[new.edges,1:2])))
#     used.nodes = unique(c(used.nodes, set))
#     set = new.nodes
#     nodes.left = setdiff(nodes.left, new.nodes)
#     dist = dist + 1
#   }
#   colors1[colors1 == -1] = 5
#   edges1 = edges1 + 1
#   
#   c1 = rep("", times = length(colors1))
#   e1 = rep("", times = length(edges1))
#   
#   #use heat colors
#   cols.touse = topo.colors(n = 6)
#   
#   for(j in 0:5) {
#     c1[colors1 == j] = cols.touse[j+1]
#     e1[edges1 == j] = cols.touse[j+1]
#   }
#   
#   
#   
#   
#   ## Find color setting 1, James Harington dist. 
#   colors1 = rep(-1, times = N)
#   edges1 = rep(-1, times = dim(edges)[1])
#   start1 = 2054 #Charles I
#   
#   used.nodes = NULL
#   nodes.left = 1:N
#   dist = 0
#   set = start1
#   nodes.left = setdiff(nodes.left, set)
#   while(length(set) > 0) {
#     colors1[set] = dist
#     new.edges = NULL
#     for(j in 1:length(set)) {
#       new.edges = c(new.edges, which(edglist[,1] == set[j]), 
#                     which(edglist[,2] == set[j]))
#     }
#     new.edges = intersect(new.edges, which(edges1 == -1))
#     edges1[new.edges] = dist
#     
#     new.nodes = intersect(nodes.left, unique(as.numeric(edglist[new.edges,1:2])))
#     used.nodes = unique(c(used.nodes, set))
#     set = new.nodes
#     nodes.left = setdiff(nodes.left, new.nodes)
#     dist = dist + 1
#   }
#   colors1[colors1 == -1] = 5
#   edges1 = edges1 + 1
#   
#   c1 = rep("", times = length(colors1))
#   e1 = rep("", times = length(edges1))
#   
#   #use heat colors
#   cols.touse = topo.colors(n = 6)
#   cols.touse = rainbow(n = 6, start = 3/6, e = 4/6)
#   
#   for(j in 0:5) {
#     c1[colors1 == j] = cols.touse[j+1]
#     e1[edges1 == j] = cols.touse[j+1]
#   }
#   
#   
#   
}

generate_topicmodel_colors = function() {
  #@F ----------------------------------------
  #@F Function ''
  #@F Args (Input):
  #@F Purpose:
  #@F Example:
  #@F Output:
  #@F Notes: 
  #@F Global Variables Needed:
  #@F ----------------------------------------
  
  # TODO: Write this based on existing code [ FIND MORE CODE RELATED TO THIS... 
  # TODO: -- in some file...]
  
  
#   
#   
#   ## Using topicmodel topics
#   load("topicmodel.fit.Rdata")
#   
#   cols.touse = c('red', 'blue', 'green', 'black', 'brown')
#   cols.touse = c('red', 'beige', 'beige', 'beige', 'beige')
#   cols.touse = c('beige', 'blue', 'beige', 'beige', 'beige')
#   cols.touse = c('beige', 'beige', 'green', 'beige', 'beige')
#   cols.touse = c('beige', 'beige', 'beige', 'black', 'beige')
#   cols.touse = c('beige', 'beige', 'beige', 'beige', 'brown')
#   
#   #new
#   
#   load("tms.Rdata")
#   
#   k = 10
#   cols.touse = rep('beige', times = 10)
#   cols.touse[k] = 'blue'
#   c1 = cols.touse[topics(topic.models)] #need to load topicmodel
#   
#   
  
}



