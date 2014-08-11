#@S Code for function create_edges
#@L This function generates plotting information for all the edges (color,width,curvature,etc)

# TODO: [Idea] Edge plotting priority list: certain tpyes of edges need to be plotted 
# TODO: -- in the background, in certain situations. right now they may cause mess. 

create_edges = function(network_params, node_data, subset_matrix, net, adjm, plotcord, log_file, verbose) {
  #@F ----------------------------------------
  #@F Function 'create_edges'
  #@F Args (Input):
  #@F   - network_params = output of 'create_plot_params' (a list)
  #@F   - lambda_matrix = confidence matrix, values between 0 to 1. Expect row/columns to be named. 
  #@F   - log_file = file to store additional text output (logging of the processing)
  #@F   - verbose = logical: determines amount of output
  #@F 
  #@F   - subset_matrix, net, adjm, node_data: All these are output of create_network_object
  #@F   - plotcord: output of create_nodes
  #@F
  #@F Purpose: Generate edge locations, colors, widths, for plotting
  #@F Output: 
  #@F   edges: data frame, with columns. this stores all edge information (not for plotting)
  #@F     - X1,Y1,X2,Y2 = x/y coordinates; start/end coordinates
  #@F     - weight = edge plotting weight
  #@F     - color = edge plotting color
  #@F   edge_plot: 
  # TODO: [Document] edge_plot data frame. I need to test this to see wht it is. 
  #@F ----------------------------------------
  
  output_log("Generating edge plotting information...", file_out = log_file, verbose = verbose)
  
  edglist = as.matrix.network.edgelist(net)
  edges = data.frame(plotcord[edglist[,1],c(1,2)], plotcord[edglist[,2],c(1,2)])
  colnames(edges) <-  c("X1","Y1","X2","Y2")
  
  ## Add edge weight if necessary
  edges$weight = 1
  
  for(j in 1:length(edges$weight)) {
    edges$weight[j] = subset_matrix[edglist[j,1],edglist[j,2]]
  }
  edges$weight <- edges$weight*diff(range(network_params$plot_size_settings$edge))+min(network_params$plot_size_settings$edge)
  # TODO: Add Edge weights: The code works, but the edge differeces are not really noticable
  # TODO: -- thus, more work needed to make plotting edge weights useful. 
  
  ## add edge colors as necessary
  edges$color = 'nocolor'
  
  type1s = node_data$type[edglist[,1]]
  type2s = node_data$type[edglist[,2]]
  tab = network_params$edge_setting_table
  is_curved = rep(FALSE, times = length(type1s)) # also records curved edges here
  
  for (j in 1:nrow(tab)) {
    matches = match_two_columns(type1s, type2s, target1 = tab$type1[j], target2 = tab$type2[j])
    edges$color[matches] = tab$color[j]
    is_curved[matches] = tab$curve[j]
  }
  
  # Error checking
  if (any(edges$color == 'nocolor')) {
    output_log("--Edge coloration not complete: unnoted edges are in yellow", file_out = log_file, verbose = verbose)
    edges$color[edges$color == 'nocolor'] = 'yellow'
  }
  if (any(edges$color == 'NOCOLOR')) {
    output_log("--Edge coloration manually 'nocolor'ed; those edges are in red.", file_out = log_file, verbose = verbose)
    edges$color[edges$color == 'NOCOLOR'] = 'red'
  }
  
  ## curved vs. straight edges
  allEdges = list()
  for(j in 1:nrow(edglist)) {
    allEdges[[j]] = edgeMaker(j, len = network_params$edge_plot_points, curved = is_curved[j],
                              layoutCoordinates = as.matrix(plotcord[,1:2]), adjacencyList = edglist)
  }
  allEdges <- do.call(rbind, allEdges)
  
  edg_color = rep(edges$color, each = network_params$edge_plot_points)
  edg_weight = rep(edges$weight, each = network_params$edge_plot_points)
  
  edge_plot = cbind(allEdges, color = edg_color, weight = edg_weight, stringsAsFactors = FALSE)
  
  return(list(edges, edge_plot))
}

