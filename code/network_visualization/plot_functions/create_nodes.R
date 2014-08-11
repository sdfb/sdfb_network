#@S Code for function create_nodes
#@L This function generates plotting information for nodes 
#@L   (generates location, coloring, etc)

create_nodes = function(network_params, node_data, subset_matrix, net, adjm, log_file, verbose) {
  #@F ----------------------------------------
  #@F Function 'create_nodes'
  #@F Args (Input):
  #@F   - network_params = output of 'create_plot_params' (a list)
  #@F   - lambda_matrix = confidence matrix, values between 0 to 1. Expect row/columns to be named. 
  #@F   - log_file = file to store additional text output (logging of the processing)
  #@F   - verbose = logical: determines amount of output
  #@F 
  #@F   - subset_matrix, net, adjm, node_data: All these are output of create_network_object
  #@F
  #@F Purpose: Generate node locations, coloration, sizes, for the purpose of plotting
  #@F Output: Single data.frame (often referred to as plotcord). Columns are: 
  #@F   - x = x-coordinate for plotting
  #@F   - y = y-coordinate for plotting
  #@F   - names = person names corresponding to nodes
  #@F   - type = node type (same type as in node_data)
  #@F   - size = assigned node size
  #@F   - color = assigned node color
  #@F ----------------------------------------
  
  output_log("Generating node plotting information...", file_out = log_file, verbose = verbose)


  ## Generate node locations (in plotcord)
  
  if(network_params$node_coord_type == "default") {    
    ## Use default method to generate node locations
    
    plotcord <- data.frame(gplot.layout.kamadakawai(adjm, NULL))
    
  } else if (network_params$node_coord_type == "default_trail_2") {
    ## Use old trail_2 code to generate node locations (nice looking graphics, but hard to read?)
    
    elen_default = generate_default_distmatrix(net) # obtains default elen matrix
    
    # Scale down distances within "clusters" : node_coord_option$within_scaledown
    p1nbr = c(#which(node_data$type == "CENTER1"), 
      which(node_data$type == "L1"),
      which(node_data$type == "LBOTH") )
    p2nbr = c(#which(node_data$type == "CENTER2"), 
      which(node_data$type == "L2"),
      which(node_data$type == "LBOTH") )
    elen = elen_default
    elen[p1nbr,p1nbr] = elen[p1nbr,p1nbr] * network_params$node_coord_options$within_scaledown
    elen[p2nbr,p2nbr] = elen[p2nbr,p2nbr] * network_params$node_coord_options$within_scaledown
    
    # Set distance between target nodes : node_coord_option$target_dist
    elen[1,2] = network_params$node_coord_options$target_dist
    elen[2,1] = network_params$node_coord_options$target_dist
    
    plotcord <- data.frame(gplot.layout.kamadakawai(adjm, list(elen = elen))) 
    
  } else if (network_params$node_coord_type == "groupwise") {
    ## Use distance matrix with entries adjusted by 'group' membership
    
    elen_default = generate_default_distmatrix(net) # obtains default elen matrix
    elen = elen_default
    
    all_combs = expand.grid(1:nrow(node_data), 1:nrow(node_data))
    modifier = rep(NA, times = nrow(all_combs))


    type1s = node_data$type[all_combs[,1]]
    type2s = node_data$type[all_combs[,2]]
    tab = network_params$edge_setting_table
    
    for (j in 1:nrow(tab)) {
      matches = match_two_columns(type1s, type2s, target1 = tab$type1[j], target2 = tab$type2[j])
      modifier[matches] = tab$modifier[j]
    }

    print(cbind(all_combs, modifier))
    
    modifier_matrix = matrix(modifier, nrow = nrow(node_data))
    
    for(d1 in 1:nrow(elen)) {
      for(d2 in 1:nrow(elen)) {
        elen[d1,d2] = network_params$node_coord_options$adj_funct(elen[d1,d2], modifier_matrix[d1,d2])
      }
    }

    print(elen)
    
    plotcord <- data.frame(gplot.layout.kamadakawai(adjm, list(elen = elen))) 
    
  } else if (network_params$node_coord_type == "edge_wts") {
    ## Use edge weights to determine node locations
    
    elen_default = generate_default_distmatrix(net) # obtains default elen matrix
    elen = elen_default
    
    index_to_edit = which(!is.na(match(node_data$type, network_params$node_coord_options$types)))
    if (length(index_to_edit) < 1) {
      stop("network_params$node_coord_options$types has invalid values.")
    }
    
    for(j in 1:length(index_to_edit)) {
      ind = index_to_edit[j]

      elen[ind,] = network_params$node_coord_options$adj_funct(
        dist = elen_default[ind,], weights = subset_matrix[ind,])
      elen[,ind] = network_params$node_coord_options$adj_funct(
        dist = elen_default[ind,], weights = subset_matrix[ind,])
    }

    plotcord <- data.frame(gplot.layout.kamadakawai(adjm, list(elen = elen))) 

  } else if (network_params$node_coord_type == "location_assign") {
    # use my method to optimize locations
    edglist = as.matrix.network.edgelist(net)

    types = unique(node_data$type)
    pG = network_params$node_coord_options$pG
    pE = network_params$node_coord_options$pE
    pC = network_params$node_coord_options$pC
    pM = network_params$node_coord_options$pM
    tabid = match(types, network_params$node_setting_table$type)
    
    plotcord =
      assign_locations(
        node_groups = match(node_data$type, types),
        group_rads = network_params$node_setting_table$circ_rad[tabid],
        group_centers = cbind(
          network_params$node_setting_table$circ_cenx[tabid],
          network_params$node_setting_table$circ_ceny[tabid]),
        edge_list = edglist,
        pG = pG, pE = pE, pC = pC, pM = pM)
    
  } else {
    stop("Improper choice of network_params$node_coord_type")
  
  }
  
  colnames(plotcord) = c("x","y")
  plotcord$names = node_data$names
  plotcord$type = node_data$type
  
  # Center coordinates
  if (network_params$centering == "viewwindow") {
    plotcord[,1:2] = center_matrix(plotcord[,1:2], center = center_of_points(plotcord[,1:2]))
  } else {
    central_ind = grep(network_params$centering, node_data$type)
    if (length(central_ind) > 0) {
      plotcord[,1:2] = center_matrix(plotcord[,1:2],
                                     center = apply(plotcord[central_ind,1:2], 2, mean))
    } else {
      output_log(paste("--WARNING: No nodes of type ",network_params$centering," found, so centering view-window (default).", sep = ""), file_out = log_file, verbose = verbose)
      
      # viewwindow case
      plotcord[,1:2] = center_matrix(plotcord[,1:2], center = center_of_points(plotcord[,1:2]))
    }
  }
  
  # Rotate coordinates
  if (network_params$rotate_plot) {
    rot_degree = atan( (plotcord[1,2] - plotcord[2,2]) / (plotcord[1,1] - plotcord[2,1]) )
    plotcord[,1:2] = rotate_matrix(plotcord[,1:2], rotate = rot_degree)
  }
  
  # Generate node sizes
  l = min(network_params$plot_size_settings$node)
  u = max(network_params$plot_size_settings$node)
  if (is.character(network_params$node_text_size)) {
    if (network_params$node_text_size == 'same') {
      plotcord$size = u
    } else if (network_params$node_text_size == 'settings') {
      tab = network_params$node_setting_table
      plotcord$size = tab$text_size[match(node_data$type,tab$type)]  
    }
  } else {
    textsizes = network_params$node_text_size[node_data$ID]
    rescale_01 = textsizes / max(textsizes)
    plotcord$size = (u - l) * rescale_01 + l
  }
  
  # Generate node colors
  ## Match point type with color table: 
  tab = network_params$node_setting_table
  plotcord$color = tab$color[match(node_data$type,tab$type)]  
  
  ## If there is an error with node color assignment:
  if (any(is.na(plotcord$color))) {
    plotcord$color[is.na(plotcord$color)] = 'yellow'
    output_log("--Node coloration not complete: unnoted nodes are in yellow", file_out = log_file, verbose = verbose)
  }
  
  return(plotcord)
}

