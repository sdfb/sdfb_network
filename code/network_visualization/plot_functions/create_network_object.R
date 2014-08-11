#@S Code for function create_network_object
#@L This function processes the incoming data, and extracts the pertinent sub-network

create_network_object = function(network_params, lambda_matrix, log_file, verbose) {
  #@F ----------------------------------------
  #@F Function 'create_network_object'
  #@F Args (Input):
  #@F   - network_params = output of 'create_plot_params' (a list)
  #@F   - lambda_matrix = confidence matrix, values between 0 to 1. Expect row/columns to be named. 
  #@F   - log_file = file to store additional text output (logging of the processing)
  #@F   - verbose = logical: determines amount of output
  #@F Purpose: Extract a subset of full network (adjacency confidence matrix) for further processing
  #@F Output: A named list, with the following components: 
  #@F   - node_data = data.frame containing the following columns: 
  #@F     - ID == row/column number of node in base confidence_matrix
  #@F     - type == specific node classification; options depend on plot type
  #@F     - names == name of person corresponding with node
  #@F
  #@F   - subset_matrix = corresponding subset of lambda_matrix. rows/cols correspond to 
  #@F       rows in 'node_data'
  #@F
  #@F   - net = Object of class 'network' from library(network). 
  #@F
  #@F   - adjm = 1/0-filled adjacency matrix (basically 1/0 version of subset_matrix)
  #@F     
  #@F Notes: See documentation of create_plot_params to see what kind of plots are supported. 
  #@F ----------------------------------------

  node_names = colnames(lambda_matrix)
  
  ## Generate nodes to be in network
  if (network_params$vis_type == "ego") {
    output_log(paste("Generating a ego-centric network around '", network_params$target, "'", sep = ""), file_out = log_file, verbose = verbose)
    
    center_nodenum = which(node_names == network_params$target)
    if (length(center_nodenum) == 0) {
      stop(paste("Target name(s) not found (in network_params$target): ",
                 network_params$target, sep = ""))
    }
    other_nodenum = topk(lambda_matrix[,center_nodenum], k = network_params$topK,
                         thres = network_params$thres)
    
    output_log(paste("**Number of other nodes = ", length(other_nodenum), sep = ""), file_out = log_file, verbose = verbose)
    if (length(other_nodenum) < network_params$topK) {
      output_log(paste("--NOTE: Due to threshold choice, fewer than",network_params$topK,"neighboring nodes have been added."), file_out = log_file, verbose = verbose)
    }
    
    node_ids = c(center_nodenum, other_nodenum)
    node_code = c("CENTER", rep("OTHER", times = length(other_nodenum)))
    
  } else if (network_params$vis_type == "dyad") {
    
    output_log(paste("Generating a dyad network around '", network_params$target[1], 
                     "' and '", network_params$target[2], "'", sep = ""), file_out = log_file, verbose = verbose)
    
    n1 = which(node_names == network_params$target[1])
    n2 = which(node_names == network_params$target[2])
    if (length(n1) == 0) {
      stop(paste("Target name(s) not found (in network_params$target): ",
                 network_params$target[1], sep = ""))
    }
    if (length(n2) == 0) {
      stop(paste("Target name(s) not found (in network_params$target): ",
                 network_params$target[2], sep = ""))
    }
    l1 = topk(lambda_matrix[,n1], k = network_params$topK,
              thres = network_params$thres)
    l2 = topk(lambda_matrix[,n2], k = network_params$topK,
              thres = network_params$thres)
    
    output_log(paste("**Number of neighbors of '",network_params$target[1],"' = ", length(l1), sep = ""), file_out = log_file, verbose = verbose)
    output_log(paste("**Number of neighbors of '",network_params$target[2],"' = ", length(l2), sep = ""), file_out = log_file, verbose = verbose)
    output_log(paste("**Number of non-target nodes = ", length(union(l1,l2)), sep = ""), file_out = log_file, verbose = verbose)
    
    l1 = setdiff(l1, n2)
    l2 = setdiff(l2, n1)
    lboth = intersect(union(l1, l2), 
                      intersect(which(lambda_matrix[,n1] >= network_params$thres),
                                which(lambda_matrix[,n2] >= network_params$thres) ))
    l1 = setdiff(l1, lboth)
    l2 = setdiff(l2, lboth)
    
    node_ids = c(n1, n2, l1, l2, lboth)
    node_code = c("CENTER1", "CENTER2", rep("L1", times = length(l1)),
                  rep("L2", times = length(l2)), rep("LBOTH", times = length(lboth)))
    
  } else if (network_params$vis_type == "trail_2") {
    
    output_log(paste("Generating a network path (3-away) from '", network_params$target[1], 
                     "' to '", network_params$target[2], "'", sep = ""), file_out = log_file, verbose = verbose)
    
    n1 = which(node_names == network_params$target[1])
    n2 = which(node_names == network_params$target[2])
    if (length(n1) == 0) {
      stop(paste("Target name(s) not found (in network_params$target): ",
                 network_params$target[1], sep = ""))
    }
    if (length(n2) == 0) {
      stop(paste("Target name(s) not found (in network_params$target): ",
                 network_params$target[2], sep = ""))
    }
    
    l1 = which(lambda_matrix[,n1] >= network_params$threshold) #edges1
    l2 = which(lambda_matrix[,n2] >= network_params$threshold) #edges2
    
    # drop primary node links
    l1 = setdiff(l1, n2)
    l2 = setdiff(l2, n1)
    
    lboth = intersect(l1,l2)
    
    if (length(l1) == 0 | length(l2) == 0) {
      stop(paste("Threshold is too high too allow connections to one of targets"))
    }
    
    l1_good = NULL
    l2_good = NULL
    
    for(j in 1:length(l1)) {
      l2_matches = intersect(which(lambda_matrix[,l1[j]] >= network_params$threshold), l2)
      if (length(l2_matches) > 0) {
        l1_good = c(l1_good, l1[j])
        l2_good = c(l2_good, l2_matches)
      }
    }
    
    output_log(paste("**Number of neighbors of '",network_params$target[1],"' = ", length(l1_good), sep = ""), file_out = log_file, verbose = verbose)
    output_log(paste("**Number of neighbors of '",network_params$target[2],"' = ", length(l2_good), sep = ""), file_out = log_file, verbose = verbose)
    output_log(paste("**Number of non-target nodes = ", length(union(l1_good,l2_good)), sep = ""), file_out = log_file, verbose = verbose)
    
    l1_good = setdiff(l1_good, lboth)
    l2_good = setdiff(l2_good, lboth)
    
    node_ids = c(n1, n2, l1_good, l2_good, lboth)
    node_code = c("CENTER1", "CENTER2", rep("L1", times = length(l1_good)),
                  rep("L2", times = length(l2_good)), rep("LBOTH", times = length(lboth)))
    
  }
  
  node_data = data.frame(ID = node_ids, type = node_code, names = node_names[node_ids],
                         stringsAsFactors = FALSE)

  ## TODO: Document 'ignore'
  if (!is.null(network_params$ignore)) {
    to_iggy = match(network_params$ignore, node_data$names)
    to_iggy = to_iggy[!is.na(to_iggy)]
    if (length(to_iggy) > 0) {
      node_data = node_data[-to_iggy,]
    }
  }


  subset_matrix = lambda_matrix[node_data$ID, node_data$ID]
  adjacency_matrix = matrix(as.numeric(subset_matrix) >= network_params$threshold, nrow = nrow(node_data))
  subset_matrix = adjacency_matrix * subset_matrix
  
  net = as.network(x = adjacency_matrix, matrix.type = "adjacency", directed = FALSE)
  set.vertex.attribute(net, "names", node_data$names)
  
  adjm <- as.matrix.network.adjacency(net)
  
  return(list(node_data = node_data, subset_matrix = subset_matrix, net = net, adjm = adjm))
}
