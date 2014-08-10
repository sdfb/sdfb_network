#@S Function to generate graphvis script file to input into graphvis for plotting.

generate_graphvis_script = function(conf_matrix, threshold, 
                                    out_file = "graph_data_gvis.txt") {
  #@F ----------------------------------------
  #@F Function 'generate_graphvis_script'
  #@F Args (Input):
  #@F   conf_matrix = confidence matrix, entries between 0,1?
  #@F   threshold = threshold to ignore all edges below this confidence
  #@F   out_file = file to store graph data 
  #@F Purpose: Generates a graphvis graph data script to run using its algorithms
  #@F Output: NONE (saves a file)
  #@F ----------------------------------------
  
  # note conf_matrix should have named columns/rows?, sparse format (ofc symmetric)
  num_nodes = nrow(conf_matrix) 
  
  # Initial graph header
  cat("graph testg {\n", 
      # "graph [overlap=prism]\n", # skipped this line
      "node [shape=point]\n",
      sep = "", file = out_file) # NO append = TRUE: THIS CLEARS THE out_file. 
    
  # Write out node data
  for(j in 1:num_nodes) {
    cat(j," [label=\"\", shape=circle", "]", "\n",
        sep = "", file = out_file, append = TRUE)
    if (j %% 500 == 0) {
      print(j)
    }
  }
  
  # Write out edge data
  for(j in 1:(num_nodes - 1)) {
    picked_edges = which(conf_matrix[j,] >= threshold)
    picked_edges = picked_edges[picked_edges > j]
    
    if (length(picked_edges) > 0) {
      for(k in picked_edges) {
        cat(j, " -- ", k, " [color=lightcyan1]", "\n",
            sep = "", file = out_file, append = TRUE)
      }
    }
    if (j %% 500 == 0) {
      print(j)
    }
  }
  cat("}\n", file = out_file, append = TRUE)
  
  return(NULL)
}
