#@S This file contains the information for network plotting functions
#@S   it points to individual files for some functions
#@L The general outline is: 
#@L  1. [create_plot_params] 
#@L     - Assign plotting parameters and place them in appropriate locations
#@L     ** This is in "create_plot_params.R"
#@L  2. [create_network_object]
#@L     - Create a network object based on input parameters
#@L     - Find appropriate subset of matrix, and output corresponding node information
#@L     ** This is in "create_network_object.R"
#@L  3. [create_nodes]
#@L     - Run algorithm for node positioning and node parameters
#@L     ** This is in "create_nodes.R"
#@L  4. [create_edges]
#@L     - Create edge plotting objects
#@L     ** This is in "create_edges.R"
#@L  5. [create_ggplot]
#@L     - Create ggplot2 object
#@L The function [create_plot] runs functions 2-5, and is stored in this file along 
#@L   with [create_ggplot]
#@L 
#@L See function documentation for specific details for each of these functions

# TODO: Need to think/implement plan to save old drawn network
# TODO: [Idea] A way to do dotted lines and such on edges?
# TODO: Dyad plot: want to enforce distance between central nodes. 
# TODO: allow weighting node locations by edge weight

# General Notes: 
## Usage of global varaible for plot data seems necessary, but unfortunate. 
## Why does ggplot require varaibles to be in global env, even if called within a function?

########### Needed global variables

PD_edges = NULL
PD_edge_plot = NULL
PD_plotcord = NULL
PD_params = NULL

############ Sourcing Functions
# Loading all of the functions called by functions in this file, and all necessary 
#   functions/variables needed for general usage 
#   (see example_plots.R in parent folder)

source("network_visualization/plot_functions/create_network_object.R")
source("network_visualization/plot_functions/create_nodes.R")
source("network_visualization/plot_functions/create_edges.R")
source("network_visualization/plot_functions/create_plot_params.R")
source("network_visualization/plot_functions/default_params.R")
source("network_visualization/plot_functions/helper_functions.R")
source("network_visualization/plot_functions/assign_node_locations.R")

############ Main Functions

create_plot = function(network_params, lambda_matrix, log_file, verbose) {
  # TODO: [Document] this function  
  # This is the overall call. This returns a single ggplot object, to be plotted as desired. 
  
  
  # Log intro
  output_log("--------------------------------------------", file_out = log_file, verbose = verbose)
  output_log("----------New network plot attempt----------", file_out = log_file, verbose = verbose)
  output_log(date(), file_out = log_file, verbose = verbose)
  output_log("--------------------------------------------", file_out = log_file, verbose = verbose)
  
  # Log network_params ONLY TO FILE
  output_list(L = network_params, file_out = log_file)
  output_log("--------------------------------------------", file_out = log_file, verbose = FALSE)
  
  # Run each of the separate sub-functions
  temp = create_network_object(network_params, lambda_matrix, log_file = log_file, verbose)
  node_data = temp$node_data
  subset_matrix = temp$subset_matrix
  net = temp$net
  adjm = temp$adjm
  
  temp = create_nodes(network_params, node_data, subset_matrix, net, adjm, log_file = log_file, verbose)
  plotcord = temp
  
  temp = create_edges(network_params, node_data, subset_matrix, net, adjm, plotcord, log_file = log_file, verbose)
  edges = temp[[1]]
  edge_plot = temp[[2]]
  
  final_plot = create_ggplot(network_params, node_data, subset_matrix, net, adjm, 
                             plotcord, edges, edge_plot, log_file = log_file, verbose)
  
  return(final_plot)
}


create_ggplot = function(network_params, node_data, subset_matrix, net, adjm, 
                         plotcord, edges, edge_plot, log_file, verbose) {
  # TODO: [Document] this function
  
  output_log("Generating ggplot object...", file_out = log_file, verbose = verbose)
  p = ggplot()
  
  # Set plot window
  PD_edges <<- edges
  p = p + geom_segment(aes(x=1.2*min(PD_edges$X1), y=1.2*min(PD_edges$Y1), 
                           xend = 1.2*max(PD_edges$X1), yend = 1.2*max(PD_edges$Y2),size=5), 
                       colour="white")
  
  # Draw edges first (so they are in background)
  PD_edge_plot <<- edge_plot
  p = p + geom_path(aes(x = x, y = y, group = Group), 
                    colour = PD_edge_plot$color, 
                    size = PD_edge_plot$weight, data=PD_edge_plot)
  # geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2,size=weight), data=edges, colour=edges$color) +
  
  # Draw nodes (as text)
  #   geom_point(aes(X1, X2,colour=elements,size=size), data=plotcord) + #puts the nodes where plotcord tell them to be and colors them based on plotcord$elements
  #   geom_point(aes(X1, X2,size=size), data=plotcord,color="black",fill="white",pch=21) + #same as above but color is white
  PD_plotcord <<- plotcord

  # TODO: Move code out from here eventually?
  # TODO: document NameFormat
  if (network_params$name_format == "split") {
    PD_plotcord$names = gsub(" ", "\n", PD_plotcord$names)
  }
  p = p + geom_text(aes(x=x,y=y,label=names,size=size),
                    colour=PD_plotcord$color,data=PD_plotcord)
  
  PD_params <<- network_params
  
  # Misc
  p = p + 
    scale_size(range=range(c(PD_params$plot_size_settings$node,
                             PD_params$plot_size_settings$edge))) +
    scale_colour_brewer(palette="Set1") +
    scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
    # discard default grid + titles in ggplot2 
    theme(panel.background = element_blank()) + 
    theme(legend.position="none")+
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    theme(legend.background = element_rect(colour = NA)) + 
    theme(panel.background = element_rect(fill = "white", colour = NA)) + 
    theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
  
  return(p)
}



