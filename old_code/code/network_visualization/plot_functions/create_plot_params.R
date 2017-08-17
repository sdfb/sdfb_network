#@S This file contains the function to generate the 'network_params' named list and 
#@S   the corresponding documentation of the entries
#@L ----- ----- ----- -----
#@L Usage of create_plot_params: 
#@L 
#@L   - "vis_type" should be one of 'ego', 'dyad', or 'trail_2'. 
#@L       It is an element of the 'network_params' list, but is needs to be 
#@L       passed in to the function separately to determine the default list of settings
#@L 
#@L   - "list_params" A list (colllection) of lists of form 'params' to override 
#@L       the default setting. Later lists in this collection take higher 
#@L       priority (i.e. the earlier lists get their changes applied first, 
#@L       but could be overridden by later lists). Any sub-list(s) of GLOBAL_PLOT_SETTINGS
#@L       in default_params.R could be used here. 
#@L 
#@L   - "params" A named list with the following elements: 
#@L       Elements are listed next, in the format [NAME] : [TYPE] \n [DESCRIPTION]
#@L 
#@L ----- ----- ----- -----
#@L vis_type : character singleton
#@L   Has to be one of 'ego', 'dyad', or 'trail_2'
#@L     'ego' = plots 'topK' most confident connections >= 'threshold'
#@L     'dyad' = plots 'topK' most confident connections around both peresons >= 'threshold'
#@L     'trail_2' = plots all found paths of length 2 between 'target[1]' and 'target[2]'
#@L       (i.e. shows target1 - p1 - p2 - target2) Only uses edges with confidence >= 'threshold'
#@L 
#@L target : character vector
#@L   Length depends on vis_type: 
#@L   'ego' = (Length 1) : is name of central node
#@L   'dyad', 'trail_2' = (Length 2) : Is names of two target actors (persons)
#@L   
#@L topK : numeric [integer] singleton
#@L   For 'ego', 'dyad', this is the max number of links found around target(s)
#@L   
#@L threshold : numeric [0 <= x <= 1] singleton  
#@L   Only edges with confidence greater than 'threshold' will be shown in plots
#@L   
#@L node_coord_type : character singleton
#@L   This specifies the method to generate node coordinates
#@L   Choices are "default", "default_trail_2", "edge_wts", "groupwise"
#@L   See explanations in node_coord_options
#@L
#@L node_coord_options : list
#@L   The entries of this list depend on the choice of node_coord_type. 
#@L   -> "default" => This uses default kamadakawai layout. no list entries. 
#@L
#@L   -> "default_trail_2" => This uses old trail_2 method; this leads to often visually 
#@L      nicer plots (since groups are highly-clustered), but hard to see edges. 
#@L      Named-list entries are: 
#@L   -> -> within_scaledown = multiplier for scaling down distance within target1/nbr, target2/nbr
#@L   -> -> target_dist = fixed distance between central edges
#@L      These options modify the current distance matrix, which is geodesic distance
#@L   
#@L   -> "edge_wts" => This tries to, as opposed to default kamadakawai, uses edge weights
#@L      to influence the default distance metric. The influence is adjusted as follows: 
#@L      Named-list entries are: 
#@L   -> -> types = character vector; of things like "CENTER", "L1", etc. Only edges to
#@L           one of these types will have its weights adjusted by adj_funct
#@L   -> -> adj_funct = function, two inputs: dist = distance matrix value,
#@L           weights = confidence value; output = replacement dist value. 
#@L 
#@L   -> "groupwise" => This scales up/down the default distances within/between groups. 
#@L   -> -> adj_funct = function, two inputs: elen_entry (dist matrix entry), and modifier
#@L           modifier is a value in edge_setting_table, which is possibly different
#@L           for different 'types'. Output of adj_funct is a replacement value for
#@L           elen_entry (some numeric value)
#@L   -> -> NOT IN THIS LIST; edge_setting_table data frame can have an extra column
#@L   -> -> called 'modifier' that would be input into adj_funct. 
#@L   
#@L node_text_size : numeric vector or character singleton
#@L   IF numeric vector, then needs to be vector of relative size of output text (length 6289)
#@L   IF character singeton: 
#@L     value is 'same' => all nodes same size
#@L     value is 'settings' => different size for each node type (see node_setting_table)
#@L
#@L
#@L ----- ----- ----- -----
#@L See function documentation for further details on the code. 


# TODO: [Document] Finish documenting parameter list options

## node_setting_table: data frame: columns: 'type', 'color'. colors nodes accordingly. 
##    eg of 'type' is 'CENTER' or 'OTHER'
##    column 'text_size' IF node_text_size = 'settings'
## edge_setting_table: data frame: type1, type2 are types as in node_setting table
##    color = color of edges of certain type
##    curve = TRUE if want curved, FALSE otherwise. 
###   modifier = optional; used if node_coord_type is groupwise_multiplier; this has the muiltipliers
## edge_plot_points: number of points used to plot an edge. used to be 200
## edge_weight_display: TRUE if want to use confidences to denote edge widths
## output_format: pdf or png
## output_settings: list: png => height, width, res; pdf => height, width
## plot_size_settings: list of two settings: node, edge
## centering: "viewwindow" is default. Otherwise, would be a regex for type, and it centers plot  
##    at the centroid of all the nodes with the specified type(s) [by regex]. ie never call a type 'viewwindow'
## rotate_plot: FALSE -> no rotate
##              TRUE -> rotate using first two rows of plotcord

## document: name_format


create_plot_params = function(vis_type = c('ego', 'dyad', 'trail_2'), 
                              params = NULL, list_params = NULL) {
  # TODO: [Document] this function
  
  # finish adding all params. then add default cases for any unwritten parameters. 
  # also add option to use an existing non-dfault paramter list and edit that?
  # params should be a named list. every item in the list will be checked if it is null: 
  #  if not, the corresponding paramater will be adjusted. 
  
  # load type-default parameters
  if (vis_type == 'ego') {
    toreturn = GLOBAL_PLOT_SETTINGS$DEFAULT_EGO
  } else if (vis_type == 'dyad') {
    toreturn = GLOBAL_PLOT_SETTINGS$DEFAULT_DYAD
  } else if (vis_type == 'trail_2') {
    toreturn = GLOBAL_PLOT_SETTINGS$DEFAULT_TRAIL_2
  } else {
    stop("Non-existing vis_type")
  }
  
  # Apply list_params parameters first, in order
  if (length(list_params) > 0) {
    for (list_i in 1:length(list_params)) {
      current_list = list_params[[list_i]]
      
      # apply changes from current list
      for (i in 1:length(current_list)) {
        ind = which(names(current_list)[i] == names(toreturn))
        toreturn[[ind]] = current_list[[i]]
      }
      
    }
  }
  
  # Apply any other changes in params
  if (length(params) == 0) {
    # do nothing: no changes
  } else {
    for (i in 1:length(params)) {
      ind = which(names(params)[i] == names(toreturn))
      toreturn[[ind]] = params[[i]]
    }
  }
  return(toreturn)
}
