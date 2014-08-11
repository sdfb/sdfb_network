#@S This file contains a list of default plotting settings (that are loaded globally)
#@L Existing plot settings are as follows: 

GLOBAL_PLOT_SETTINGS = list()

#@L $DEFAULT_EGO = default ego-plot settings (in png)
GLOBAL_PLOT_SETTINGS$DEFAULT_EGO = list(
  vis_type = 'ego',
  target = "James Harrington",
  ignore = "",
  topK = 40, 
  threshold = 0.7, 
  node_coord_type = "default",
  node_coord_options = NULL,
  node_setting_table = data.frame(type = c("CENTER", "OTHER"), 
                                  color = c("blue", "brown"), 
                                  stringsAsFactors = FALSE),
  node_text_size = 'same',
  edge_setting_table = data.frame(type1 = c("CENTER","CENTER","OTHER"),
                                  type2 = c("CENTER","OTHER", "OTHER"),
                                  color = c("black","grey20","beige"),
                                  curve = c(FALSE, FALSE, TRUE),
                                  stringsAsFactors = FALSE),
  output_format = "png",
  output_settings = list(height = 2000,
                         width = 2500,
                         res = 96),
  plot_size_settings = list(node = c(8,13),
                            edge = c(1,2)),
  centering = "^CENTER$",
  rotate_plot = FALSE,
  edge_weight_display = FALSE,
  edge_plot_points = 100,
  name_format = "split"
)


# TODO: When using create_plot_params, if there is a new 'setting', it will crash. Need alternate solution... Make it add the new setting as an additional entry in the list?


#@L $DEFAULT_DYAD = default dyad plot settings (in png)
GLOBAL_PLOT_SETTINGS$DEFAULT_DYAD = list(
  vis_type = 'dyad',
  target = c("John Milton", "Andrew Marvell"),
  ignore = "",
  topK = 15, 
  threshold = 0.7, 
  node_coord_type = "default",
  node_coord_options = NULL,
  node_setting_table = data.frame(type = c("CENTER1", "CENTER2", "L1", "L2", "LBOTH"), 
                                  color = c("blue", "brown", "black", "black", "black"), 
                                  Stringsasfactors = FALSE),
  node_text_size = 'same',
  edge_setting_table = data.frame(type1 = c("CENTER1", "CENTER1",   "CENTER1",   "CENTER2", "CENTER2", "L1",     "L1",     "L1",     "L2",     "L2",     "LBOTH"),
                                  type2 = c("CENTER2", "L1",        "LBOTH",     "L2",      "LBOTH",   "L1",     "L2",     "LBOTH",  "L2",     "LBOTH",  "LBOTH"),
                                  color = c("black",   "lightblue", "lightblue", "wheat1",  "wheat1",  "grey80", "grey80", "grey80", "grey80", "grey80", "grey80"),
                                  curve = c(FALSE,     FALSE,       FALSE,       FALSE,     FALSE,     TRUE,     TRUE,     TRUE,      TRUE,      TRUE,    TRUE),
                                  stringsAsFactors = FALSE),
  output_format = "png",
  output_settings = list(height = 2000,
                         width = 2500,
                         res = 96),
  plot_size_settings = list(node = c(8,13),
                            edge = c(1,2)),
  centering = "^CENTER",
  rotate_plot = TRUE,
  edge_weight_display = FALSE,
  edge_plot_points = 100,
  name_format = "split"
)


#@L $DEFAULT_TRAIL_2 = default dyad plot settings (in png)
GLOBAL_PLOT_SETTINGS$DEFAULT_TRAIL_2 = list(
  vis_type = 'trail_2',
  target = c("John Milton", "George Fox"),
  ignore = "",
  topK = 15, 
  threshold = 0.7, 
  node_coord_type = "default_trail_2",
  node_coord_options = list(within_scaledown = 0.3,
                            target_dist = 4),
  node_setting_table = data.frame(type = c("CENTER1", "CENTER2", "L1", "L2", "LBOTH"), 
                                  color = c("blue", "brown", "black", "black", "black"), 
                                  stringsAsFactors = FALSE),
  node_text_size = 'same',
  edge_setting_table = data.frame(type1 = c("CENTER1", "CENTER1",   "CENTER1",   "CENTER2", "CENTER2", "L1",     "L1",     "L1",     "L2",     "L2",     "LBOTH"),
                                  type2 = c("CENTER2", "L1",        "LBOTH",     "L2",      "LBOTH",   "L1",     "L2",     "LBOTH",  "L2",     "LBOTH",  "LBOTH"),
                                  color = c("black",   "lightblue", "lightblue", "wheat1",  "wheat1",  "grey80", "grey80", "grey80", "grey80", "grey80", "grey80"),
                                  curve = c(FALSE,     FALSE,       FALSE,       FALSE,     FALSE,     TRUE,     TRUE,     TRUE,      TRUE,      TRUE,    TRUE),
                                  stringsAsFactors = FALSE),
  output_format = "png",
  output_settings = list(height = 2000,
                         width = 2500,
                         res = 96),
  plot_size_settings = list(node = c(8,13),
                            edge = c(1,2)),
  centering = "^CENTER",
  rotate_plot = TRUE,
  edge_weight_display = FALSE,
  edge_plot_points = 100,
  name_format = "split"
)

#@L $GROUPWISE_TRAIL_2 = default dyad plot settings (in png)
GLOBAL_PLOT_SETTINGS$GROUPWISE_TRAIL_2 = list(
  vis_type = 'trail_2',
  target = c("John Milton", "George Fox"),
  ignore = "",
  topK = 15, 
  threshold = 0.7, 
  node_coord_type = "groupwise",
  node_coord_options = list(adj_funct = 
                              function(elen_val, modifier_val) {
                                return(modifier_val+(elen_val/10))
                              }
                            ),
  node_setting_table = data.frame(type = c("CENTER1", "CENTER2", "L1", "L2", "LBOTH"), 
                                  color = c("blue", "brown", "black", "black", "black"), 
                                  stringsAsFactors = FALSE),
  node_text_size = 'same',
  edge_setting_table = data.frame(type1 =    c("CENTER1",  "CENTER1",  "CENTER1",    "CENTER1",    "CENTER1",  "CENTER2",  "CENTER2",  "CENTER2",  "CENTER2",  "L1",      "L1",      "L1",      "L2",      "L2",      "LBOTH"),
                                  type2 =    c("CENTER1",  "CENTER2",  "L1",         "LBOTH",      "L2",       "CENTER2",  "L1",       "L2",       "LBOTH",    "L1",      "L2",      "LBOTH",   "L2",      "LBOTH",   "LBOTH"),
                                  color =    c("NOCOLOR",  "black",    "lightblue",  "lightblue",  "NOCOLOR",  "NOCOLOR",  "NOCOLOR",  "wheat1",   "wheat1",   "grey80",  "grey80",  "grey80",  "grey80",  "grey80",  "grey80"),
                                  curve =    c(FALSE,      FALSE,      FALSE,        FALSE,        FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      TRUE,      TRUE,      TRUE,      TRUE,      TRUE,      TRUE),
                                  modifier = c(0.1,        6,          2.3,          2,            3.6,        1,          3.6,        2.3,        2,          0.1,       4.5,       2.3,        0.1,      2.3,       0.1),
                                  stringsAsFactors = FALSE),
  output_format = "png",
  output_settings = list(height = 2000,
                         width = 2500,
                         res = 96),
  plot_size_settings = list(node = c(8,13),
                            edge = c(1,2)),
  centering = "^CENTER",
  rotate_plot = TRUE,
  edge_weight_display = FALSE,
  edge_plot_points = 100
)

#@L $LOCASSIGN_TRAIL_2
GLOBAL_PLOT_SETTINGS$LOCASSIGN_TRAIL_2 = list(
  vis_type = 'trail_2',
  target = c("John Milton", "George Fox"),
  ignore = "",
  topK = 15, 
  threshold = 0.7, 
  node_coord_type = "location_assign",
  node_coord_options = list(pG = 200, pE = 4, pC = 2, pM = 0.1),
  node_setting_table = data.frame(
    type =      c("CENTER1", "CENTER2", "L1",    "L2",    "LBOTH"), 
    color =     c("blue",    "brown",   "black", "black", "black"),
    circ_rad =  c(0.1,       0.1,       .8,       .8,       0.3),   
    circ_cenx = c(-3,        3,         -2,      2,       0),
    circ_ceny = c(0,         0,         -2,      2,       0),
    stringsAsFactors = FALSE),
  
  node_text_size = 'same',
  edge_setting_table = data.frame(
    type1 =    c("CENTER1",  "CENTER1",  "CENTER1",    "CENTER1",    "CENTER1",  "CENTER2",  "CENTER2",  "CENTER2",  "CENTER2",  "L1",      "L1",      "L1",      "L2",      "L2",      "LBOTH"),
    type2 =    c("CENTER1",  "CENTER2",  "L1",         "LBOTH",      "L2",       "CENTER2",  "L1",       "L2",       "LBOTH",    "L1",      "L2",      "LBOTH",   "L2",      "LBOTH",   "LBOTH"),
    color =    c("NOCOLOR",  "black",    "lightblue",  "lightblue",  "NOCOLOR",  "NOCOLOR",  "NOCOLOR",  "wheat1",   "wheat1",   "grey80",  "grey80",  "grey80",  "grey80",  "grey80",  "grey80"),
    curve =    c(FALSE,      FALSE,      FALSE,        FALSE,        FALSE,      FALSE,      FALSE,      FALSE,      FALSE,      TRUE,      TRUE,      TRUE,      TRUE,      TRUE,      TRUE),
    modifier = c(0.1,        6,          2.3,          2,            3.6,        1,          3.6,        2.3,        2,          0.1,       4.5,       2.3,        0.1,      2.3,       0.1),
    stringsAsFactors = FALSE),
  output_format = "png",
  output_settings = list(height = 2000,
                         width = 2500,
                         res = 96),
  plot_size_settings = list(node = c(8,13),
                            edge = c(1,2)),
  centering = "^CENTER",
  rotate_plot = TRUE,
  edge_weight_display = FALSE,
  edge_plot_points = 100,
  name_format = "split"
)

#@L $OUTPUT_PDF = changes sizes a bit for standard PDF output
GLOBAL_PLOT_SETTINGS$OUTPUT_PDF = list(
  output_format = "pdf",
  output_settings = list(height = 16,
                         width = 20),
  plot_size_settings = list(node = c(8,10),
                            edge = c(1,3))
)

#@L $OUTPUT_PNG = changes size settings for standard PNG output
GLOBAL_PLOT_SETTINGS$OUTPUT_PNG = list(
  output_format = "png",
  output_settings = list(height = 2000,
                         width = 2500,
                         res = 96),
  plot_size_settings = list(node = c(8,13),
                            edge = c(1,2))
)

# TODO: add settings for black and white plots for ego and dyad

#@L $EGO_WT_DIST = testing egocentric network with weighted edges affecting node placement
GLOBAL_PLOT_SETTINGS$EGO_WT_DIST = list(
  node_coord_type = "edge_wts",
  node_coord_options = list(
    types = "CENTER",
    adj_funct = function(dist, weights) {
      temp = (1.3 - weights^2) # new formula / attempt
      
      # TODO: Try to implement old functional code for weighted edge distances
      #     spr.mat = matrix(sub.matrix)
      #     zeros = (spr.mat[,1] == 0)
      #     temp = spr.mat[,1]
      #     temp[zeros] = 50
      #     temp[!zeros] = (abs(log(temp[!zeros]))^2)/3
      #     spr.mat = matrix(temp, nrow = dim(sub.matrix)[1])
      
      return(dist * temp)
    }
    )
)



