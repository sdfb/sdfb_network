#@S This file contains examples of using the network plotting code
#@L ---
#@L The general flow is as follows: 
#@L 1. Load appropriate libraries
#@L 2. Source functions needed (this also instantiates some global variables)
#@L 3. Load network data
#@L 4a. Generate plot settings using function <create_plot_params>
#@L 4b. Generate ggplot object using function <create_plot>
#@L 5. Plot ggplot (this uses the existing global variables, written to by create_plot)
#@L      The code segment at the end plots the ggplot object into either a pdf or png, as specified.

#############################
############################# Libraries
#############################

# These are needed for all plots
library(network)
library(ggplot2)
library(sna)
library(ergm)

# These two packages are for the curved-edges
library(Hmisc)
library(reshape2)

#############################
############################# Source function code
#############################

source("general_help_functions/textOutputFormatting.R")
source("network_visualization/plot_functions/main.R")

#############################
############################# Loading Data
#############################

# Load/set up appropriate lambda_matrix (and any other files needed)

load("network_visualization/conf_matrix.Rdata")
lambda_matrix = conf_matrix/100
test = read.csv("network_visualization/new.idnamecount.csv")
rownames(lambda_matrix) = test$Entity.Name
colnames(lambda_matrix) = test$Entity.Name

# this is the log_10 of the number of times the entities appear in the corpus
log_count = log(test[,4], base = 10)

#############################
############################# Plot settings
#############################

# output plot results to this file: 
LOG_FILE = "TEMP/zGEN_plotlog.txt"

# Run one of the params/gp pairs (or write one), to generate plotting information
# Note that this affects the set of global variables starting with "PD_"

params = create_plot_params(vis_type = "ego", 
                            params = list(target = "James Harrington",
                                          threshold = 0.5,
                                          node_text_size = 2,
                                          plot_size_settings = list(node = c(8,15),
                                                                    edge = c(1,2))))

params = create_plot_params( #indenting differently because this takes up too much space
  vis_type = "ego", 
  params = list(target = "Nicholas Breton",
                threshold = 0.5,
                node_text_size = log_count,
                plot_size_settings = list(node = c(8,15),
                                          edge = c(1,2)),
                
                node_setting_table = data.frame(type = c("CENTER", "OTHER"), 
                                                color = c("blue", "brown"),
                                                text_size = c(15,10),
                                                stringsAsFactors = FALSE))
)

temp = sort(lambda_matrix[which(colnames(lambda_matrix) == "Nicholas Breton"),], decreasing = TRUE)
t(t(head(temp, 75)))

params = create_plot_params( # testing dyad
  vis_type = "dyad",
  list_params = list(GLOBAL_PLOT_SETTINGS$OUTPUT_PDF),
  list(target = c("John Milton", "Andrew Marvell"),
       threshold = 0.5,
       edge_weight_display = TRUE
  )
)

params = create_plot_params( # testing trail_2
  vis_type = 'trail_2',
  params = list(threshold = 0.5,
                node_coord_options = list(within_scaledown = 0.3,
                                          target_dist = 4))
)

params = create_plot_params(
  vis_type = "ego", 
  list_params = list(GLOBAL_PLOT_SETTINGS$EGO_WT_DIST),
  params = list(target = "James Harrington",
                threshold = 0.4)
)

# testing trail_2 new

params = create_plot_params( # testing trail_2
  vis_type = 'trail_2',
  list_params = list(GLOBAL_PLOT_SETTINGS$DEFAULT_TRAIL_2),
  params = list(threshold = 0.7, target = c("Lucy Hutchinson", "Andrew Marvell"))
)

params = create_plot_params( # testing trail_2
  vis_type = 'trail_2',
  list_params =list(GLOBAL_PLOT_SETTINGS$GROUPWISE_TRAIL_2),
  params = list(threshold = 0.6, target = c("Lucy Hutchinson", "Andrew Marvell")
    )
  )
    
params = create_plot_params( # testing trail_2
  vis_type = 'trail_2',
  list_params =list(GLOBAL_PLOT_SETTINGS$LOCASSIGN_TRAIL_2, GLOBAL_PLOT_SETTINGS$OUTPUT_PDF),
  params = list(threshold = 0.45, target = c("Lucy Hutchinson", "Andrew Marvell"),
    node_setting_table = data.frame(
      type =      c("CENTER1", "CENTER2", "L1",    "L2",    "LBOTH"), 
      color =     c("blue",    "brown",   "black", "black", "black"),
      circ_rad =  c(0.1,       0.1,       1.1,       1.5,       0.3),   
      circ_cenx = c(-3,        3,         -2,      2,       0),
      circ_ceny = c(0,         0,         -2,      2,       0),
      stringsAsFactors = FALSE)
    )

  
  # TODO: properly document the locassign thing... it can 'obselete' some of the approximations
  # TODO: -- to this (the other trail_2 options?)
  # TODO: -- generalize/rename this... this is not necessarily a trail_2 thing. 
  )

params = create_plot_params( # testing trail_2
  vis_type = 'trail_2',
  list_params =list(GLOBAL_PLOT_SETTINGS$LOCASSIGN_TRAIL_2, GLOBAL_PLOT_SETTINGS$OUTPUT_PDF),
  params = list(threshold = 0.7, target = c("John Milton", "Isaac Newton"), ignore = c("King Charles", "Winston Churchill"),
    node_text_size = 'settings',
    node_setting_table = data.frame(
      type =      c("CENTER1", "CENTER2", "L1",    "L2",    "LBOTH"), 
      color =     c("blue",    "brown",   "black", "black", "black"),
      circ_rad =  c(0.1,       0.1,       1.8,       1.8,       0.3),   
      circ_cenx = c(-3,        3,         -2,      2,       0),
      circ_ceny = c(0,         0,         -2,      2,       0),
      text_size = c(10, 10, 9, 9, 9.5),
      stringsAsFactors = FALSE)
    )
  )

gp = create_plot(params, lambda_matrix, log_file = LOG_FILE, verbose = TRUE)

#############################
############################# Output specific plot
#############################

# Run this code to output the specific plot to file. 
if (params$output_format == "png") {
  png(paste(format(Sys.time(), "%a%b%d%H%M%S"), ".png"), width=PD_params$output_settings$width, 
      height=PD_params$output_settings$height, res=PD_params$output_settings$res)
  print(gp)
  dev.off()
} else if (params$output_format == "pdf") {
  pdf(paste(format(Sys.time(), "%a%b%d%H%M%S"), ".pdf"), width=PD_params$output_settings$width,
      height=PD_params$output_settings$height)
  print(gp)
  dev.off()
}


