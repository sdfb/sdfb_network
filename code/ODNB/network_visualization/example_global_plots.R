#@S This file contains examples of global plots
#@S   - Requires: graphviz toolkit (www.graphviz.org)
#@L ---
#@L The general flow is as follows: 
#@L 1. Create graphviz graph-data file, run it through graphviz to obtain plot locations
#@L      - cannot use same package as for smaller plots; runtime is unbearable
#@L      - graphviz can process ~ 6000 nodes and create locations in a few minutes
#@L 2. Load results into R
#@L 3. Generate plot settings
#@L 4. Plot ggplot
#@L 
#@L Note that step 1 may only need to be done once. 

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

source("network_visualization/global_plots/generate_graphvis_script.R")
source("network_visualization/global_plots/g_create_ggplot.R")

#source("network_visualization/plot_functions/main.R")

#############################
############################# Example:
#############################

## Step 1a: Load Data

test = read.csv("network_visualization/new.idnamecount.csv")
node_names = test$Entity.Name
log_count = log(test[,4], base = 10)

load("network_visualization/conf_matrix.Rdata")
cm = conf_matrix / 100

## Step 1b: Generate graphviz graph-data file

# generate_graphvis_script(conf_matrix = cm, threshold = 0.5, 
#                          out_file = "conf_50_biggraph.txt")

## Step 1c: This is run in graphvis: 

# Graphvis command: (On windows; assuming all files in the same directory as executable)
# Print to PNG: (just to see what happens)
#   sdfp -Tpng -oTEST.PNG conf_50_biggraph.txt
#   (i.e. sdfp -T[type] -o[out_file] [in_file])
# Print to text (to process back and plot using R)
#   sdfp -Tplain -obiggraph_locs_c50.txt conf_50_biggraph.txt

## Step 2: 
# By now, should have 'biggraph_locs_c50.txt' (or similar)
# A copy is stored in data/global_plot. 
g_create_ggplot(node_names = node_names, node_sizes = log_count, 
                graphviz_outfile = "data/global_plot/biggraph_locs_c50.txt",
                color_function = generate_birthbased_colors)


## Temporary solution -> plot results

# TODO: Update the following code... 
# TODO: Link this global plotting stuff with general plotting? 

#################################
png(paste(format(Sys.time(), "%a%b%d%H%M%S"), ".png"), width=3000, height=2000, res=96)

ggplot() +
  #  geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2,size=weight), data=edges, colour=e1) +
  geom_point(aes(x,y,size=size), data=G_PLOTCORD,
             color="black", fill=G_PLOTCORD$color,pch=21) + 
  #same as above but color is white
  scale_size(range=range(c(1, G_SETTINGS$node_size_range))) + # edge.weight.range
  scale_colour_brewer(palette="Set1") +
  #  geom_text(aes(x=X1,y=X2,label=names),size = 15, colour="red",data=n1) +
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
  # discard default grid + titles in ggplot2 
  theme(panel.background = element_blank()) + 
  theme(legend.position="none")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme( legend.background = element_rect(colour = NA)) + 
  theme(panel.background = element_rect(fill = "white", colour = NA)) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(panel.background = element_rect(fill='grey59', colour='grey59'))

dev.off()

