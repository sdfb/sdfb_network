#@S Code for generating better-quality birthdate-colored graphs

# TODO: Finish cleaning this file?
# TODO: black/white version of this plot

#libraries: 

# These are needed for all plots
library(network)
library(ggplot2)
library(sna)
library(ergm)

# For nice coloration?
library(grid)
library(scales) # for muted

# Generate plot coordinates into G_PLOTCORD: 

source("network_visualization/global_plots/g_create_ggplot.R")

test = read.csv("network_visualization/new.idnamecount.csv")
node_names = test$Entity.Name
log_count = log(test[,4], base = 10)

g_create_ggplot(node_names = node_names, node_sizes = log_count, 
                graphviz_outfile = "data/global_plot/biggraph_locs_c50.txt",
                color_function = generate_birthbased_colors)

# Rotate data
source("network_visualization/plot_functions/helper_functions.R")
xy = cbind(G_PLOTCORD$x, G_PLOTCORD$y)
xy = center_matrix(xy)
xy = scale_matrix(xy)
xy = rotate_matrix(xy, rotate = 0.6)

G_PLOTCORD$x = xy[,1]
G_PLOTCORD$y = xy[,2]

# scale-down size
G_PLOTCORD$size = G_PLOTCORD$size * 0.9
# Extract specific monarchs

monarch_inds = rev(c(968, 3557, 1027, 2069, 487, 488, 2071))
sub_plotcord = G_PLOTCORD[monarch_inds,c(1,2,4)] #x,y,names

sub_plotcord$year = rev(c(1547, 1553, 1558, 1604, 1625, 1660, 1685))
sub_plotcord$text = paste(sub_plotcord$names," (",sub_plotcord$year,")", sep = "")

### Remove nodes that are too far out to make a difference: 
torm = which(G_PLOTCORD$x^2 + G_PLOTCORD$y^2 > 9)
G_PLOTCORD = G_PLOTCORD[-torm,]



##################################################################

#place names on side, and draw line from point to name
# stick at x = 13
# y = 11, 10.5, 10, 9.5, 9, 8.5, 8
extra_lines = data.frame(x1 = sub_plotcord$x, y1 = sub_plotcord$y, x2 = 2.1, y2 = seq(from = 3.5, to = 1.4, by = -.35))
sub_plotcord$xt = extra_lines$x2
sub_plotcord$yt = extra_lines$y2


# red/green
#scale_fill_gradient("Approximate Year of Birth", low = "#5EFF87",
#                    high = "#FF5E5E", limits = c(1450,1750), na.value = "#FF5E5E",
#                    space = "rgb") 
# 


# Large png: needs fixing? 
# #################################
# png(paste(format(Sys.time(), "%a%b%d%H%M%S"), ".png"), width=5000, height=4000, res=96)
# 
# ggplot() +
#   geom_point(aes(x, y, fill = year, size = size), data=G_PLOTCORD, color="black",pch=21) + #same as above but color is white
#   scale_fill_gradient("Approximate Year of Birth", low = "#2B8CBE",
#                       high = "#FF5E5E", limits = c(1450,1750), na.value = "#FF5E5E",
#                       space = "rgb") +
#   scale_size("Log(Total Mentions) [base 10]",range=range(c(edge.weight.range,name.size.range))) +
#   scale_colour_brewer(palette="Set1") +
#   
#   scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
#   geom_segment(aes(x=x1, y=y1, xend = x2, yend = y2), data = extra_lines, colour = "yellow",
#                size = 3) + 
#   geom_text(aes(x=xt,y=yt,label=text),size = 35, colour="yellow",data=sub_plotcord, face = "bold", hjust = 0) +
#   # discard default grid + titles in ggplot2 
#   theme(legend.key.size = unit(150 ,"points")) +
#   theme(legend.text = element_text(size = 50)) +
#   theme(legend.title = element_text(size = 70)) +
#   theme(legend.position = 'bottom') +
#   theme(panel.background = element_blank()) + #theme(legend.position="none")+
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
#   #theme( legend.background = element_rect(colour = NA)) + 
#   #theme(panel.background = element_rect(fill = "white", colour = NA)) + 
#   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
#   theme(panel.background = element_rect(fill='black', colour='black'))
# 
# 
# dev.off()
# 
# 
# ################################# # SMALLER PNG
# png(paste(format(Sys.time(), "%a%b%d%H%M%S"), ".png"), width=1200, height=1000, res=96)
# edge.weight.range <- c(0.5,1) ##Controls amount of edge ink
# name.size.range <- c(1,4) ##Controls amount of text ink
# 
# ggplot() +
#   #  geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2,size=weight), data=edges, colour=e1) +
#   geom_point(aes(x, y, fill = year, size = size), data=G_PLOTCORD, color="black",pch=21) + #same as above but color is white
#   scale_fill_gradient("Approximate Year of Birth", low = "#2B8CBE",
#                       high = "#FF5E5E", limits = c(1450,1750), na.value = "#FF5E5E",
#                       space = "rgb") +
#   scale_size("Log(Total Mentions) [base 10]",range=range(name.size.range)) +
#   scale_colour_brewer(palette="Set1") +
#   
#   scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
#   geom_segment(aes(x=x1, y=y1, xend = x2, yend = y2), data = extra_lines, colour = "yellow", size = 1) + 
#   geom_text(aes(x=xt,y=yt,label=text),size = 7, colour="yellow",data=sub_plotcord, face = "bold", hjust = 0) +
#   geom_text(aes(x=min(G_PLOTCORD$x) + 0.1,y=max(G_PLOTCORD$y) - 0.2,label="sixdegreesoffrancisbacon.com"),size = 9, colour="beige", hjust = 0) +
#   geom_text(aes(x=min(G_PLOTCORD$x) + 0.1,y=max(G_PLOTCORD$y) - 0.7,label="Image by Lawrence Wang"), size = 5, colour="beige", hjust = 0) +
#   
#   
#   theme(legend.key.size = unit(45 ,"points"), legend.key.width = unit(70, "points")) +
#   theme(legend.text = element_text(size = 16)) +
#   theme(legend.title = element_text(size = 20)) +
#   theme(legend.position = 'bottom') +
#   theme(panel.background = element_blank()) + #theme(legend.position="none")+
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
#   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
#   theme(panel.background = element_rect(fill='black', colour='black'))
# 
# dev.off()
# 
# #a
# 
# 
# 
# ################################# as pdf, for AoAS paper
# pdf(paste(format(Sys.time(), "%a%b%d%H%M%S"), ".pdf"), width=20, height=16)
# 
# 
# edge.weight.range <- c(0.5,1) ##Controls amount of edge ink
# name.size.range <- c(3,9) ##Controls amount of text ink
# 
# ggplot() +
#   #  geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2,size=weight), data=edges, colour=e1) +
#   geom_point(aes(x, y, fill = year, size = size), data=G_PLOTCORD, color="black",pch=21) + #same as above but color is white
#   scale_fill_gradient("Approximate Year of Birth", low = "#2B8CBE",
#                       high = "#FF5E5E", limits = c(1450,1750), na.value = "#FF5E5E",
#                       space = "rgb") +
#   scale_size("Log(Total Mentions) [base 10]",range=range(name.size.range)) +
#   scale_colour_brewer(palette="Set1") +
#   
#   scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
#   geom_segment(aes(x=x1, y=y1, xend = x2, yend = y2), data = extra_lines, colour = "yellow",
#                size = 2) + 
#   geom_text(aes(x=xt,y=yt,label=text),size = 10, colour="yellow",data=sub_plotcord, face = "bold", hjust = 0) +
#   # discard default grid + titles in ggplot2 
#   theme(legend.key.size = unit(80 ,"points")) +
#   theme(legend.text = element_text(size = 20)) +
#   theme(legend.title = element_text(size = 30)) +
#   theme(legend.position = 'bottom') +
#   theme(panel.background = element_blank()) + #theme(legend.position="none")+
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
#   #theme( legend.background = element_rect(colour = NA)) + 
#   #theme(panel.background = element_rect(fill = "white", colour = NA)) + 
#   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
#   theme(panel.background = element_rect(fill='black', colour='black'))
# 
# 
# dev.off()


# Attempt to make black and white plot

pdf(paste(format(Sys.time(), "%a%b%d%H%M%S"), ".pdf"), width=20, height=20)

edge.weight.range <- c(0.5,1) ##Controls amount of edge ink
name.size.range <- c(3,9) ##Controls amount of text ink

ggplot() + 
  #  geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2,size=weight), data=edges, colour=e1) +
  geom_point(aes(x, y, fill = year, size = size), data=G_PLOTCORD, color="black",pch=21) + #same as above but color is white
  scale_fill_gradient("Approximate Year of Birth", low = "#000000",
                      high = "#FFFFFF", limits = c(1450,1750), na.value = "#AAAAAA",
                      space = "rgb") +
  scale_size("Total Mentions in ODNB [log_10]",range=range(name.size.range)) +
  scale_colour_brewer(palette="Set1") +
  
  scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) +
  geom_segment(aes(x=x1, y=y1, xend = x2, yend = y2), data = extra_lines, colour = "#FFFFFF",
               size = 1.3) + 
  geom_text(aes(x=xt,y=yt,label=text),size = 10, colour="#FFFFFF",data=sub_plotcord, face = "bold", hjust = 0) +
  # discard default grid + titles in ggplot2 
  theme(legend.key.size = unit(80 ,"points")) + 
  theme(legend.text = element_text(size = 20)) + 
  theme(legend.title = element_text(size = 30)) + 
  theme(legend.position = 'bottom') +
  theme(panel.background = element_blank()) + #theme(legend.position="none")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  #theme( legend.background = element_rect(colour = NA)) + 
  #theme(panel.background = element_rect(fill = "white", colour = NA)) + 
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(panel.background = element_rect(fill="#AAAAAA", colour="#000000"))

dev.off()
