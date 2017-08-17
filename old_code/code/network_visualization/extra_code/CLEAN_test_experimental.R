#@S This file contains some testing code for the plot functions (some might be outdated)
#@S It also contains some experimental code


# OLD Test code: 
# load("network_visualization/SS.small.Rdata")
# lambda_matrix = total.lamb
# test = read.csv("network_visualization/new.idnamecount.csv")
# rownames(lambda_matrix) = test$Entity.Name
# colnames(lambda_matrix) = test$Entity.Name
# 
# network_params = list(vis_type = "ego", target = "James Harrington", topK = 20,
#                       threshold = 0.6, node_coord_type = "default",
#                       node_setting_table = data.frame(type = c("CENTER", "OTHER"), color = c("black", "red"), stringsAsFactors = FALSE))
# test = create_network_object(params, log_file = "test.txt", lambda_matrix, verbose = TRUE)
# 
# test2 = create_nodes(network_params, test$node_data, test$subset_matrix, test$net, test$adjm)
# 
# test3 = create_edges(network_params, test$node_data, test$subset_matrix, test$net, test$adjm,
#                      plotcord = test2)
# 
# # FOR GGPLOT: Need variables in current environment. cannot use as separate function byitself
# test4 = create_ggplot(network_params, test$node_data, test$subset_matrix, test$net, test$adjm,
#                       test2, test3[[1]], test3[[2]])


#### Additional code that could potentially be added: 

# TODO: [idea] Randomization of edge colors to slightly different shades of grey
# TODO: -- probably want to enforce some sort of difference if there are few edges? ie not exactly uniformly random
# Code for randomization of edge colors to a shade of grey (if edge is more or less unimportant)
#     if (!is.null(network.params$grey.rand)) {
#       inds = which(edges$color == network.params$grey.rand) 
#       sam  = sample(50:80, size = length(inds), replace = TRUE)
#       edges$color[inds] = paste('gray', sam, sep = "")
#     }
#   }
