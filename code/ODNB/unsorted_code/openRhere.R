#@S Placeholder file to note that R should be opened with this as the 
#@S   main directory. (Open ESS here...)

# no code!
load("network_visualization/conf_matrix.Rdata")
ls()
conf_matrix
which(colnames(lambda_matrix) == "Francis Bacon")
thres = 0.8
deg1 = which(lambda_matrix[,1096] >= thres)
length(deg1)
deg2 = which(apply(lambda_matrix[c(deg1, 1096),], 2, max) >= thres)
length(deg2)
deg3 = which(apply(lambda_matrix[c(1096, deg1, deg2),], 2, max) >= thres)
length(deg3)

cat("thres = ", thres, " ----")
c(length(deg1), length(deg2), length(deg3))

# TODO: Make emacs follow my indentation preferences...
node_names = as.character(node_names)
iggy = c(which(node_names == "Winston Churchill"), which(node_names == "King Charles"), which(node_names == "American Indians"))

cm[1930,]
cm[2914,]

thres = 0.4

isaac_nbr = setdiff(which(cm[1930,] >= thres), iggy)
isaac_nbr = isaac_nbr[order(cm[1930, isaac_nbr], decreasing = TRUE)]
milton_nbr = setdiff(which(cm[2914,] >= thres), iggy)
milton_nbr = milton_nbr[order(cm[2914, milton_nbr], decreasing = TRUE)]
 

common_nbrs = intersect(isaac_nbr, milton_nbr)


helperf = function(a) {
  second_deg = setdiff(intersect(milton_nbr,which(cm[a,] >= thres)), iggy)
  second_deg = second_deg[order(cm[a,second_deg], decreasing = TRUE)]
  
  if (length(second_deg > 0)) { 
    temp = cbind("Isaac Newton", "--", cm[1930,a],"--", node_names[a], "--", cm[a,second_deg], "--", node_names[second_deg], "--", cm[2914,second_deg],"--", "John Milton")
    temp = rbind(temp, rep("", times = 13))
    return(temp)
  } else {
    return(NULL)
  }
}

z = lapply(isaac_nbr, helperf)
nres = z[[1]]
for(j in 2:length(z)) {
  nres = rbind(nres, z[[j]])
}
write.csv(nres, file = "test_newton.csv")

write.csv(cbind("Isaac Newton", "--", cm[1930,common_nbrs], "--", node_names[common_nbrs],"--", cm[2914, common_nbrs], "--", "John Milton"), file = "one_deg.csv")


helperm = function(a) {
  second_deg = setdiff(intersect(isaac_nbr,which(cm[a,] >= thres)), iggy)
  second_deg = second_deg[order(cm[a,second_deg], decreasing = TRUE)]
  
  if (length(second_deg > 0)) { 
    temp = cbind("John Milton", "--", cm[2914,a],"--", node_names[a], "--", cm[a,second_deg], "--", node_names[second_deg], "--", cm[1930,second_deg],"--", "Isaac Newton")
    temp = rbind(temp, rep("", times = 13))
    return(temp)
  } else {
    return(NULL)
  }
}

z = lapply(milton_nbr, helperm)
nres = z[[1]]
for(j in 2:length(z)) {
  nres = rbind(nres, z[[j]])
}
write.csv(nres, file = "test_milton.csv")
