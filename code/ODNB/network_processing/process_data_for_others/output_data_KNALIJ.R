#@S Output data to csv format, using r,c,confidence format. 

load("network_visualization/conf_matrix.Rdata")

M = conf_matrix / 100 

test = read.csv("network_visualization/new.idnamecount.csv", stringsAsFactors = FALSE)
names_rm = c("Society of Antiquaries", "K Charles", "King Charles")
rm = match(names_rm, test[,3])

M_adj = M[-rm,-rm]
names_adj = test[-rm,-1:-2]
names_adj = cbind(ID=1:nrow(names_adj), names_adj)

res = NULL

for(j in 1:(ncol(M_adj)-1)) {
  lines = intersect(which(M_adj[j,] > 0), j:ncol(M_adj))
  to_add = cbind(j, lines, M_adj[j,lines])
  if (ncol(to_add) < 3) {
    
  } else {
    res = rbind(res, to_add)
  }
  print(j)
}

colnames(res) = c("node1", "node2", "ConfidenceEst")
write.csv(res, file = "conf_ests.csv", row.names = FALSE)

write.csv(names_adj, file = "names.csv", row.names = FALSE)
