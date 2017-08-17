load("update2.datamat.Rdata")
load("newpoismod.Rdata")

load("scovmat.label.Rdata")
load("apois2.Rdata")

a.poisson$lambda
good.pois$lambda

cbind(a.poisson$sparsity[-1], good.pois$sparsity)
#a.pois 4 compared to good.pois 5,6

a.poisson$lambda[4]
old.names = test[grep(" ", test)]
new.names = upd.two.word.names
old.touse = match(new.names, old.names)

adj.old = a.poisson$path[[4]][old.touse,old.touse]
adj.new = good.pois$path[[8]]



find.names.from.nodenum.both <- function(edge.mat.mb = gg1, edge.mat.pois = gg2, 
                                         names = new.names, nodenum) {
  result = list()
  nodenum = as.numeric(nodenum)
  for(j in 1:length(nodenum)) {
    result[[j]] = list()
    result[[j]][[1]] = c("Person is:", names[nodenum])
    matches = which(edge.mat.mb[nodenum[j],] + edge.mat.pois[nodenum[j],] == 2) 
    result[[j]][[2]] = cbind(matches, names[matches])
    colnames(result[[j]][[2]]) = c("Node Number", "Person [BOTH LISTS]")
    
    matrix2 = edge.mat.mb - edge.mat.pois
    matches = which(matrix2[nodenum[j],] == 1)
    result[[j]][[3]] = cbind(matches, names[matches])
    colnames(result[[j]][[3]]) = c("Node Number", "Person [Old Only]")
    
    matrix3 = edge.mat.pois - edge.mat.mb
    matches = which(matrix3[nodenum[j],] == 1)
    result[[j]][[4]] = cbind(matches, names[matches])
    colnames(result[[j]][[4]]) = c("Node Number", "Person [New Only]")
    
  }
  return(result)
}
sum(adj.old)
sum(adj.new)
sum(adj.old - adj.new)
sum(adj.old + adj.new == 2)

find.names.from.nodenum.both(edge.mat.mb = adj.old, edge.mat.pois = adj.new, nodenum = 1743)



## Trying to check how correlation is affected by weird values
# 2453 : 97
# 2583 : 241, 330
# 2269 : 374
# 2653
c(which(upd.data.matrix[,2453] == 97),
  which(upd.data.matrix[,2583] > 97),
  which(upd.data.matrix[,2269] > 97)) -> rm.test

cor(upd.data.matrix[,c(2269,2453,2583,2653)])
cor(upd.data.matrix[-rm.test,c(2269,2453,2583,2653)])







