load("1231cb500.test.fit.Rdata")
load("1230sp.datamat.sparse.Rdata")
library(Matrix)

# iggy = which(!find.done(results))
# save(iggy, file = "refits.Rdata")

# forgot to remove
iggy = c(488, 545 ,881, 1360, 1487)

#sparse.dm = sparse.dm[,-to.remove]
#dm.names = dm.names[-to.remove]
#results = results[-to.remove]


n = length(dm.names)

res.lambs = matrix(0, nrow = n, ncol = n)




spi <- function(x) {
  return(which(x > 0)[1])
}

# spi(results[[1]]$)
todo = setdiff(1:n, iggy)

for(j in todo) {
  locs = apply(results[[j]]$beta, 1, spi)
  coords = which(!is.na(locs))
  adj.coords = coords
  adj.coords[adj.coords >= j] = adj.coords[adj.coords >= j] + 1
  res.lambs[j,adj.coords] = results[[j]]$lambda[locs[coords]]
  print(j)
}

save(res.lambs, file = "res.lambs.Rdata")

# load("refits.Rdata")
# for(j in iggy) {
#   locs = apply(results[[j]]$beta, 1, spi)
#   coords = which(!is.na(locs))
#   res.lambs[j,coords] = results[[j]]$lambda[locs[coords]]
#   print(j)
#   
# }
# save(res.lambs, file = "res.lambs.Rdata")

sim.lambs = res.lambs

for(j in 1:n) {
  for(k in j:n) {
    if(sim.lambs[j,k] > sim.lambs[k,j]) {
      sim.lambs[k,j] = sim.lambs[j,k]
    } else if (sim.lambs[k,j] > sim.lambs[j,k]) {
      sim.lambs[j,k] = sim.lambs[k,j]
    }
  }
  print(j)
}


dm.names = dm.names[-iggy]
sparse.dm = sparse.dm[,-iggy]

sim.lambs = sim.lambs[-iggy,-iggy]


save(sim.lambs, file = "sim.lambs.Rdata")
