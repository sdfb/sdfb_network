res.lambs = matrix(0, nrow = 6751, ncol = 6751)

iggy = which(!find.done(results))
save(iggy, file = "refits.Rdata")


spi <- function(x) {
  return(which(x > 0)[1])
}

spi(results[[1]]$)

todo = setdiff(1:6751, iggy)

for(j in todo) {
locs = apply(results[[j]]$beta, 1, spi)
coords = which(!is.na(locs))
res.lambs[j,coords] = results[[j]]$lambda[locs[coords]]
print(j)
}

save(res.lambs, file = "res.lambs.Rdata")

load("refits.Rdata")
for(j in iggy) {
locs = apply(results[[j]]$beta, 1, spi)
coords = which(!is.na(locs))
res.lambs[j,coords] = results[[j]]$lambda[locs[coords]]
print(j)

}

save(res.lambs, file = "res.lambs.Rdata")

sim.lambs = res.lambs

for(j in 1:6751) {
for(k in j:6751) {
if(sim.lambs[j,k] > sim.lambs[k,j]) {
  sim.lambs[k,j] = sim.lambs[j,k]
} else if (sim.lambs[k,j] > sim.lambs[j,k]) {
  sim.lambs[j,k] = sim.lambs[k,j]
}
}

print(j)

}

save(sim.lambs, file = "sim.lambs.Rdata")
write.csv(sim.lambs, file = "lambda.matrix.csv")









