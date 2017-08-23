load("sim.lambs.Rdata")
read.csv("new.idnamecount.csv") -> temp

z = temp[order(temp[,4]),]
ids.tosend = tail(z, 500)[,2]

write.csv(temp[ids.tosend,], file = "subset.idnamecount.csv")

colnames(sim.lambs) = 1:6289
rownames(sim.lambs) = 1:6289

sub.lambs = sim.lambs[ids.tosend, ids.tosend]

write.csv(sub.lambs, file = "subset.adjmatrix.csv")
