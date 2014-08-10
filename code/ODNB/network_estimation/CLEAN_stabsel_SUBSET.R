#@S Temporary file

# NOTE:::
# Description of code: just extracting the lambdas from the 10 runs from awhile back.
#  no real need to keep this around (although the writeMM function is nice to reference)



# TODO: Clear this file



# proc on my computer  
#
#
#
#
library(Matrix)
total.lamb = Matrix(0, nrow = 6289, ncol = 6289, sparse = TRUE)
for(j in 1:10) {
  load(paste("ALLstabsel",j,".Rdata", sep = ""))
  total.lamb = total.lamb + (lamb.mat > 0)
  print(j)
}
# used wrong code, so need to adjust rows..
for(k in 1:6288) {
  total.lamb[k,(k+1):6289] = total.lamb[k,k:6288]
  print(k)
}
for(k in 1:6289) {
  total.lamb[k,k] = 0
  print(k)
}
total.lamb = total.lamb / 10
total.lamb = (total.lamb + t(total.lamb)) / 2
save(total.lamb, file = "SS.small.Rdata")
load("sparse.sim.lambs.Rdata")

load("0213sp.datamat.sparse.Rdata")
write.csv(dm.names, file = "nodenames.csv")

writeMM(total.lamb, file = "confidence.matrix.mtx")
