## Code for exploration with latentnet package. Presumably useless at this stage, but will keep in case. 

## Latentnet

library(latentnet)


# 886 = JH
b = c(886, topk.rank(edge.add[886,], k = 30)[,1])
ns = b

#make adj.matrix
jh.1.adj = matrix(0, nrow = length(ns), ncol = length(ns))

count = 1
for(j in 1:length(ns)) {
  b = topk.rank(dat = edge.add[ns[j],], k = 30)
  if (length(b) > 0) {
    for(k in 1:dim(b)[1]) {
      if (length(intersect(b[k,1], ns)) > 0) {
        jh.1.adj[j,which(ns == b[k,1])] = 1
      }
    }
  }
}
a = (jh.1.adj + t(jh.1.adj)) / 1.9
jh.1.adj = round(a)

adj.mat = network(x = jh.1.adj, vertex.attrnames = list("vertex.names"),
                  vertex.attr = list(corr.names[ns]), directed = FALSE)

try.fit = ergmm(adj.mat ~ euclidean(d=2))
try.fit = ergmm(adj.mat ~ euclidean(d=2, G=3))
plot(try.fit)

apply(try.fit$sample$Z, c(2,3), mean)

network.vertex.names(adj.mat)
pts = apply(try.fit$sample$Z, c(2,3), mean)
plot(pts)
identify(pts, labels = network.vertex.names(adj.mat))
