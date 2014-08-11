load("sim.lambs.Rdata")

post.topic = posterior(topic.models)$topics
topics = topics(topic.models)
counts = table(topics)

find.pcts = function(x) {
  id = topics[x]
  res = rep(0, times = 0)
  for(j in 1:10) {
    if(j == id) {
      res[j] = sum(sim.lambs[x,which(topics == j)]>0)/(counts[j]-1)
    } else {
      res[j] = sum(sim.lambs[x,which(topics == j)]>0)/counts[j]
    }
  }
  return(res)
}

find.pcts(3)*100

edge.dists = matrix(0, nrow = 6289, ncol = 10)

for(j in 1:6289) {
 edge.dists[j,] = find.pcts(j) * 100
 print(j)
}

edge.dists[1,]/sum(edge.dists[1,])
sum(sim.lambs > 0)

head(edge.dists[2,])

topic.list = list()
for(j in 1:10) {
  topic.list[[j]] = which(topics == j)
}
res.mat = matrix(0, nrow = 10, ncol = 10)
for(j in 1:10) {
for(k in 1:10) {
 res.mat[j,k] = sum(sim.lambs[topic.list[[j]],topic.list[[k]]] >= 0.0001)
}}

den = counts * c(543, rep(544, times = 9))
row1_prob = res.mat[1,] / den

row1_prob_s1 = row1_prob / sum(row1_prob)
row1_prob_s1 = as.vector(row1_prob_s1)

dists = rep(0, times = 6289)

for(j in 1:6289) {
  dists[j] = dist(rbind(row1_prob_s1,edge.dists[j,]/sum(edge.dists[j,])))
}
for(j in 1:6289) {
  dists[j] = dist(rbind(as.vector(row1_prob),edge.dists[j,]))
}
plot(post.topic[,1], dists)