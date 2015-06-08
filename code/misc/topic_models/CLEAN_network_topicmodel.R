load("finaldata2.Rdata")
load("end2.Rdata")

load("final.dmnames.Rdata")

bios = list()
for(person in 1:length(dm.names)) {
  a = which(final.data$MatchName == dm.names[person])
  
  fins = final.data[a,]
  #nres = sum(fins[,4])
  
  res = list()
  for (j in 1:nrow(fins)) {
    
    id = fins[j,3]
    docid = fins[j,2]
    end.res2 = end.result[[docid]][[2]]
    pos = end.res2[end.res2[,3] == id,2]
    inds.tocheck = NULL
    for (k in 1:length(pos)) {
      inds.tocheck = c(inds.tocheck, (-15:25 + pos[k]))
    }
    inds.tocheck = unique(inds.tocheck)
    inds.tocheck = inds.tocheck[inds.tocheck > 0]
    inds.tocheck = inds.tocheck[inds.tocheck <= nrow(end.result[[docid]][[3]])] 
    remove = which(end.result[[docid]][[3]][,2] != "")
    inds.tocheck = setdiff(inds.tocheck, remove)
    
    res[[j]] = paste(end.result[[docid]][[3]][inds.tocheck,1], collapse = " ")
  }
  res = paste(c(res, recursive = TRUE), collapse = " ")
  bios[[person]] = res
  print(person)
}

save(bios, file = "gen.bios.Rdata")

load("gen.bios.Rdata")


# cleaned until here ------------------------------------------------------




library(tm)
library(topicmodels)
load("final.dmnames.Rdata")

bios = c(bios, recursive = TRUE)
bios.src = VectorSource(bios)

corpus = VCorpus(bios.src)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeNumbers)

# cleaned until here ------------------------------------------------------

dt.matrix = DocumentTermMatrix(corpus)

save(dt.matrix, file = "dt.matrix.bios.Rdata")

load("dt.matrix.bios.Rdata")
topicmod = LDA(x=dt.matrix, k = 5)

library(tm)
library(topicmodels)
load("gen.bios.Rdata")
bios = c(bios, recursive = TRUE)

bios.src = VectorSource(bios)
corpus = VCorpus(bios.src)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, stopwords(kind = 'en'))
corpus = tm_map(corpus, stemDocument)

to.rm.list = c("age", "april", "brother", "cousin", "date", "daughter", "day", "death",
               "decemb", "die", "earlier", "father", "februari", "five", "fourth", "friend",
               "husband", "januari", "juli", "june", "march", "month", "mother", "novemb",
               "octob", "septemb", "six", "third", "fifth", "nephew", "seven", "sir")

corpus = tm_map(corpus, removeWords, to.rm.list)

save(corpus, file = "tm_corpus.Rdata")

load("tm_corpus.Rdata")

dt.matrix = DocumentTermMatrix(corpus)

term_tfidf <-
  tapply(dt.matrix$v/row_sums(dt.matrix)[dt.matrix$i], dt.matrix$j, mean) *
  log2(nDocs(dt.matrix)/colSums(dt.matrix > 0))
summary(term_tfidf)

torm = which(table(dt.matrix$j) > 100)
# dt.matrix2 = dt.matrix[,-torm]

sub.samples = list()
sub.samples[[1]] = sample(1:6289, size = 2000)
sub.samples[[2]] = sample(setdiff(1:6289, sub.samples[[1]]), size = 2000)
sub.samples[[3]] = sample(setdiff(1:6289, c(sub.samples[[1]], sub.samples[[2]])), size = 2000)

topic.models = list()
for (K in 3:10) {
  topic.models[[K]] = list()
  for(S in 1:3) {
    print(date())
    print(paste("Starting", S, "of 3: K = ", K))
    topic.models[[K]][[S]] = LDA(x = dt.matrix[-sub.samples[[S]],], k=K)
  }
}
save(topic.models, sub.samples, file = "tms.cv.Rdata")

system.time((topicmod = LDA(x=dt.matrix, k = 5)))
save(topicmod, file = "topicmodel.fit.Rdata")

load("topicmodel.fit.Rdata")





library(tm)
library(topicmodels)
load("tm_corpus.Rdata")

dt.matrix = DocumentTermMatrix(corpus)

torm = which(table(dt.matrix$j) > 100)
 dt.matrix2 = dt.matrix[,-torm]

topic.models = list()
for (K in c(3,5,7,10)) {
  print(date())
  print(paste("Starting K = ", K))
  
  topic.models = LDA(x = dt.matrix2, k = K)
    
}

save(topic.models, file = "tms.Rdata")


load("tms.Rdata")
dim(topics(topic.models,2))
names(posterior(topic.models))
head(posterior(topic.models)$topics)



tops = topics(topic.models)

net$edges



result.matrix = matrix(0, nrow = 10, ncol = 10)
edges = cbind(c(a[1,], recursive = TRUE), c(a[2,], recursive = TRUE))
dim(edges)

for(j in 1:nrow(edges)) {
      s = tops[edges[j,1]]
      e = tops[edges[j,2]]
      result.matrix[s, e] = result.matrix[s, e] + 1
  
  print(j)
}

result.m1 = result.matrix
for(j in 1:9) {
  for(k in (j+1):10) {
    a = result.matrix[j,k] + result.matrix[k,j]
    result.m1[j,k] = a
    result.m1[k,j] = a
  }
}
for(j in 1:10) {
  result.m1[j,j] = result.matrix[j,j]
}

result.m2 = result.matrix + t(result.matrix)

row_s1 = result.m2
for(j in 1:10) {
  row_s1[j,] = row_s1[j,] / sum(row_s1[j,])
}

round(row_s1, 4)
round(table(tops)/6289, 4)

edge.fracs = apply(result.m2, 2, sum)/sum(apply(result.m2, 2, sum))
round(row_s1 - matrix(edge.fracs, nrow = 10, ncol = 10, byrow = TRUE), 4)

renorm.k = function(v,k) {
  v[k] = 0
  v = v/sum(v)
  return(v)
}
renorm.k(edge.fracs, k = 1)

renorm.diff = matrix(0,nrow = 10, ncol = 10)
for(j in 1:10) {
  renorm.diff[j,] = renorm.k(row_s1[j,], k=j) - renorm.k(edge.fracs, k = j)
}
round(renorm.diff, 4)

############
table(topics(topic.models))
tab = matrix(nrow = 8, ncol = 10)
for (j in 8:1) {
  tab[9-j,] = apply(posterior(topic.models)$topic > j/10, 2, sum)
}


##############

post = posterior(topic.models)$topic
res.mat.post = matrix(0, nrow = 10, ncol = 10)

for(j in 1:nrow(edges)) {
  s = edges[j,1]
  e = edges[j,2]
  
  res.mat.post = res.mat.post + post[s,] %*% t(post[e,])
  print(j)
}

RMP = res.mat.post + t(res.mat.post)
round(RMP,1)

rs1 = RMP
for(j in 1:10) {rs1[j,] = rs1[j,] / sum(rs1[j,]) }


round(rs1 - matrix(edge.fracs, nrow = 10, ncol = 10, byrow = TRUE), 4)

edge.fracs = apply(RMP, 2, sum)/sum(RMP)
renorm.diff = matrix(0,nrow = 10, ncol = 10)
for(j in 1:10) {
  renorm.diff[j,] = renorm.k(rs1[j,], k=j) - renorm.k(edge.fracs, k = j)
}
round(renorm.diff, 4)


## Code for plotting topic models
##### File/Package Loading #####
# these are needed for all plots
library(network)
library(ggplot2)
library(sna)
library(ergm)

library(grid)
library(scales) # for muted

id.name.count = read.csv("new.idnamecount.csv", stringsAsFactors = FALSE)[,-1]
load("sparse.sim.lambs.Rdata")
load("largenet.Rdata")

graphdat = readLines("biggraph.txt")[1:(N+1)]
graphsize = graphdat[1]
nodelocs = strsplit(graphdat[-1], split = " ")



edge.weight.range <- c(0.5,1.5) ##Controls amount of edge ink
name.size.range <- c(18,25) ##Controls amount of text ink
name.size.range <- c(8,12) ##Controls amount of text ink - FOR TOPIC MODEL

plotcord = matrix(0, nrow = N, ncol = 2)
plotcord = as.data.frame(plotcord)
for(j in 1:N) {
  plotcord[j,] = as.numeric( nodelocs[[j]][3:4])
}

colnames(plotcord) = c("X1","X2")
plotcord$elements <- rep("black", times = N)

plotcord$size <- log(id.name.count[node.ids,3], base = 10)

#plotcord$size <-(plotcord$size/max(plotcord$size))*diff(range(name.size.range))+name.size.range[1]
plotcord$names <- get.vertex.attribute(net, "names")


###Create plotting matrix for edges
edglist <- as.matrix.network.edgelist(net)
edges <- data.frame(plotcord[edglist[,1],c(1,2)], plotcord[edglist[,2],c(1,2)])
colnames(edges) <-  c("X1","Y1","X2","Y2")
edges$weight <- rep(1, times = nrow(edges))

load("tms.Rdata")
k = 10
cols.touse = rep('beige', times = 10)
cols.touse[k] = 'blue'
c1 = cols.touse[topics(topic.models)] #need to load topicmodel

plotcord$prob = posterior(topic.models)$topics[,2]

# red/green
#scale_fill_gradient("Approximate Year of Birth", low = "#5EFF87",
#                    high = "#FF5E5E", limits = c(1450,1750), na.value = "#FF5E5E",
#                    space = "rgb") 

#################################
png(paste(format(Sys.time(), "%a%b%d%H%M%S"), ".png"), width=5000, height=4000, res=96)

ggplot() +
  #  geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2,size=weight), data=edges, colour=e1) +
  geom_point(aes(X1, X2, fill = prob, size = size), data=plotcord, color="black",pch=21) + #same as above but color is white
  scale_fill_gradient("Estimated Posterior Probability of Topic", low = "#2B8CBE",
                      high = "#FF5E5E", limits = c(0,1), na.value = "#FF5E5E",
                      space = "rgb") +
  scale_size("Log(Total Mentions) [base 10]",range=range(c(edge.weight.range,name.size.range))) +
  scale_colour_brewer(palette="Set1") +
  
  scale_x_continuous(breaks = NA) + scale_y_continuous(breaks = NA) +
  # discard default grid + titles in ggplot2 
  opts(legend.key.size = unit(150 ,"points")) +
  opts(legend.text = theme_text(size = 50)) +
  opts(legend.title = theme_text(size = 70)) +
  opts(legend.position = 'bottom') +
  opts(panel.background = theme_blank()) + #opts(legend.position="none")+
  opts(axis.title.x = theme_blank(), axis.title.y = theme_blank()) +
  #opts( legend.background = theme_rect(colour = NA)) + 
  #opts(panel.background = theme_rect(fill = "white", colour = NA)) + 
  opts(panel.grid.minor = theme_blank(), panel.grid.major = theme_blank()) +
  opts(panel.background = theme_rect(fill='black', colour='black'))


dev.off()
