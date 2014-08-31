library(BMN)

load("0325sp.datamat.sparse.Rdata")

bin.dm = sparse.dm > 0

size = c(10,25,50,75,100,250,500,750,1000,2000)
for(j in 1:10) {
  save(size, file = paste("RUN",j,".note.Rdata"))
  a = sample(1:6289, size = size[j])
  submat = matrix(as.numeric(bin.dm[,a]), ncol = size[j])
  
time = list()
rand.fit = list()
time = system.time((
  
  rand.fit = BMNPseudo(submat, rhoVec = unique(c(100,90,80,70,60,50,40,30,20,10,5,4,3,
                                                 2,1,0.8,0.7,0.6,0.5,
                                                 seq(0.5,0.1, by= -0.1),
                                                 seq(0.1, 0.05, by = -0.01))),
                       verbose = FALSE)
  
  
  ))
  save(time, rand.fit, file = "BMNtest.Rdata") 
}

plot(1:5, c(10, 20, 60, 90, 120))



## random test

x = rnorm(300, 2,2)
y = rnorm(300, 0,1) + x
z = rnorm(300)
w = matrix(as.numeric(cbind(x > 0, y > 0, z > 0)), ncol = 3)
cor(w)
w = rbind(w, matrix(0, ncol = 3, nrow = 700))

a = BMNPseudo(w, rhoVec = 3)
