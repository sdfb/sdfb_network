## work to find out what saint references are

# relies on some data...
load("truedocs.Rdata")
load("count.list.Rdata")
load("finalist.Rdata")
load("scovmat.label.Rdata")
cur.names = test[grep(" ", test)]
saints.names = cur.names[2122:2223]

find.random.occurrence <- function(name) {
  matches = which(finalist[,2] == name)
  rnd.ind = sample(matches, size = 1)
  doc.num = as.numeric(finalist[rnd.ind,7])
  #matches2 = which(count.list[[doc.num]][[2]][,2] == name)  
  #rnd.ind2 = sample(matches2, size = 1)
  #return(c(doc.num,count.list[[doc.num]][[2]][rnd.ind2,1]))
  return(doc.num)
}

find.random.occurrence("St Cuthbert")


find.nearby.words <- function(articlenum, name) {
  doc = true.docs[[recordt[articlenum]]][[2]]
  res = grep(name, doc)
  if (length(res) > 0) {
  cat("Sentence containing: ", name, "\n")
  print(doc[res[1]])
  } else {
    cat("Error with", name, "\n")
  }
  invisible(0)
}

find.nearby.words(26227, "St John")

for(i in 1:102) {
  a = find.random.occurrence(saints.names[i])
  find.nearby.words(a,saints.names[i])
  }


# see text documents 'saints passages.txt'



##########################
# looking at cv results
load("CVresults1.Rdata")
load("CVresults2.Rdata")

apply(test.RD.fm[,,1:8], 2, mean)
apply(train.RD[,1:8,1:8], 2, mean)
apply(test.RD[,1:8,1:8], 2, mean)

train.RD[1:5,1:8,1:8]
test.RD[1:5, 1:8, 1:8]
lambdas = seq(.2, .06, by = -.02)

plot.random.testRD <- function(n, which.runs = 1:8) {
  indices = sample(which.runs, size = n, replace = TRUE)
  plot(-100, -100, xlim = c(0, 0.3), ylim = c(0, 200)) 
  for(i in 1:length(which.runs)) {
    N = sum(indices == which.runs[i])
    if (N > 0) {
      use = sample(1:dim(test.RD)[1], size = N)
      for(j in 1:length(use)) {
        lines(lambdas, test.RD[use,which.runs[i],1:8])
      }
    }
  }
}

find.low.mean.plot <- function(n) {
  matr = matrix(0, nrow = n, ncol = 8)
  for(i in 1:n) {
    print(i)
    indices = sample(1:8, size = 1)
    use = sample(1:dim(test.RD)[1], size = 1)
    if (all(test.RD[use,indices,1:8] < 200)) {
      matr[i,] = test.RD[use,indices,1:8]
    } else { matr[i,] = NA }
  }
  res = apply(matr, 2, mean, na.rm = TRUE)
  plot(lambdas, res, type = "b")
}
plot.random.testRD(n=1)
find.low.mean.plot(n = 5000)
















