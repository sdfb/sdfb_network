###########removal:
#words.totest = c("County","Society","Agriculture","School","Museum","Journal",
#"Almighty","Exposition","Garden","Gardens","Collection","Court","Method","Company",
#"Paraphrased","Defence","Theatre","Theater","Handbook","Grammar","Advancement","Foreign",
#"Institution","Station","Council","Clinic","Annal","Annals","Research","World")
#coln=NULL
#for(j in words.totest) {
#coln=c(coln,grep(j,wordss))
#}
#length(unique(coln))

test.list = list()
a = list()
for(i in 1:length(count.list)) {
if(!is.null(count.list[[i]])) {
 if(length(count.list[[i]]) > 1) {
 a[[i]] = cbind(count.list[[i]][[1]][,-3:-4], i)
colnames(a[[i]])[7] = "DocNum"
}
}
if (i %% 2500 == 0) {print(i)} 
}

b = list()
for(i in 1:16656) {
j = 6*i - 5
b[[i]] = rbind( a[[j]], a[[j+1]], a[[j+2]], a[[j+3]], a[[j+4]], a[[j+5]])
print(i)
}

r = list()
for(i in 1:2776) {
j = 6*i - 5
r[[i]] = rbind( b[[j]], b[[j+1]], b[[j+2]], b[[j+3]], b[[j+4]], b[[j+5]])
print(i)
}
r = s

s = list()
for(i in 1:42) {
j = 4*i - 3
s[[i]] = rbind( s[[j]], s[[j+1]], s[[j+2]], s[[j+3]])
print(i)
}



bob
a[[99941]]

finalist = s[[1]]
for(j in 2:42) {
finalist = rbind(finalist, s[[j]])
print(j)
}


finalist = matrix("", nrow = sum(lengths.table.id), ncol = 7)

curid = 1
for(i in 1:length(count.list)) {
 if(!is.null(count.list[[i]]) & length(count.list[[i]]) > 1) {
  temp = count.list[[i]][[1]]
  numents = dim(temp)[1]
  rows = curid:(curid+numents-1)
  finalist[rows,7] = i
  finalist[rows,1] = temp[,1]
  finalist[rows,2] = temp[,2]
  finalist[rows,3] = temp[,5]
  finalist[rows,4] = temp[,6]
  finalist[rows,5] = temp[,7]
  finalist[rows,6] = temp[,8]
  curid = curid + numents
 }
 if(i %% 1000 == 0) {print(i)}
}

save(finalist, file = "finalist.Rdata")

