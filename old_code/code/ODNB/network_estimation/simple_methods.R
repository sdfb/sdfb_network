## do simple counts?

load("data_manual/ODNB_dataset.Rdata")
library(Matrix)

sparse.cor3 <- function(x, thres = 0.1){
  cSums = colSums(x)
  nonzeros = which(cSums > 0)
  x = x[, nonzeros]
  
  n <- nrow(x)
  
  cMeans <- colMeans(x)
  cSums <- colSums(x)
  
  # Calculate the population covariance matrix.
  # There's no need to divide by (n-1) as the std. dev is also calculated the same way.
  # The code is optimized to minize use of memory and expensive operations
  covmat <- tcrossprod(cMeans, (-2*cSums+n*cMeans))
  crossp <- as.matrix(crossprod(x))
  covmat <- covmat+crossp
  
  sdvec <- sqrt(diag(covmat)) # standard deviations of columns
  res = covmat/crossprod(t(sdvec)) # correlation matrix
  
  highcorlist = list()
  
  for(j in 1:nrow(res)) {
    anyinds = which((res[,j] > thres) & 1:nrow(res) > j)
    if (length(anyinds) > 0) {
      highcorlist[[j]] = data.frame(I1 = nonzeros[j], I2 = nonzeros[anyinds], Cor=res[j,anyinds])
    }
  }
  res = do.call(rbind, highcorlist)
  
  return(res)
}

cor_list = list()
for(j in 1:100) {
  cat(j, '---', date(), "\n")
  load(paste("data/ODNB_newfinal/sampmatrix", j, ".Rdata", sep = ""))
  cor_list[[j]] = sparse.cor3(dcmat)
}

save(cor_list, file = "data/cor_thresholded.Rdata")



load("data/cor_thresholded.Rdata")
library(Matrix)

thres_mat_01 = Matrix(0, nrow = 13309, ncol = 13309, sparse = TRUE)
thres_mat_02 = Matrix(0, nrow = 13309, ncol = 13309, sparse = TRUE)
thres_mat_03 = Matrix(0, nrow = 13309, ncol = 13309, sparse = TRUE)
thres_mat_04 = Matrix(0, nrow = 13309, ncol = 13309, sparse = TRUE)
thres_mat_05 = Matrix(0, nrow = 13309, ncol = 13309, sparse = TRUE)
thres_mat_06 = Matrix(0, nrow = 13309, ncol = 13309, sparse = TRUE)
sum_threscor = Matrix(0, nrow = 13309, ncol = 13309, sparse = TRUE)
for(j in 1:100) {
  cat("\n Working on iteration ---- ", j, "\n")
  for(k in 1:nrow(cor_list[[j]])) {
    r = cor_list[[j]]
    if (r$Cor[k] > 0.1) { thres_mat_01[r$I1[k],r$I2[k]] = thres_mat_01[r$I1[k],r$I2[k]] + 1 }
    if (r$Cor[k] > 0.2) { thres_mat_02[r$I1[k],r$I2[k]] = thres_mat_02[r$I1[k],r$I2[k]] + 1 }
    if (r$Cor[k] > 0.3) { thres_mat_03[r$I1[k],r$I2[k]] = thres_mat_03[r$I1[k],r$I2[k]] + 1 }
    
    if (r$Cor[k] > 0.4) { thres_mat_04[r$I1[k],r$I2[k]] = thres_mat_04[r$I1[k],r$I2[k]] + 1 }
    if (r$Cor[k] > 0.5) { thres_mat_05[r$I1[k],r$I2[k]] = thres_mat_05[r$I1[k],r$I2[k]] + 1 }
    if (r$Cor[k] > 0.6) { thres_mat_06[r$I1[k],r$I2[k]] = thres_mat_06[r$I1[k],r$I2[k]] + 1 }
    sum_threscor[r$I1[k],r$I2[k]] = sum_threscor[r$I1[k],r$I2[k]] + r$Cor[k]
    if (k %% 100 == 0) { cat(k, "")}
  }
}

save(thres_mat_01, thres_mat_02, thres_mat_03, thres_mat_04, thres_mat_05, thres_mat_06, file = "data/cor_thres.Rdata")

library(Matrix)
load("data/cor_thres.Rdata")
res = matrix(nrow = sum(thres_mat_01 > 0), ncol = 3)
count = 1
for(j in 1:13309) { 
  print(j)
  for(k in which(thres_mat_01[j,] > 0)) {
    res[count,1] = j
    res[count,2] = k
    res[count,3] = thres_mat_01[j,k]
    count = count + 1    
  }
}
res = data.frame(Node1 = res[,1], Node2 = res[,2], Conf = res[,3])
write.csv(res, file = "confidence_rough.csv", row.names = FALSE)
write.table(`)


# Testing -----------------------------------------------------------------

# 
# head(nodeset)
# which(nodeset$ODNB_CORRECT_ID == 18800)
# nodeset[8309,]
# k = grep("Harrington", nodeset$full_name)
# nodeset[k,]
# y = 5566
# y = 8309
# y = 11671
# z =c (which(thres_mat_01[,y] > 0), which(thres_mat_01[y,] > 0))
# nodeset[z, ]
# temp = cbind(round((thres_mat_01[z,y]+thres_mat_01[y,z]) / 26, 2), nodeset$full_name[z], nodeset$first_name[z], nodeset$surname[z])
# temp[order(temp[,1], decreasing = TRUE),]
# 
# 
# load(zzfile_base_entity_matrix)
# 
# head(exact_df)
# nodeset[unique(exact_df[grep(18800, exact_df$DocNum),]$SDFB_ID),]
# nodeset[unique(exact_df[grep(25200, exact_df$DocNum),]$SDFB_ID),]
