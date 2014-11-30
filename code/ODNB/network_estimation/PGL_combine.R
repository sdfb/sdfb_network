files = list.files("data_manual/PGL_fit/", full.names = TRUE)
files = files[grep(files, pattern = "SS\\_")]
length(files)

library(Matrix)

load(files[1])
res_matrix = Matrix(0, sparse = TRUE, nrow = nrow(lambda_matrix), ncol = ncol(lambda_matrix))
for(j in seq_along(files)) {
  print(j)
  load(files[j])
  res_matrix = res_matrix + (lambda_matrix + t(lambda_matrix) > 0)
}
res_matrix = round(res_matrix / length(files),2)

table(res_matrix[12935,])
sum(res_matrix)

load("data_manual/ODNB_dataset.Rdata")

nodeset[grep("Harrington", nodeset$search_names_ALL),]

k = 8309 # Milton
k = 5566 # Harrington
sum(res_matrix[k,] == res_matrix[,k])
table(res_matrix[k,])
z = which(res_matrix[k,] > 0)
z = z[order(res_matrix[k,z])]
cbind(nodeset$full_name[z], nodeset$full_date[z], res_matrix[k,z])


is_search_date_adj = function(nodeset_row) {
  birth = switch(gsub("/", "", nodeset_row$AF.BF.CA.IN), AF = F, AFIN = F, BF = T, BFIN = T, CA = T, IN = F)
  death = switch(gsub("/", "", nodeset_row$AF.BF.CA.IN2), AF = T, AFIN = T, BF = F, BFIN = F, CA = T, IN = F)
  res = try(birth | death, silent = TRUE)
  if (class(res) == "try-error") { res = T }
  return(res)
}
adj_searchdate = rep(NA, times = nrow(nodeset))
for(j in seq_len(nrow(nodeset))) {
  adj_searchdate[j] = is_search_date_adj(nodeset[j,])
}
table(adj_searchdate)

final_res = NULL
for(j in seq_len(nrow(nodeset))) {
  keeprows = intersect(which(res_matrix[j,] > 0.1), j:nrow(nodeset))
  if (length(keeprows) > 0) {
    temp = data.frame(ID1 = j, ID2 = keeprows, ConfEst = res_matrix[j,keeprows], Edgetype = adj_searchdate[j] + adj_searchdate[keeprows])
    final_res = rbind(final_res, temp)
  }
  print(j)
}

write.csv(final_res, row.names = FALSE, file = "conf_matrix_20141129.csv")


