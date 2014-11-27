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
