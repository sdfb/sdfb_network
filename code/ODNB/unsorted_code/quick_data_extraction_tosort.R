load("new_fit_confidence_matrix.Rdata")
load("../private_data/odnb_data_proc/ODNB_fullnamelist.Rdata")

head(full_result3)
grep("Milton", full_result3$main_name)
colnames(conf_matrix)
load("../private_data/odnb_data_proc/ODNB_fincounts.Rdata")
ls()
dim(f_count_mat)
fit_ids = as.numeric(colnames(f_count_mat))


y = grep("Marvell", full_result3$main_name)
w = y[which(!is.na(match(full_result3$ID[y], fit_ids)))]
full_result3$main_name[w]
full_result3[w,]
y[6]
which(fit_ids == full_result3$ID[15796])
which(fit_ids == 10017061)


person = 3823
inds = which(conf_matrix[person,] > 0)
rs = match(fit_ids[inds], full_result3$ID)
data.frame(conf = as.numeric(conf_matrix[person,inds]), full_result3$main_name[rs], full_result3$full_date[rs], stringsAsFactors = FALSE) -> df

write.csv(df[order(df[,1]),], file = "f3.csv", row.names = FALSE)





which(conf_matrix[3934,] > 0.5)




order(df)





sapply(inds, function(x) {full_result3$main_name[which(full_result3$ID == fit_ids[x])]})])


res_df = data.frame(ID1 = 0, ID2 = 0, Conf = 0)
for (i in 1:nrow(conf_matrix)) {
  print(i) 
  z = which(conf_matrix[i,] >= 0.3)
  z = z[z > i]
  if (length(z) > 0) {
  y = fit_ids[i]
  x = fit_ids[z]
  res_df = rbind(res_df, data.frame(ID1 = y, ID2 = x, Conf = conf_matrix[i,z]))
}
}



write.csv(fit_ids, file = "all_ids.csv", row.names = FALSE, col.names = FALSE)
write.csv(











