#@S Code to plot number of alive persons from ODNB database at any year. 

# TODO: Remove this code; use new date processing.

## Old code, to toss eventually
# TODO: Toss this code when ready.
# 
# load("odnbdata.Rdata")
# load("curnums.Rdata")
# load("count.list.Rdata")
# 
# lenss = sapply(count.list, length)
# 
# nums.touse = which(lenss == 3) #these are between 1, 99940)
# 
# dates.char = rep("", times = length(nums.touse))
# 
# for (j in 1:length(nums.touse)) {
#   dates.char[j] = ODNB.data[[which(ODNB.nums == nums.touse[j])]]$dates
#   print(j)
# }

dates.matrix = matrix(NA, nrow = length(nums.touse), ncol = 2)

type1 = grep("^[[:digit:]]{4}-[[:digit:]]{4}$", dates.char)
for(j in 1:length(type1)) {
 W = strsplit(dates.char[type1[j]], "-")[[1]]
 dates.matrix[type1[j],] = as.numeric(W)
 if (j %% 100 == 1) { print(j) }
}

rem1 = dates.char
rem1[type1] = ""
no.date = setdiff(1:length(rem1), grep("[[:digit:]]", rem1))
rem1[no.date] = ""

rem2 = gsub("<em>c.</em>", "", rem1)
rem2 = gsub("<em>fl. </em>", "", rem2)
rem2 = gsub("\\?", "", rem2)
grep("/[[:digit:]]+[-]", rem2)
rem2 = gsub("/[[:digit:]]+", "", rem2)

type2 = grep("^[[:digit:]]{4}-[[:digit:]]{4}$", rem2)
for(j in 1:length(type2)) {
 W = strsplit(rem2[type2[j]], "-")[[1]]
 dates.matrix[type2[j],] = as.numeric(W)
 if (j %% 100 == 1) { print(j) }
}

rem3 = rem2
rem3[type2] = ""
head(rem3[which(rem3 != "")], 150)
rem3 = gsub("<em>bap. </em>", "", rem3)
rem3 = gsub("<em>d.</em>", "", rem3)
rem3 = gsub("<em>b. </em>", "", rem3)
rem3 = gsub(" ", "", rem3)

type3 = grep("^[[:digit:]]{4},[[:digit:]]{4}$", rem3)
for(j in 1:length(type3)) {
 W = strsplit(rem3[type3[j]], ",")[[1]]
 dates.matrix[type3[j],] = as.numeric(W)
 if (j %% 100 == 1) { print(j) }
}

years = 1000:2000
ct = rep(0, times = length(years))

dates.matrix.n = dates.matrix[!is.na(dates.matrix[,1]),]
for(k in 1:length(years)) {
 ct[k] = sum(dates.matrix.n[,1] <= years[k] & years[k] <= dates.matrix.n[,2])
 print(k)
}

plot(years, ct, xlab = "Year", ylab = "Frequency", main = "Number of biographies
 about someone alive at certain Year", type = "l")

abline(v = 1642, col = 2)
abline(v = 1914, col = 2)
abline(v = 1918, col = 3)
abline(v = 1939, col = 3)

save(years,ct, file = "datesproc.Rdata")

load("datesproc.Rdata")
