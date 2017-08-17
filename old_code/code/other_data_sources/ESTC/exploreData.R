#@S Exploring the dataset

# test = readLines("data/estc_data/Duhaime20130716.mrc", n = 5)

# Try this tool
#  http://rossjohnson.homemail.com.au/MARCRTP/
#
# -> installed in software/marc... 
# explore data (summary) 
## marc -s C:\Users\Lawrence\Desktop\gitRepositories\SDFB\private_data\estc_data\Duhaime20130716.mrc summary.tab
#
# download pertinent fields
# Fields I want:
### 100-a = author name
### 100-b = numeration for name
### 100-c = titles
### 100-d = dates for name
### 245-a = main title
### 245-b = remainder of title
### 260-b = name of publisher,distributor, etc. [avoid indcators 2,3?]
### 008 = Date (in certain format)

# all seem to be non-repeating

# marc request file is called main_request.rqt
# Command:
## For ubuntu
## cd software/marc/mrtp-1.4.9
## ./marc ../main_request.rqt ../../../private_data/estc_data/Duhaime20130716.mrc .out
## mv allData.out ../../../private_data/estc_data_proc/

## for windows
## marc C:\Users\Lawrence\Desktop\gitRepositories\SDFB\software\marc\main_request.rqt C:\Users\Lawrence\Desktop\gitRepositories\SDFB\private_data\estc_data\Duhaime20130716.mrc .out
#

full_data = read.delim("private_data/estc_data_proc/allData.out", stringsAsFactors = FALSE, header = FALSE)

b = readLines("private_data/estc_data_proc/allData.out")
b1 = strsplit(b, "\t")
full_data = data.frame(a=sapply(1:length(b1), function(x){b1[[x]][1]}))
for(j in 2:8) {
  print(j)
  full_data = cbind(full_data, sapply(1:length(b1), function(x){b1[[x]][j]}))
}

for (c in 1:ncol(full_data)) {
  full_data[full_data[,c] == "!!NONE",c] = NA
  print(c)
}
colnames(full_data) = c("Title", "Title2", "Author", "Author_RomanNum", "Author_Title",
                        "Author_Dates", "Publisher", "Date")
save(full_data, file = "private_data/estc_data_proc/allData_df.Rdata")


# TODO: Document this file
