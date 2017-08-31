# Convert json version of document counts to a matrix that can be used by network estimation code
library(jsonlite)
library(Laurae)
library(data.table)
library(parallel)


json.data = read_json("data/dataset2.json")

# Each entry in the json represents a document
# Each document contains a list of entities in that document
doc.ids = names(json.data)
data.tables.list <- mclapply(1:10, function(idx) {
  doc = doc.ids[[idx]]
  persons = json.data[[idx]]
  data = data.table(do.call(cbind, persons))
  row.names(data) = doc
  return(data)
})

data = rbindlist(data.tables.list, fill=TRUE)