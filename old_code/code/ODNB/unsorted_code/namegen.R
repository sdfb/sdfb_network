name.list = list() initialize only
for (j in 1:58265) {
  test = process.name(name = ODNB.names[j], type = name.normal[j])
  if (!is.null(test)) {
    name.list[[ODNB.nums[j]]] = test
  }
  if(j %% 250 == 0) {
    print(j)
  }
}

save(name.list, file = "nameslist.Rdata")
