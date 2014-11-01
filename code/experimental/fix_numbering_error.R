

text = rep(NA, times = 199999)

for(j in 1:length(cosub_list)) {
  if (j %% 1000 == 0) { cat(j, " ") }
  
  if (length(cosub_list[[j]]) > 100) {
  } else if (length(cosub_list[[j]]) > 1) {
    ## test for common name
    
    cos_res = process_cosubject(ids = cosub_list[[j]])
#|----##Changing parameterization --Fri Oct 31 15:49:10 2014--
    
    for(k in seq_along(cos_res$text)) {
      if (!is.na(text[k])) {
        ## For whatever reason, some people share ID nums (cosubjects). I'm adding 100,000 to the id number for these... 
        text[cos_res$ids[k] + 100000] <- cos_res$ids[k]
      } else {
        text[cos_res$ids[k]] <- cos_res$ids[k]
      }
    }
  } else {
    text[j] = cosub_list[[j]][1]
  }
}


