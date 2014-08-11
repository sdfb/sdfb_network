#@S This file contains some general simple helper functions used in various settings of HTML processing
#@L See individual function documentation

find_special_characters = function(x) {
  #@F ----------------------------------------
  #@F Function 'find_special_characters'
  #@F Args (Input): x = character vector
  #@F Purpose: find all examples of &[..]; in text 'x'
  #@F Output: table (named numeric vector) of HTML special codes (of form &[..];)
  #@F ----------------------------------------
  
  matches = regmatches(x, gregexpr("&[^ ]+?;", x))
  matches = c(matches, recursive = TRUE)
  return(table(matches))
}

strip_spaces = function(x) {
  #@F ----------------------------------------
  #@F Function 'strip_spaces'
  #@F Args (Input): x = character vector
  #@F Purpose: removes leading and trailing spaces
  #@F Output: character vector
  #@F ----------------------------------------
  
  x = gsub("^ *", "", x)
  x = gsub(" *$", "", x)
  x = gsub(" +", " ", x)
  return(x)
}

compress_spaces = function(x) {
  #@F ----------------------------------------
  #@F Function 'compress_spaces'
  #@F Args (Input): x = character vector
  #@F Purpose: removes multiple spaces in a row: compresses to single space; 
  #@F   Also removes leading/trailing spaces
  #@F Output: character vector
  #@F Notes: Dependencies: strip_spaces
  #@F ----------------------------------------
  
  x = strip_spaces(x)
  return(gsub(" +", " ", x))
}
