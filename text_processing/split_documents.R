# Takes a folder of docs and splits them into approx equal 500 word chunks
# Minimal split size is 500, so 999 -> one doc, 1000 -> two 500 docs, etc..

library(parallel)
library(stringr)

split_odnb_docs <- function(input_folder="/data/ODNB/ODNB-1450-1700", output_folder="/data/ODNB/splits/") {
  

  files = list.files(path=input_folder, full.names=T, recursive=FALSE)

  # set results to temp to suppress output of empty list
  temp = mclapply(files, function(file) {

    cat("file = ", file, fill=T)

    lines = readLines(file) # The ODNB docs are only ever 1 line
    
    words = unlist(strsplit(lines, " "))
    parts = str_split(file, "/", simplify=T)
    name = parts[,ncol(parts)]
    
    word_count = length(words)
    
    num_docs = word_count %/% 500
    
    chunk_size = word_count %/% num_docs
    
    if(num_docs > 1) {
      
      for(i in 1:num_docs) {
        start_idx = (i - 1) * chunk_size
        end_idx = start_idx + chunk_size
        chunk = words[start_idx:end_idx]
        writeLines(text = chunk, con=paste0(output_folder, name, "-", i), sep=" ")
      }
      
    } else {
      writeLines(text = words, con=paste0(output_folder, name), sep=" ")
    }
    
  })  
}

split_jstor_docs <- function(input_folder="/data/JSTOR/bundle", output_folder="/data/JSTOR/splits/") {
  
  
  files = list.files(path=input_folder, full.names=T, recursive=FALSE)

  # set results to temp to suppress output of empty list
  temp = mclapply(files, function(file) {

    cat("file = ", file, fill=T)
 
    lines = readLines(file) # The ODNB docs are only ever 1 line
    
    # Get only the content in the <pages> section
    keep_lines = 
    words = unlist(strsplit(lines, " "))
    words = words[nzchar(x=words)]
    
    parts = str_split(file, "/", simplify=T)
    name = parts[,ncol(parts)]
    
    word_count = length(words)
    
    num_docs = word_count %/% 500
    
    chunk_size = word_count %/% num_docs
    
    if(num_docs > 1) {
      
      for(i in 1:num_docs) {
        start_idx = (i - 1) * chunk_size
        end_idx = start_idx + chunk_size
        chunk = words[start_idx:end_idx]
        writeLines(text = chunk, con=paste0(output_folder, name, "-", i), sep=" ")
      }
      
    } else {
      writeLines(text = words, con=paste0(output_folder, name), sep=" ")
    }
    
  })  
}

split_odnb_docs(input_folder="/pylon2/hm4s82p/walling/data/odnb_modern/docs", output_folder="/pylon2/hm4s82p/walling/data/odnb_modern/splits/")
