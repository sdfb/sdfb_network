#@S This file contains some general simple helper functions used in various situations
#@S   when producing text output
#@L See individual function documentation


output_log = function(message, file_out, verbose = TRUE) {
  #@F ----------------------------------------
  #@F Function 'output_log'
  #@F Args (Input): message, verbose
  #@F   message = singleton character
  #@F   verbose = TRUE if want to display to screen; FALSE => only log in file
  #@F   file_out = filename to output text to
  #@F Purpose: This logs the specific message in zGEN_plotlog.txt, as well as
  #@F   to the screen, if so desired (in verbose)
  #@F Output: None
  #@F ----------------------------------------
  
  cat(message,'\n', file = file_out, append = TRUE)
  if (verbose) {
    print(message)
  }
  return(NULL)
}

fix_length = function(t, len, at_end = TRUE) {  
  #@F ----------------------------------------
  #@F Function 'fix_length'
  #@F Args (Input): t = character vector
  #@F   len = target length
  #@F   at_end = logical; modify entries at the beginning or at the end?
  #@F Purpose: For all elements of 't', its number of characters is adjusted to be 
  #@F   'len' either by removing characters or adding spaces, based on 'at_end'
  #@F Output: character singleton
  #@F ----------------------------------------

  fix_length_single = function(t_single, len, at_end = TRUE) {
    # This works for a single character singleton t
    t_len = nchar(t_single)
    if (t_len < len) {
      if (at_end) {
        return(paste(c(t_single, 
                       rep(" ", times = len - t_len)), 
                     collapse = "") )
      } else {
        return(paste(c(rep(" ", times = len - t_len), 
                       t_single), 
                     collapse = "") )
      }
    } else if (t_len > len) {
      if (at_end) {
        return(substr(t_single, start = 1, stop = len))
      } else {
        return(substr(t_single, start = t_len - len + 1, stop = t_len))
      }
    } else {
      return(as.character(t_single))
    }
  }
  return(vapply(t, FUN = fix_length_single, FUN.VALUE = 'character', len = len, at_end = at_end))
}

create_space_chars = function(v, ch = " ") {
  #@F ----------------------------------------
  #@F Function 'create_space_chars'
  #@F Args (Input): v = numeric vector, ch = character. length 1. 
  #@F Purpose: Creates a character vector of a varying amount of 'ch' characters; this is 
  #@F   mainly used to space out text for pretty displaying (in printing dataframes)
  #@F Example: if v = c(5,1), returns c("     ", " ")
  #@F Output: character vector, length(v)
  #@F ----------------------------------------
  
  toreturn = as.character(v)
  for (j in 1:length(v)) {
    toreturn[j] = paste(rep(ch, times = v[j]), collapse = "")
  }
  return(toreturn)
}

output_list = function(L, file_out, recurse_num = 0) {
  #@F ----------------------------------------
  #@F Function 'output_list'
  #@F Args (Input): L: list, recurse_num: integer, file_out = filename
  #@F Purpose: This outputs (in a relatively 'pretty' format) the content of list L to 
  #@F   'file_out' 
  #@F Output: None
  #@F Notes: This function recursively calls itself (with recurse_num incremented) for 
  #@F   the purpose of nicer printing
  #@F ----------------------------------------
  
  
  for (j in 1:length(L)) {
    Lsub = L[[j]] # This is the current list entry that is being processed/output... substitution is only for clarity
    
    header = rep('---', times = recurse_num) 
    cat(header,' $', names(L)[j], '\n',sep = "", file = file_out, append = TRUE)
    if ( is.vector(Lsub, mode = "numeric") | is.vector(Lsub, mode = "character") | is.vector(Lsub, mode = "logical")) {
      if (length(Lsub) > 20) {
        cat(header, " ___[", paste(c(Lsub[1:20],"<<ETC...>>"), collapse = "::") ,"]\n", sep = "", file = file_out, append = TRUE)
      } else {
        cat(header, " ___[", paste(Lsub, collapse = "::") ,"]\n", sep = "", file = file_out, append = TRUE)
      }
      
    } else if (is.data.frame(Lsub)) { 
      all_widths = rbind(nchar(as.matrix(Lsub)), nchar(colnames(Lsub)))
      use_widths = apply(all_widths, 2, max) + 1
      cat(header, " -------------------- \n", sep = "", file = file_out, append = TRUE)
      
      temp = paste(create_space_chars(use_widths - nchar(colnames(Lsub))), colnames(Lsub), sep = "", collapse = " :")
      cat(header, temp,"\n", sep = "", file = file_out, append = TRUE)      
      for (k in 1:nrow(Lsub)) {
        temp = paste(create_space_chars(use_widths - nchar(Lsub[k,])), Lsub[k,], sep = "", collapse = " :")
        cat(header, temp,"\n", sep = "", file = file_out, append = TRUE)      
      }
    } else if (is.list(Lsub)) {
      output_list(Lsub, file_out = file_out, recurse_num = recurse_num + 1)
    } else {
      cat(header,'This case not written\n',sep = "", file = file_out, append = TRUE)
    }
    if (recurse_num == 0) {
      cat("========================================\n", file = file_out, append = TRUE)
    }
  }
  return(NULL)
}


