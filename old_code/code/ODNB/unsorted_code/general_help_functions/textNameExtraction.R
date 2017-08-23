#@S Help functions for extracting names from text
#@L See individual function documentation
#@L In general, these functions are intended to extract names from text; some modification
#@L   might be needed in order to apply in more general context. This framework allows for
#@L   collecting different 'types' of names (names extracted from different contexts). 
#@L   The idea is to create a number of regular expression rules to extract names with, 
#@L   (these are stored in some data frame), and then run the functions that apply these
#@L   rules in order of priority (ie some sentences can have multiple rules that extract
#@L   names), and generate a data frame with name matches per sentence. 
 
extract_matches = function(text,
                           prefix_regex = "", 
                           main_regex = "(([A-Z][[:alpha:].-]+ *)|( and )|( & ))+", 
                           suffix_regex = "", 
                           and_regex = " (and)|(&) ", and_replacement = " ## ",
                           ignore_regex = "(\\]|\\[)") {
  #@F ----------------------------------------
  #@F Function 'extract_matches'
  #@F Args (Input): 
  #@F   text = character vector; text to be processed
  #@F   main_regex = main pattern to search text for. match for this is returned
  #@F   prefix_regex, suffix_regex = regex expressions to prepend / append, these are 
  #@F     intended in the situation that there is specific context to find 'main_regex' in
  #@F   and_regex = regular expression to find 'and' segments
  #@F   and_replacement = text to replace and segments with, in output
  #@F   ignore_regex = this text will be substituted out before searching
  #@F Purpose: The main function for name extraction: prefix/suffix provide context for
  #@F   names in this specific dataset, so will vary. Default main_regex looks for 
  #@F   capital words, with few allowable punctuation, or 'and' words. Perhaps this could
  #@F   be changed a bit (allow for 'of'? etc.). It will then replace 'and' words/characters
  #@F   and output the results (only the portions that match main_regex, not portions
  #@F   that match prefix/suffix)
  #@F Output: character vector, same length as text, of matching parts (hopefully names)
  #@F ----------------------------------------
  
  full_regex = paste(prefix_regex, main_regex, suffix_regex, sep = "")
  t = gsub(ignore_regex, "", text)
  
  r_out = gregexpr(full_regex, t)
  r_mat = regmatches(t, r_out)
  res = r_mat
  
  if (prefix_regex != "") {
    res = sapply(res, function(x) {gsub(paste("^",prefix_regex, sep = ""), "", x)})
  }
  if (suffix_regex != "") {
    res = sapply(res, function(x) {gsub(paste(suffix_regex, "$", sep = ""), "", x)})
  }
  
  if (and_regex != "") {
    res = sapply(res, function(x) {gsub(and_regex, and_replacement, x)})
  }
  res = sapply(res, function(x) {paste(x, collapse = and_replacement)})
  
  return(res)
}

replace_empty_entries = function(DF, col, replacement_col) {
  #@F ----------------------------------------
  #@F Function 'replace_empty_entries'
  #@F Args (Input): 
  #@F   DF = target data frame
  #@F   col = column number, or column name, of column with entries to be replaced
  #@F     needs to be singleton entry (col number or col name)
  #@F   replacement_col = column of values to replace DF[,col] with. 
  #@F Purpose: replaces empty entries of a specific column. Only those column entries 
  #@F   of column 'col', in data frame DF, who have
  #@F   currently a <0> or a <""> entry (depending on numeric vs. character valued) will
  #@F   be replaced with values from replacement_col. 
  #@F Output: data frame DF, with some entries replaced
  #@F ----------------------------------------
  
  if(is.character(col)) {
    col = which(colnames(DF) == col)
  }
  
  if(is.character(DF[,col])) {
    rep_rows = which(DF[,col] == "")
    res = DF
    res[rep_rows,col] = replacement_col[rep_rows]
  } else if (is.numeric(DF[,col])) {
    rep_rows = which(DF[,col] == 0)
    res = DF
    res[rep_rows,col] = replacement_col[rep_rows]
  } else {
    print("Unhandled case in 'replace_empty_results' (DF column type not numeric/character")
    print(" Thus, no changes made. ")
    res = DF
  }
  return(res)
}



create_rule_entry = function(prefix_regex = "", 
                             main_regex = "(([A-Z][[:alpha:].-]+ *)|( and )|( & )|( *, *))+",
                             suffix_regex = "", 
                             and_regex = " (and)|(&) ", and_replacement = " ## ",
                             ignore_regex = "(\\]|\\[)",
                             types, all_types = c("p_by", "p_for", "s_by")) {
  #@F ----------------------------------------
  #@F Function 'create_rule_entry'
  #@F Args (Input): 
  #@F   types = specific types of output that this rule generates
  #@F   all_types = all possible types of output for 'rules'
  #@F   remaining arugments: see extract_matches documentation
  #@F Purpose: Creates rule formats for running extract_matches. This is used to 
  #@F   create a data frame of rules, that will all need extract_matches run at once. 
  #@F Output: Data frame, with one row
  #@F ----------------------------------------
  
  inds = t(as.numeric(!is.na(match(all_types, types))))
  ret = data.frame(prefix_regex = prefix_regex, main_regex = main_regex,
                   suffix_regex = suffix_regex, and_regex = and_regex,
                   and_replacement = and_replacement, 
                   ignore_regex = ignore_regex, inds, 
                   stringsAsFactors = FALSE)
  colnames(ret)[(ncol(ret) - length(inds) + 1):ncol(ret)] = all_types
  return(ret)
}

compare_rules = function(rules, text, compare_rows) {
  #@F ----------------------------------------
  #@F Function 'compare_rules'
  #@F Args (Input):
  #@F   rules = rules data frame (rbind output of create_rule_entry)
  #@F   text = text to search names from
  #@F   compare_rows = which rows of rules data frame to test out
  #@F Purpose: collect matches from each of the various rules
  #@F Output: matrix of matches from each rule. 
  #@F ----------------------------------------
  
  to_return = NULL
  for (j in compare_rows) {
    cat("Applying rule number", j, "::", date() ,"\n")
    rule_result = extract_matches(text = text, 
                                  prefix_regex = rules$prefix_regex[j],
                                  main_regex = rules$main_regex[j],
                                  suffix_regex = rules$suffix_regex[j],
                                  and_regex = rules$and_regex[j],
                                  and_replacement = rules$and_replacement[j],
                                  ignore_regex = rules$ignore_regex[j])
    to_return = cbind(to_return, rule_result)
  }
  colnames(to_return) = as.character(compare_rows)
  return(to_return)
}

apply_prioritized_rules = function(rules, text, types) {
  #@F ----------------------------------------
  #@F Function 'apply_prioritized_rules'
  #@F Args (Input):
  #@F   rules = rules data frame (rbind output of create_rule_entry)
  #@F   text = text to search names from
  #@F   types = all the different possible types found in rules
  #@F Purpose: extract matches using all of the rules in 'rules'
  #@F Output: data frame of extracted names (and columns indicating which rule
  #@F   generated this specific name)
  #@F ----------------------------------------
  
  left_df = matrix("", nrow = length(text), ncol = length(types))
  right_df = matrix(0, nrow = length(text), ncol = length(types))
  res_df = data.frame(left_df, right_df, stringsAsFactors = FALSE)
  colnames(res_df) = c(types, paste("RULENO_",types,sep = ""))
  
  rules_type_cols = match(types, colnames(rules))
  if (any(is.na(rules_type_cols))) {
    stop("Some type is inconsistent")
  }
  
  for(j in 1:nrow(rules)) {
    cat("Applying rule number", j, "::", date() ,"\n")
    rule_result = extract_matches(text = text, 
                                  prefix_regex = rules$prefix_regex[j],
                                  main_regex = rules$main_regex[j],
                                  suffix_regex = rules$suffix_regex[j],
                                  and_regex = rules$and_regex[j],
                                  and_replacement = rules$and_replacement[j],
                                  ignore_regex = rules$ignore_regex[j])
    
    for(k in which(rules[j,rules_type_cols] == 1)) {
      res_df = replace_empty_entries(DF = res_df,
                                     col = k,
                                     replacement_col = rule_result)
      res_df = replace_empty_entries(DF = res_df,
                                     col = length(types) + k,
                                     replacement_col = j * as.numeric(rule_result != ""))
    }
  }
  
  return(res_df)    
}



