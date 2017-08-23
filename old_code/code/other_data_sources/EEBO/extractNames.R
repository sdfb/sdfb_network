#@S Attempting to extract the network from EEBO. 
#@S This extracts the names from the various columns

# Load data: 
# full_data is a data frame with 120k rows, with columns: 
#   Author, Title, Year, Imprint
load("existing_networks/EEBO/subsetData.Rdata")

source("general_help_functions/HTMLProcessing.R")

## Processing: 
# Fix spacing
full_data$Imprint = compress_spaces(full_data$Imprint)

find_special_characters(full_data$Imprint)
# Need to substitute &#039; and &amp;
full_data$Imprint = gsub("&#039;", "'", full_data$Imprint)
full_data$Imprint = gsub("&amp;", "&", full_data$Imprint)
full_data$Author = gsub("&#039;", "'", full_data$Author)

# Check if any instances of name separation by commas only exists?
test = gregexpr("([A-Z][[:alpha:].-]+ *)+ *, *([A-Z][[:alpha:].-]+ *)+", full_data$Imprint)
test2 = regmatches(full_data$Imprint, test)
# A fair number exists


# Look for s.n. -> 20629 lines
empty_lines = grep("s[.]n[.]", full_data$Imprint)
full_data$Imprint[empty_lines]

# head(full_data)
# full_data$Imprint[nchar(full_data$Imprint) == 20]
# full_data$Imprint[50:70] 
# test = sapply(gregexpr(":", full_data$Imprint), function(x) { sum(x > 0) })

# a=grep("(p|P)rinted by", full_data$Imprint)
# b=grep("(p|P)rinted for", full_data$Imprint)
# full_data$Imprint[-union(a,b)]
# length(intersect(a,b))
# full_data$Imprint[a][1:50]

source("general_help_functions/textNameExtraction.R")

prioritized_rules = rbind(
  # 1. Standard "Printed by and for [...]"
  create_rule_entry(prefix_regex = "(p|P)rinted by and for ", types = c("p_by", "p_for")),
  # 2. Standard "Printed by [...]"
  create_rule_entry(prefix_regex = "(p|P)rinted by ", types = "p_by"),
  # 3. Standard "Printed for [...]"
  create_rule_entry(prefix_regex = "(p|P)rinted for ", types = "p_for"),
  # 4. Standard "Printed by ... for [...]"
  create_rule_entry(
    prefix_regex = "(p|P)rinted by (([A-Z][[:alpha:].-]+ *)|( and )|( & ))*,* *for ", 
    types = "p_for"),
  # 5. Standard "Printed, and (are to be) sold by [...]"
  create_rule_entry(
    prefix_regex = "(p|P)rinted,* and,* (are to be)* sold by ", 
    types = c("p_by", "s_by")),
  # 6. Standard "Printed for and are to be sold by [...]"
  create_rule_entry(
    prefix_regex = "(p|P)rinted,* for,* and,* (are to be)* sold by ", 
    types = c("p_for", "s_by")),
  # 7. Remaining "sold by [...]"
  create_rule_entry(
    prefix_regex = "(S|s)old by ", 
    types = "s_by"),
  # 8. By and for [...]"
  create_rule_entry(
    prefix_regex = "(B|b)y and for ", 
    types = c("p_by", "p_for")),
  # 9. By [...]
  create_rule_entry(
    prefix_regex = "(B|b)y ", 
    types = "p_by"),
  # 10. For [...]
  create_rule_entry(
    prefix_regex = "(F|f)or ", 
    types = "p_for")
)

# test = compare_rules(rules = prioritized_rules,text = full_data$Imprint,
#                      compare_rows = c(1,2,3,4,5))
# 
# mismatch = which(nchar(test[,5]) > nchar(test[,2]) + 2)
# test[mismatch,c(2,5)] -> a
# write.csv(a, file = "test2.csv")

imprint_matches = apply_prioritized_rules(
  rules = prioritized_rules,
  text = full_data$Imprint,
  types = c("p_by", "p_for", "s_by"))

write.csv(
  cbind(full_data$Imprint, imprint_matches), 
  file = "test.out.csv")

head(imprint_matches)
sum(apply(imprint_matches[,4:6], 1, sum) > 0)
# 85603 rows with extracted names

out = cbind(full_data, imprint_matches)
to.show = sample(1:nrow(out), size = 5000)

write.csv(out[to.show,], file = "new_res.csv")
save(out, file = "existing_networks/EEBO/EEBO_results.Rdata")

