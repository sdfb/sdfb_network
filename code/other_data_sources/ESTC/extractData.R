##@S Extracting pertinent parts of the data

Sys.setlocale('LC_ALL','C') ## This fixes the problem..

load("private_data/estc_data_proc/allData_df.Rdata")

names(full_data)
source("general_help_functions/HTMLProcessing.R")

find_special_characters(full_data$Author) ## TODO: This errors. fix it? something about multibyte strings

nchar(full_data$Author[1065])

z = grep("John", full_data$Author)

head(full_data$Author, 2000)


# Check if any instances of name separation by commas only exists?
test = gregexpr("([A-Z][[:alpha:].-]+ *)+ *, *([A-Z][[:alpha:].-]+ *)+", full_data$Pub)
test2 = regmatches(full_data$Pub, test)

# Look for s.n. -> 31k
empty_lines = grep("s[.]n[.]", full_data$Pub)

###################################


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
  text = full_data$Pub,
  types = c("p_by", "p_for", "s_by"))





write.csv(
  cbind(full_data$Imprint, imprint_matches), 
  file = "test.out.csv")

head(imprint_matches)
sum(apply(imprint_matches[,4:6], 1, sum) > 0)
# 300k rows with extracted names

out = cbind(full_data, imprint_matches)
to.show = sample(1:nrow(out), size = 5000)

write.csv(out[to.show,], file = "new_res.csv")
save(full_data, imprint_matches, out, file = "existing_networks/ESTC/ESTC_results.Rdata")

load("existing_networks/ESTC/ESTC_results.Rdata")
pby_list = strsplit(imprint_matches$p_by, "(##|,)")
pfor_list = strsplit(imprint_matches$p_for, "(##|,)")

remove_na_blank = function(x) {
  x = x[!is.na(x)]
  x = x[x != ""]
  return(x)
}

extract_names_j = function(j) {
  a = strip_spaces(pby_list[[j]])
  b = strip_spaces(pfor_list[[j]])
  return(remove_na_blank(c(as.character(full_data$Author[j]), a, b)))
}

names_lists = list()
for(j in 1:nrow(full_data)) {
  if (j %% 10000 == 0) { print(j) }
  names_lists[[j]] = extract_names_j(j)
}
save(names_lists, file = "existing_networks/ESTC/proc_names.Rdata")

allnames = c(names_lists, recursive = TRUE)
length(unique(allnames))
head(unique(allnames))
tab_names = sort(table(allnames))
dat = cbind(Name=names(tab_names), Count=tab_names)
write.csv(dat, file = "etsc_counts.csv")

adj_mat = matrix(0, nrow = length(tab_names), ncol = length(tab_names))

names = sort(unique(allnames))
for(j in 1:length(names_lists)) {
  coords = match(names_lists[[j]], names)
  adj_mat[coords,coords] = adj_mat[coords,coords] + 1
  if (j %% 1000 == 0) {print(j)}
}

save(names, adj_mat, dat, file = "existing_networks/ESTC/adjmat.Rdata")
