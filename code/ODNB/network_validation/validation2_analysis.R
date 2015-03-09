# Code that examines and plots the validation results

# Load pertinent data, do light data-cleanup ------------------------------

## Load Data
jh_links = read.csv("data_vc/validation2/datajh.csv", header = TRUE, stringsAsFactors = FALSE)[,c(1,2,3)]
jm_links = read.csv("data_vc/validation2/datajm.csv", header = TRUE, stringsAsFactors = FALSE)[,c(1,2,3,4)]
jm_links = jm_links[which(jm_links[,1] == "John Milton"), c(2,3,4)]
jm_links[,3] = as.numeric(jm_links[,3])
raw_ranks = read.csv("data_vc/validation2/rank_raw.csv", header = TRUE, stringsAsFactors = FALSE)

colnames(jh_links) = c("Name", "ID", "Ranking")
colnames(jm_links) = c("Name", "ID", "Ranking")


## Re-arrange names to ensure same row identification
jh_ranks = raw_ranks[which(raw_ranks$Main.Person == "James Harrington"), ]
jh_ranks = jh_ranks[match(jh_links$Name, jh_ranks$Link), ]
nrow(jh_ranks) == sum(jh_ranks$Link == jh_links$Name) # check that names match up

jm_ranks = raw_ranks[which(raw_ranks$Main.Person == "John Milton"), ]
jm_ranks = jm_ranks[match(jm_links$Name, jm_ranks$Link), ]
nrow(jm_ranks) == sum(jm_ranks$Link == jm_links$Name) # check that names match up

## Change method values into a ranking, where larger numbers are better. 
for(c in 4:27) {
  jm_ranks[,c] = rank(jm_ranks[,c], na.last = "keep", ties.method = "average")
  jh_ranks[,c] = rank(jh_ranks[,c], na.last = "keep", ties.method = "average")
}

jm_links = cbind(jm_links, Ranked = rank((101 - jm_links$Ranking), na.last = "keep", ties.method = "average"))
jh_links = cbind(jh_links, Ranked = rank((101 - jh_links$Ranking), na.last = "keep", ties.method = "average"))

## Compute Spearman correlations -- basically, look at how consistent two rankings are. 
spear_cors = matrix(0, nrow = 24, ncol = 2)
 
for(j in 4:24) { 
  spear_cors[j,1] = cor(jm_links$Ranked, jm_ranks[,j], use = "pairwise.complete.obs", method = "spearman")
  spear_cors[j,2] = cor(jh_links$Ranked, jh_ranks[,j], use = "pairwise.complete.obs", method = "spearman")
}


# Function that returns the indices (in order) of the top k matches. (Larger = better ranked)
topk = function(dat, k) {
  k = min(k, sum(!is.na(dat)))
  return(order(dat,decreasing= TRUE)[1:k])
}

# Function that counts the number of matches of a certain type, vs. when they occur (top 10, top 20, etc.)
count_matches = function(est_ranks, orig_ranks, cuts_s = c(0, 20, 50, 80, 100), cuts_e = c(19, 49, 79, 99, 500)) {
  # est_ranks = rank vector whose match to the correct rankings which should be computed
  # orig_ranks = correct ranking vector
  # cuts_s = vector of starting points of cuts
  # cuts_e = vector of ending points of cuts
  # The best ranking would be all orig_ranks falling inside cuts_s[1] <= x <= cuts_e[1]
  
  # Returns a matrix of dimension: nrows = length(est_ranks), ncol = length(cuts_s) + 1
  # The matrix tells how many matches of the different types have occured up to the kth observation (and this information is stored in row k)
  #   The last column in the matrix tells how many NA rankings have been estimated up to the kth row. 
  
  resmatrix = matrix(0, nrow = sum(!is.na(est_ranks)), ncol = length(cuts_s) + 1)
  for(k in seq_len(sum(!is.na(est_ranks)))) {
    topk_ranked = topk(est_ranks, k = k)
    actual_ranks = orig_ranks[topk_ranked]
    
    ## Do counting
    for(c in 1:length(cuts_s)) {
      resmatrix[k,c] = sum(actual_ranks >= cuts_s[c] & actual_ranks <= cuts_e[c], na.rm = TRUE)
    }
    resmatrix[k,length(cuts_s) + 1] = sum(is.na(actual_ranks))
  }
  return(resmatrix)
}

# test
# count_matches(est_ranks = jm_ranks[,6], orig_ranks = jm_links$Ranking)



## TODO: Redo plot below




plot(x = -1,y = -100, xlim = c(0,50), ylim = c(0,40), xlab = "Top x", 
     ylab = "Number of Top x correct", main = "James Harrington")
#|----##../ --Tue Feb 24 17:29:13 2015--
for(k in 1:3) {
  
  xs = rowSums(count.res.matrix[,,k])
  ys = rowSums(count.res.matrix[,to.analyze,k, drop = FALSE])
  points(xs, ys, type = "l", col = k)
  
}
legend(x = "topleft", col = 1:3, lty = 1, 
       legend = c("PGL full docs", "PGL split-docs", "Corr split-docs"),
       cex = 0.7)


