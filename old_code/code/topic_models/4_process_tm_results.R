
# Load libraries ----------------------------------------------------------

library(topicmodels)
source("code/ODNB/ODNB_setup.R")
load(zzfile_curated_nodeset_update)

load("data/TOPIC_MODEL/fitted_topic_models_20150614.Rdata")
load("data/TOPIC_MODEL/clean_corpus_20150610.Rdata")

conftable = read.csv("conf_matrix_20141129.csv")

# Helper functions --------------------------------------------------------

threshold_topics = function(tm, thres = 0.6) { 
  ## Returns only the topic assignments where the posterior probabilitiies are higher than the threshold
  post_mat = posterior(tm)[[2]]
  tops = topics(tm)
  below_thres = which(apply(post_mat, 1, max) < thres)
  tops[below_thres] = NA
  return(tops)
}

extract_avg_confs = function(ids, topics, confs) {
  ## ids -- give SDFB id's
  ## topics -- cluster for topic model
  ## confs -- data frame: ID1, ID2, ConfEst
  ## thres -- different vaues to threshold the confidence estimates
  
  ## link_array -- dim 1,2: cluster assignments; dim 3: amount that passes threshold
  nc = length(unique(topics[!is.na(topics)]))
  confs$T1 = topics[match(confs$ID1, ids)]
  confs$T2 = topics[match(confs$ID2, ids)]
  
  anyna = is.na(confs$T1) | is.na(confs$T2)
  confs = confs[!anyna,]
  topiccounts = as.numeric(table(topics))
  
  b_sum = 0
  w_sum = 0
  for(j in 1:nrow(confs)) {
    if (confs$T1[j] == confs$T2[j]) {
      w_sum = w_sum + confs$ConfEst[j]
    } else {
      b_sum = b_sum + confs$ConfEst[j]
    }
  }
  
  t_count = length(topics) * (length(topics)-1) / 2
  tt = table(topics)
  w_count = sum(tt * (tt -1) / 2)
  between = b_sum/(t_count - w_count)
  within = w_sum/w_count
  
  return(c(between, within))
}


extract_network_topic_table = function(ids, topics, confs, thres = c(.90, .75, .50, .30)) {
  ## ids -- give SDFB id's
  ## topics -- cluster for topic model
  ## confs -- data frame: ID1, ID2, ConfEst
  ## thres -- different vaues to threshold the confidence estimates
  
  ## link_array -- dim 1,2: cluster assignments; dim 3: amount that passes threshold
  nc = length(unique(topics[!is.na(topics)]))
  link_array = array(data = 0, dim = c(nc, nc, length(thres)+1))
  confs$T1 = topics[match(confs$ID1, ids)]
  confs$T2 = topics[match(confs$ID2, ids)]
  
  anyna = is.na(confs$T1) | is.na(confs$T2)
  confs = confs[!anyna,]
  topiccounts = as.numeric(table(topics))
  
  for(j in seq_along(thres)) { 
    pass_thres = which(confs$ConfEst >= thres[j])
    for(k in pass_thres) {
      link_array[confs$T1[k],confs$T2[k],j] = link_array[confs$T1[k],confs$T2[k],j] + 1
    }
  }
  
  for(j in 1:nc) {
    for(k in 1:nc) {
      if (j < k) { 
        link_array[j,k,1:length(thres)] = link_array[j,k,1:length(thres)] + link_array[k,j,1:length(thres)]
        link_array[j,k,length(thres) + 1] = topiccounts[j] * topiccounts[k]
      } else if (j == k) { 
        link_array[j,k,length(thres) + 1] = topiccounts[j] * (topiccounts[k] - 1) / 2
      }
    }
  }
  
  for(j in 1:nc) { for(k in 1:nc) { 
    if (j > k) { 
      link_array[j,k,] = link_array[k,j,]
    }
  }}
  
  return(link_array)
}

process_array = function(a, dim, finaldim) {
  ## computes the between/within values, for a given array 'a', dimension 'dim', and total dimension 'finaldim'
  within = sum(diag(a[,,dim]))/sum(diag(a[,,finaldim]))
  between = sum(a[,,dim][lower.tri(a[,,dim])]) / sum(a[,,finaldim][lower.tri(a[,,dim])])
  return(c(between, within))
}


# Script ------------------------------------------------------------------

## Extract nodeset information
SDFB_IDS = match(1:max(id_table$INDEX, na.rm = TRUE), id_table$INDEX)
SDFB_NAMES = paste(nodeset$first_name[SDFB_IDS], nodeset$surname[SDFB_IDS], sep = " ")
SDFB_DATES = nodeset$full_date[SDFB_IDS]

## Output best topic for each actor

script_extract_tables = function(input_tm, out_prefix = "test") {
  out1 = data.frame(SDFB_ID = SDFB_IDS, Name = SDFB_NAMES, Dates = SDFB_DATES, Class = as.numeric(topics(input_tm)))
  write.csv(out1, row.names = FALSE, file = paste("data/TOPIC_MODEL/results/", out_prefix, "_most_likely_cluster.csv", sep = ""))
  
  out2 = terms(input_tm, 500)
  write.csv(out2, file = paste("data/TOPIC_MODEL/results/", out_prefix, "_top500_cluster_terms.csv", sep = ""))

  post = posterior(input_tm)
  
  df1 = post$topics
  colnames(df1) = paste("Topic", 1:ncol(df1), sep = "_")
  df1 = cbind(data.frame(SDFB_ID = SDFB_IDS, Name = SDFB_NAMES, Dates = SDFB_DATES), df1)
  write.csv(df1, row.names = FALSE, file = paste("data/TOPIC_MODEL/results/", out_prefix, "_posteriorprob_topics.csv", sep = ""))
  
  df2 = t(post$terms)
  colnames(df2) = paste("Topic", 1:ncol(df2), sep = "_")
  df2 = cbind(data.frame(Terms=rownames(df2)), df2)
  write.csv(df2, row.names = FALSE, file = paste("data/TOPIC_MODEL/results/", out_prefix, "_posteriorprob_terms.csv", sep = ""))
  
  tab = extract_network_topic_table(ids = SDFB_IDS, topics = as.numeric(topics(input_tm)), confs = conftable)
  restable = matrix(0, nrow = 4, ncol = 2)
  largetable = NULL
  for(j in 1:4) { 
    restable[j,] = process_array(tab, j, 5)
    largetable = rbind(largetable, tab[,,j]/tab[,,5], t(rep(NA, times = dim(tab)[1])))
  }
  restable = data.frame(cbind(paste("Links: Confidence >= ", c(.90, .75, .50, .30)), restable))
  colnames(restable) = c("Conf_Threshold", "Between", "Within")
  write.csv(restable, row.names = FALSE, file = paste("data/TOPIC_MODEL/results/", out_prefix, "_result_table.csv", sep = ""))
  write.csv(largetable, row.names = FALSE, file = paste("data/TOPIC_MODEL/results/", out_prefix, "_detailed_result_table.csv", sep = ""))
  
  return("Done!")
}

script_extract_tables(tm_stem_5, out_prefix = "TM5")
script_extract_tables(tm_stem_10, out_prefix = "TM10")
script_extract_tables(tm_stem_20, out_prefix = "TM20")

extract_avg_confs(ids = SDFB_IDS, topics = as.numeric(topics(tm_stem_5)), confs = conftable)
extract_avg_confs(ids = SDFB_IDS, topics = as.numeric(topics(tm_stem_10)), confs = conftable)
extract_avg_confs(ids = SDFB_IDS, topics = as.numeric(topics(tm_stem_20)), confs = conftable)



