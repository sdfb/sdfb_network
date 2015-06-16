
# Load libraries ----------------------------------------------------------

library(topicmodels)
source("code/ODNB/ODNB_setup.R")
load(zzfile_curated_nodeset_update)

load("data/TOPIC_MODEL/tms3.Rdata")
load("data/TOPIC_MODEL/clean_corpus_20150608.Rdata")

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
  between = sum(diag(a[,,dim]))/sum(diag(a[,,finaldim]))
  within = sum(a[,,dim][lower.tri(a[,,dim])]) / sum(a[,,finaldim][lower.tri(a[,,dim])])
  return(c(between, within))
}


# Script ------------------------------------------------------------------

## Extract nodeset information
SDFB_IDS = match(1:max(id_table$INDEX, na.rm = TRUE), id_table$INDEX)
SDFB_NAMES = paste(nodeset$first_name[SDFB_IDS], nodeset$surname[SDFB_IDS], sep = " ")
SDFB_DATES = nodeset$full_date[SDFB_IDS]

## Output best topic for each actor

script_extract_tables = function(input_tm, out_prefix = "test") {
  out1 = data.frame(SDFB_ID = SDFB_IDS, Name = SDFB_NAMES, Dates = SDFB_Dates, Class = input_tm)
  write.csv(out1, file = paste(out_prefix, "_most_likely_cluster.csv", sep = ""))
  
  out2 = terms(input_tm, 500)
  write.csv(out2, file = paste(out_prefix, "_top500_cluster_terms.csv"))
            
  return("Done!")
}




head(conftable)





conftable[which(conftable$ID1 == 1715),]
main_topics = topics(tm_stem_5)

ids = SDFB_IDS
confs = conftable
topics = as.numeric(main_topics)


tops_thres = threshold_topics(tm_stem_5)

test = extract_network_topic_table(SDFB_IDS, as.numeric(topics(tm_stem_5)), conftable)
test2 = extract_network_topic_table(SDFB_IDS, tops_thres, conftable)

process_array(test2, 4, 5)







load("")


terms(tm_stem_5, 25)

??topicmodels


## for each -- table of top terms
dim(posterior(tm_stem_5)[[1]])
dim(posterior(tm_stem_5)[[2]])

tm_stem_5
