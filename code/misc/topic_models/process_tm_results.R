library(topicmodels)
source("code/ODNB/ODNB_setup.R")
load(zzfile_curated_nodeset_update)

load("data/TOPIC_MODEL/tms2.Rdata")
load("data/TOPIC_MODEL/clean_corpus_20150608.Rdata")

SDFB_IDS = match(1:max(id_table$LARGE_INDEX, na.rm = TRUE), id_table$LARGE_INDEX)
SDFB_NAMES = paste(nodeset$first_name[SDFB_IDS], nodeset$surname[SDFB_IDS], sep = " ")

conftable = read.csv("conf_matrix_20141129.csv")
head(conftable)





conftable[which(conftable$ID1 == 1715),]
main_topics = topics(tm_stem_5)

ids = SDFB_IDS
confs = conftable
topics = as.numeric(main_topics)

threshold_topics = function(tm, thres = 0.6) { 
  post_mat = posterior(tm)[[2]]
  tops = topics(tm)
  below_thres = which(apply(post_mat, 1, max) < thres)
  tops[below_thres] = NA
  return(tops)
}

tops_thres = threshold_topics(tm_stem_5)

test = extract_network_topic_table(SDFB_IDS, as.numeric(topics(tm_stem_10)), conftable)
test2 = extract_network_topic_table(SDFB_IDS, tops_thres, conftable)
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

process_array(test2, 4, 5)





tp = topics(topic.models)
within_count=0
between_count=0
w_total=0
b_total = 0

w_ge90 = 0
b_ge90 =0
w_ge75 = 0
b_ge75 = 0
w_ge5 = 0
b_ge5 = 0

for(j in 1:6289) {
  
  base = tp[j]
  same_topic = setdiff(which(tp == base),j)
  diff_topic = setdiff(setdiff(1:6289, same_topic),j)
  
  within_count = within_count + length(same_topic)
  between_count = between_count + length(diff_topic)
  
  w_total = w_total + sum(total.lamb[j,same_topic])
  b_total = b_total + sum(total.lamb[j,diff_topic])
  
  w_ge90 = w_ge90 + sum(total.lamb[j,same_topic] >= .90)
  w_ge75 = w_ge75 + sum(total.lamb[j,same_topic] >= .75)
  w_ge5 = w_ge5 + sum(total.lamb[j,same_topic] >= .5)
  
  b_ge90 = b_ge90 + sum(total.lamb[j,diff_topic] >= .90)
  b_ge75 = b_ge75 + sum(total.lamb[j,diff_topic] >= .75)
  b_ge5 = b_ge5 + sum(total.lamb[j,diff_topic] >= .5)
  print(j) 
}














load("")


terms(tm_stem_5, 25)

??topicmodels


## for each -- table of top terms
dim(posterior(tm_stem_5)[[1]])
dim(posterior(tm_stem_5)[[2]])

tm_stem_5
