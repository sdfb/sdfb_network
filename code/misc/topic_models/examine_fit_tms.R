## Produces tables and reports for topic models

load("data/TOPIC_MODEL/tms1.Rdata")

library(topicmodels)
topics(tm_raw_5)

head(posterior(tm_raw_5)$topics)
tops = topics(tm_raw_5)

terms(tm_raw_5, k = 10)
# dim(topics(topic.models,2))
# names(posterior(topic.models))
# head(posterior(topic.models)$topics)
# 
