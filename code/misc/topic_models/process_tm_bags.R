## This file contains code that cleans up the bag of words that were found for the different actors. 
load("data/TOPIC_MODEL/topic_model_bags_RAW_20150608.Rdata")
library(tm)

bios = rep("", times = 13309)

## collapse each bag of words into a single string
for(j in seq_len(13309)) {
  if (!is.null(doc_section_list[[j]])) {
    bios[j] = paste(doc_section_list[[j]], collapse = " ")
  }
  cat(j, " ")
}

## drop small/empty bags
inds = 1:13309
inds = inds[-which(nchar(bios) < 100)]
sub_bios = bios[-which(nchar(bios) < 100)]

id_table = data.frame(SDFB_ID = 1:13309, INDEX = match(1:13309, inds))

## use package 'tm' to clean up the bags of words
bios_src = VectorSource(sub_bios)
corpus = VCorpus(bios_src)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, stopwords(kind = 'en'))
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, PlainTextDocument)

library(SnowballC)
stem_corpus = tm_map(corpus, stemDocument)
to.rm.list = c("age", "april", "brother", "cousin", "date", "daughter", "day", "death",
               "decemb", "die", "earlier", "father", "februari", "five", "fourth", "friend",
               "husband", "januari", "juli", "june", "march", "month", "mother", "novemb",
               "octob", "septemb", "six", "third", "fifth", "nephew", "seven", "sir")
stem_corpus = tm_map(stem_corpus, removeWords, to.rm.list)

dtm = DocumentTermMatrix(x = corpus, control = list(global = c(10, Inf), minWordLength = 3))
stem_dtm = DocumentTermMatrix(x = stem_corpus, control = list(global = c(10, Inf), minWordLength = 3))

save(id_table, corpus, stem_corpus, dtm, stem_dtm, file = "data/TOPIC_MODEL/clean_corpus_20150608.Rdata")
