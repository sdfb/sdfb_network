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
names(bios) = 1:13309

## drop small/empty bags
inds = 1:13309
inds = inds[-which(nchar(bios) < 100)]
sub_bios = bios[-which(nchar(bios) < 100)]
large_inds = inds[-which(nchar(bios) < 2500)]
large_bios = bios[-which(nchar(bios) < 2500)]
id_table = data.frame(SDFB_ID = 1:13309, INDEX = match(1:13309, inds), LARGE_INDEX = match(1:13309, large_inds))

## use package 'tm' to clean up the bags of words
clean_bags = function(corpus) {
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, tolower)
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, removeWords, stopwords(kind = 'en'))
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, PlainTextDocument)
  return(corpus)
}
bios_src = VectorSource(sub_bios)
corpus = clean_bags(VCorpus(bios_src))

library(SnowballC)
stem_corpus = tm_map(corpus, stemDocument)
to.rm.list = c("age", "april", "brother", "cousin", "date", "daughter", "day", "death",
               "decemb", "die", "earlier", "father", "februari", "five", "fourth", "friend",
               "husband", "januari", "juli", "june", "march", "month", "mother", "novemb",
               "octob", "septemb", "six", "third", "fifth", "nephew", "seven", "sir", "one", 
               "first", "also", "may", "two", "time", "wife", "marri", "becam", "new", "made", "howev",
               "august", "three", "born", "took", "mani", "well", "known", "great", "much", "now",
               "like", "though", "children", "person")
stem_corpus = tm_map(stem_corpus, removeWords, to.rm.list)

dtm = DocumentTermMatrix(x = corpus, control = list(bounds = list(global = c(20, 8000)), minWordLength = 3))
stem_dtm = DocumentTermMatrix(x = stem_corpus, control = list(bounds = list(global = c(20, 8000)), minWordLength = 3))




largebios_src = VectorSource(large_bios)
corpus_large = clean_bags(VCorpus(largebios_src))
library(SnowballC)
stem_corpus_large = tm_map(corpus_large, stemDocument)
to.rm.list = c("age", "april", "brother", "cousin", "date", "daughter", "day", "death",
               "decemb", "die", "earlier", "father", "februari", "five", "fourth", "friend",
               "husband", "januari", "juli", "june", "march", "month", "mother", "novemb",
               "octob", "septemb", "six", "third", "fifth", "nephew", "seven", "sir")
stem_corpus_large = tm_map(stem_corpus_large, removeWords, to.rm.list)

sl_dtm = DocumentTermMatrix(stem_corpus_large, control = list(bounds = list(global = c(20, 4000)), minWordLength = 3))
dim(sl_dtm)
save(id_table, corpus, stem_corpus, corpus_large, stem_corpus_large, dtm, stem_dtm, sl_dtm, file = "data/TOPIC_MODEL/clean_corpus_20150610.Rdata")
