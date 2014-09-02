# Checking manually tagged data...


load("stage2/odnbdata.Rdata")

which(ODNB.nums == 24288)
ODNB.data[[24255]]

"dataman/mantag/24288.proc.txt"


## Input      |
## Output     |
## 
## 

tagged.text = c(strsplit(readLines("dataman/mantag/24288.proc.txt"), " "), recursive = TRUE)
sum(tagged.text == "")
tagged.text = tagged.text[!(tagged.text == "")]
a = proc_tagtext(text = tagged.text, type = "ST")
#|  ************
#|----##replace period with _ --Tue Sep  2 09:49:17 2014--
head(a)

load("truedocs.Rdata")
b = get.truetext(24288)

load("stage2/stan.Rdata")
load("stage2/lpdocs.Rdata")

records[24288]
recordl[24288]

st.docs[[21590]]

cur.types = matrix(c("ST", "LP", "ST", "LP", "PERSON", "PERSON", "ORGANIZATION", 
                     "ORGANIZATION"), nrow = 4)

find.tags(num = 24288, which.types = cur.types) -> test

find.wordlists(main.person = c("John", "Rushworth"), dat.mat = test) -> test2
tr = rbind(b, a)
pr = t(test[,1:2])

error.findall(true = tr, pred = pr) -> test3

error.findall(true = tr, pred = rbind(b, test2[[3]])) -> test4

testa = cbind(test, True=a)
head(test, 100)
head(test2)
head(test3)


NER.results = testa
true.found = cbind(test3[[1]], Step4to6 = test4[[1]][,3])
all.found = test3[[2]]
all.found46 = test4[[2]]

save(NER.results, true.found, all.found, all.found46, file = "Analysis.JRushworth.Rdata")


