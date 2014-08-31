load("update.datamat.Rdata")
load("test.Rd")
load("stage2/odnbdata.Rdata")

names.totest = two.word.names[setdiff(1:2661, 2360)]

samp.order = sample(1:length(names.totest), size = length(names.totest))

ODNB.dates = rep("", times = length(ODNB.data))
for(j in 1:length(ODNB.data)) {
  ODNB.dates[j] = gsub("<.*?>", "", ODNB.data[[j]]$date)
  if (j %% 1000 == 0) { print(j) }
}

names.simplified = gsub("<.*?>", "", ODNB.names)
dates.simplified = ODNB.dates

doc.lengths = rep(0, times = length(ODNB.dates))
for(i in 1:length(doc.lengths)) {
  doc.lengths[i] = sum(nchar(ODNB.data[[i]]$text))
  if (i %% 100 == 0) {
     print(i)
  }
}

save(samp.order, names.simplified, dates.simplified, 
     doc.lengths, two.word.names, file = "namedatecheck.Rdata")

# Very reliant on provided sample
try.sample <- function(NS) {
  result = list()
  for(i in NS) {
    result[[i]] = list()
    words = two.word.names[samp.order[i]]
    result[[i]][[1]] = words
    splits = strsplit(words," ")[[1]]
    matches = grep(splits[1], names.simplified)
    for(j in splits[-1]) {
      matches = intersect(matches, grep(j, names.simplified))
    }
    
    temp =             data.frame(Name=names.simplified[matches], 
                                  Dates=dates.simplified[matches],
                                  Length=doc.lengths[matches])
    result[[i]][[2]] = temp[order(temp$Length),]

#     if (!(dim(result[[i]][[2]])[1] == 0)) {
#       aa = sum(result[[i]][[2]]$Birth > 1525 & result[[i]][[2]]$Death < 1725)
#       if(aa == 0) {
#         result[[i]][[2]] = "Not in Date Range"
#       }
#     }
  }
  return(result)
}

try.sample(NS = 1:20)
try.sample(NS = 21:50)
try.sample(NS = 51:80)


# Name
# 2 John Cook (bap. 1608, d. 1660)
# 5 William Herbert (third earl of Pembroke) (1580–1630)
# 7 Sir Henry Killigrew (1525x8–1603)
# 20 Herbert, Sir Henry (bap. 1594, d. 1673)
# 26 John Kennedy (sixth earl of Cassillis) (1601x7–1668)

# 27 Andrew Marvell (1621–1678)
# 28 James II and VII (king of England, Scotland, and Ireland) (1633–1701)
# 37 John Drummond (styled first earl of Melfort and Jacobite first duke of Melfort) (1649–1714)
# 40 William Morgan (bishop of St Asaph) (1544/5–1604) 
# 51 Michael Drayton (1563–1631)

# Alternates
# 52 Thomas Cooper (theologian and bishop of Winchester) (c.1517–1594)
# 55 Robert Southwell [St Robert Southwell] (1561–1595)
# 56 George Herbert (Church of England clergyman) (1593–1633)
# 58 George Baker (1540–1612)
# 72 Sir Peter Lely (1618–1680)
# 75 Thomas Bilson (1546/7–1616)
# 78 Edward Montagu (second earl of Manchester) (1602-1671)


