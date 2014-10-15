## TODO: [IMPORTANT] this code is where the main adjustments need to be made... 

load("../../private_data/odnb_data_proc/ODNB_fullnamelist.Rdata")
load("../../private_data/odnb_data_proc/ODNB_entitymatrix.Rdata")
load("../../private_data/odnb_data_proc/ODNB_improvedpred.Rdata")

nrow(full_result3)

keep_biodates = function(df, rmin = 1500, rmax = 1800) {
  tokeep = ((df$ext_birth < rmax) & (df$ext_death > rmin)) | ((df$bio_min_date < rmax) & (df$bio_max_date > rmin))
  bio_names = which(df$ID < 30000000)
  return(intersect(bio_names, which(tokeep)))
}
keep_obsdates = function(df, rmin = 1500, rmax = 1800) {
  tokeep = ((df$occur_min_date < rmax) & (df$occur_max_date > rmin))
  return(intersect(which(df$ID >= 30000000), which(tokeep)))
}

keep_biodates(full_result3) -> z
keep_obsdates(full_result3) -> y

keep_results = full_result3[c(z,y),]
head(big_entity_matrix)
big_entity_matrix$DocSeg = big_entity_matrix$DocumentNum + 0.01 * big_entity_matrix$Segment
big_entity_matrix$MatchID = NA

biosubs = which(big_entity_matrix$ID == 1)
z = which(!is.na(match(10000000 + big_entity_matrix$DocumentNum[biosubs], full_result3$ID)))
big_entity_matrix$MatchID[biosubs][z] = 10000000 + big_entity_matrix$DocumentNum[biosubs][z]

z = which(!is.na(match(20000000 + big_entity_matrix$DocumentNum[biosubs], full_result3$ID)))
big_entity_matrix$MatchID[biosubs][z] = 20000000 + big_entity_matrix$DocumentNum[biosubs][z]

for(j in which(full_result3$ID >= 30000000)) {
  id = full_result3$ID[j]
  name = full_result3$main_name

  big_entity_matrix$MatchID[which(big_entity_matrix$Entity == name)] = id
  print(j)
  
}
id_vec = full_result3$ID[which(full_result3$ID >= 30000000)]
name_vec = full_result3$main_name[which(full_result3$ID >= 30000000)]
match(big_entity_matrix$Entity, name_vec) -> z
big_entity_matrix$MatchID[which(!is.na(z))] = id_vec[z[!is.na(z)]]


## Need to code in specific changes / notices...


## Code to search for name matches:
as.character(unique(big_entity_matrix$Entity[is.na(big_entity_matrix$MatchID)])) -> entities_totest
full_result3$main_name[which(full_result3$ID < 30000000)] -> namelist

invert_name_comma = function(x) {
  # If there is a comma in the name, (implying Last, First), make it First Last.
  grep(",",x) -> tofix
  strsplit(x[tofix], ",") -> splits
  sapply(splits, function(x) {paste(rev(x), collapse = " ")}) -> fixed
  x[tofix] = fixed
  gsub("(^ +)|( +$)", "", x) -> x
  gsub(" +", " ", x) -> x
  return(x) 
}
invert_name_comma(namelist) -> namelist

head(entities_totest)

df = big_entity_matrix[1:100,]

find_potential_matches = function(df) {
  ## Assumes 'namelist', 'full_result3' exist in background
  ## df is a data frame containing columns containing name, counts, dates for each match.
  ## (this is a subset of big_entity_matrix)

  unique(as.character(df$Entity)) -> names_tosearch
  reslist = list()
  counter = 0
  for(s in names_tosearch) {
    counter = counter + 1
    if (counter %% 500 == 0) { cat("\n", counter, "---", date() ,"\n") }
    cat(".")
    entitynums = which(df$Entity == s)
    
    ## Look for exact match in order. If exist, done.
    notdone = TRUE
    
    exact_matches = which(namelist == s)
    if (length(exact_matches) > 0) {
      m = check_biodate(ind = exact_matches, mindate = df$MinDate[entitynums],
        maxdate = df$MaxDate[entitynums])

      done = apply(m, 2, any)
      for(i in which(done)) {
        reslist[[entitynums[i]]] = list(
                 DocSeg = df$DocSeg[entitynums[i]],
                 Count = df$Count[entitynums[i]],
                 Matches = exact_matches[which(m[,i])],
                 Weights = full_result3$bio_length[exact_matches[which(m[,i])]]  )
      }
      entitynums = entitynums[!done]
      notdone = !all(done)
    }
    
    
    ## Look for subset match. If found, done.
    if (notdone) {
      partial_matches = setdiff(grep(s, x = namelist, fixed = TRUE), exact_matches)
      if (length(partial_matches) > 0) {
        
        m = check_biodate(ind = partial_matches, mindate = df$MinDate[entitynums],
          maxdate = df$MaxDate[entitynums])

        done = apply(m, 2, any)
        for(i in which(done)) {
          reslist[[entitynums[i]]] =list(
                 DocSeg = df$DocSeg[entitynums[i]],
                 Count = df$Count[entitynums[i]],
                 Matches = partial_matches[which(m[,i])],
                 Weights = full_result3$bio_length[partial_matches[which(m[,i])]])
            
        }
        notdone = !all(done)
        entitynums = entitynums[!done]
      }
    }
      
    ## Look for name subset match. If found, done.
    if (notdone) {
      s_split = strsplit(s, " ")[[1]]
      for(k in seq_along(s_split)) {
        if (k == 1) {
          r = grep(paste("(^|\\W)",s_split[k],"($|\\W)", sep = ""), namelist)
        } else {
          r = intersect(r, grep(paste("(^|\\W)",s_split[k],"($|\\W)", sep = ""), namelist))
        }
      }
      partial_matches = r
      if (length(partial_matches) > 0) {
        
        m = check_biodate(ind = partial_matches, mindate = df$MinDate[entitynums],
          maxdate = df$MaxDate[entitynums])
        
        done = apply(m, 2, any)
        for(i in which(done)) {
          reslist[[entitynums[i]]] = list(
                 DocSeg = df$DocSeg[entitynums[i]],
                 Count = df$Count[entitynums[i]],
                 Matches = partial_matches[which(m[,i])],
                 Weights = full_result3$bio_length[partial_matches[which(m[,i])]])
        }
      }
    }
  } 
    
  return(reslist)
}
z = find_potential_matches(df)

ind = 1:4
mindate = c(1830, 1799)
maxdate = c(1890, 1801)


check_biodate = function(ind, mindate, maxdate, adj = 10) {
  ## ind = indices to check
  ## mindate/maxdate will be adjusted by 'adj' years.
  ## both of these could be vectors

  res = matrix(FALSE, nrow = length(ind), ncol = length(mindate))

  nas = which(is.na(mindate) | is.na(maxdate))
  if (length(nas) > 0) { res[,nas] = TRUE }

  mindate = mindate - adj
  maxdate = maxdate + adj

  for(j in seq_along(ind)) {
    i = ind[j]
    ## maxdate has to be after birth, mindate has to be before death => overlaps lifetime some.
    extractdate = ((full_result3$ext_birth[i] < maxdate) & (mindate < full_result3$ext_death[i]))
    biodate = ((full_result3$bio_min_date[i] < maxdate) & (mindate < full_result3$bio_max_date[i]))
    if (any(is.na(extractdate))) {
      if (any(is.na(biodate))) {
        res[j,] = TRUE
      } else {
        res[j,] = res[j,] | biodate
      }
    } else {
      res[j,] = res[j,] | extractdate
    }
  }
  return(res)
}


entity_toprocess = big_entity_matrix[is.na(big_entity_matrix$MatchID),]
df = entity_toprocess[1:100,]
row_Docs = unique(big_entity_matrix$DocSeg)
col_Ids = full_result3$ID

library(Matrix)
#length(unique(df$Entity))
base_doccount = Matrix(0, nrow = length(row_Docs), ncol = length(col_Ids), sparse = TRUE)

big_entity_matrix[!is.na(big_entity_matrix$MatchID),] -> toadd 

for(j in 1:nrow(toadd)) {
  y = which(toadd$MatchID[j] == col_Ids)
  x = which(toadd$DocSeg[j] == row_Docs)
  if (length(y) == 1 & length(x) == 1) {
    base_doccount[x,y] = toadd$Count[j]
  } else {
    stop("bad col/rowname")
  }
  if (j %% 500 == 0) { print(j) }
}

save(base_doccount, file = "../../private_data/odnb_data_proc/ODNB_basecounts.Rdata")
load("../../private_data/odnb_data_proc/ODNB_basecounts.Rdata")

sum(base_doccount)
### Process
need_randomize = list()
counter = 1
for(j in 1:ceiling(nrow(entity_toprocess) / 10000)) {
  print("**********************")
  print(j)
  print("**********************")
  temp = find_potential_matches(df = entity_toprocess[1:10000 + (j - 1) * 10000,])

  print("********************** -- processing results --")
  for(k in 1:length(temp)) {
    if (!is.null(temp[[k]])) {
      if (length(temp$Matches) == 1) { ###### BUG IN THIS LINE. 
        x = which(temp$DocSeg == row_Docs)
        y = temp$Matches
        base_doccount[x,y] = temp$Count
      } else {
        need_randomize[[counter]] = temp[[k]]
        counter = counter + 1
      }
    }
  }
}

need_torandomize = list()
counter2 = 1
for(j in 1:length(need_randomize)) {
  if (j %% 200 == 0) { print(j) }
  if (length(need_randomize[[j]]$Matches) == 1) {
    x = which(need_randomize[[j]]$DocSeg == row_Docs)
    y = need_randomize[[j]]$Matches
    base_doccount[x,y] = need_randomize[[j]]$Count
  } else {
    need_torandomize[[counter2]] = need_randomize[[j]]
    counter2 = counter2 + 1
  }
}

sapply(need_torandomize, function(x) {x$DocSeg}) -> rand_docseg
match(rand_docseg, row_Docs) -> rand_docID

save(row_Docs, col_Ids, base_doccount, need_torandomize, file = "../../private_data/odnb_data_proc/ODNB_allcounts.Rdata")
load("../../private_data/odnb_data_proc/ODNB_allcounts.Rdata")

## system.time(( matchlist = find_potential_matches(df) ))

cond_min = function(ind) {
  ## Assumes existence of full_result3; pulls the best 'min/birth' date
  mindate = ifelse(test = (full_result3$ID[ind] < 30000000),
    yes = ifelse(is.na(full_result3$ext_birth[ind]), full_result3$bio_min_date[ind],full_result3$ext_birth[ind]),
    no = full_result3$occur_min_date[ind])
  return(mindate)
}

cond_max = function(ind) {
  ## Assumes existence of full_result3; pulls the best 'min/birth' date
  maxdate = ifelse(test = (full_result3$ID[ind] < 30000000),
    yes = ifelse(is.na(full_result3$ext_death[ind]), full_result3$bio_max_date[ind],full_result3$ext_death[ind]),
    no = full_result3$occur_max_date[ind])
  return(maxdate)
}
cond_min(73655)
cond_max(2)
min_vec = sapply(1:nrow(full_result3), cond_min)
max_vec = sapply(1:nrow(full_result3), cond_max)

bad_nodes_dates = union(which(min_vec > 1800), which(max_vec < 1500))

bad_nodes_names = NULL

which(full_result3$ID >= 30000000) -> ends
full_result3$main_name[ends]

grep_rm = c("Monograph", "Geography", "Imitation", "Pharma", "Uniform", "Indent", "Clergy", "Harbour", "Collegiate", "Chapel", "Teach", "History", "Gallery", "Magazine", "Again", "Mother", "Father", "Street", "Concert", "Truth", "Naked", "Nation", "Question", "Funeral", "Sermon", "Resource", "Works", "Chamber", "Archaeolog", "Brother", "Catechism", "Medical", "Justice", "Foundation", "Municipal", "Statute", "Limit", "Red Cross", "Philolog", "Foreign", "Festival", "Galleries", "Collection", "Templar", "Relief", "Domestic", "Medicine", "Author", "Embankment", "Merchant", "Adventure", "Telephone", "Laborato", "Retreat", "Register", "Fugitive", "Reform", "Committee", "Dispensary", "Federation", "Ordnance", "Wharf", "Chariot", "American", "Marxism", "Archive", "League", "District", "Paper", "England", "County", "Counties", "Imperial", "Airway", "Corporation", "Department", "Structure", "Energy", "Research", "Orchestra", "System", "Hotel", "Polity", "Policy", "Educat", "Record", "Science", "Academ", "Institut", "Indies", "Common", "Prayer", "Theatre", "Tragic", "Saxon", "Month", "Encyclop", "Statistic", "Account", "Colony", "Scotia", "Colour", "Revolution", "Movement", "Methodist", "Advertise", "Land Bill", "Pacific", "Examine", "Gentlemen", "Calender", "Associate", "Concept", "Building", "Division", "Priory", "Infirm", "Wife", "Confedera", "Ministry")
exact_rm = c("New", "Port", "Forest", "Board", "River", "Castle", "Fever", "Golden", "Man", "Diaries", "CB", "Hall", "St", "Old", "HMS", "Great", "The", "HRC", "MS", "Winter", "Natural", "City", "Border", "British", "Commerce", "Royal", "Guard", "Labour", "Academie", "Academy", "On", "My", "Standard", "Age", "Party", "ACE", "RAF", "Suit", "General", "Group", "Groups", "A", "Boy", "Boys", "Girl", "Girls", "Rule", "France", "Opera", "Own", "Home", "Diary", "Diaries", "MSS", "Manor", "Creed", "Schism", "England", "Fund", "Commonwealth", "Atlantic", "His", "Her", "Sports", "Sport", "Life", "Hour", "Hours", "Women", "Men", "Woman", "Man", "Congress", "Bank", "Me", "Sea", "City", "Cities", "Calendar", "Room", "Temple", "Port", "Ports", "RO", "Indiaman", "Mile", "End", "Row", "Mayor", "America", "Iliad", "Slave", "Trade", "Gas", "Light", "Country", "Countries", "Ulster", "Friar", "Friars", "Grande", "Riding", "India", "Almanac", "Janeiro", "Isle", "Elements", "Element", "War", "Common", "Prayer", "Review", "Place", "Criminal", "Pavilion", "Divine", "Britain", "Carta", "Regency", "Ancient", "Hymn", "Hymns", "Forum", "Morning", "Riding", "Brief", "Lives", "Cape", "Fleet", "Stratagem", "Essay", "An", "Britannica", "Baptism", "Nile", "Literary", "Gazette", "Daily", "Express", "Magic", "Metropolitan", "Police", "Circus", "Navigation", "Act", "Acts", "Enlightenment", "Testament", "Comedy", "Tales", "Century", "Plantation", "At", "In", "Several", "Indian", "Irish", "Almighty", "Test", "Militia", "Indians", "Arctic", "Conquest", "US", "Navy", "Scotland", "Economic", "European", "Community", "Organization", "Health", "MB", "Television")
for(s in grep_rm) {
  bad_nodes_names = c(bad_nodes_names, grep(s, full_result3$main_name))
}
for(s in exact_rm) {
  bad_nodes_names = c(bad_nodes_names, which(sapply(strsplit(full_result3$main_name, " "), function(x) {any(x == s)})))
}

bad_nodes_names = setdiff(unique(bad_nodes_names), which(full_result3$ID < 30000000))
length(bad_nodes_names)

rem_names = full_result3$main_name[setdiff(ends, bad_nodes_names)]
sample(rem_names, size = 300)

## To do: Figure out bad nodes


## To do: Combine nodes where necessary
list(c("King Charles I", "Charles I"), 
     c("King Charles II", "Charles II"),
     c("King James I", "James I"),
     c("King George II", "George II"),
     c("King George IV", "George IV"),
     c("King Edward VII", "Edward VII") ) -> changeatob

## Make sure everything has only one match
for(j in seq_along(changeatob)) {
  z = which(changeatob[[j]][1] == full_result3$main_name)
  print(z)
  print(full_result3$ID[z] >= 30000000)
  print(sum(changeatob[[j]][2] == full_result3$main_name))
}

## combine cols
torm_manual = NULL
for(j in seq_along(changeatob)) {
  oldcolnum = which(changeatob[[j]][1] == full_result3$main_name)
  combcolnum = which(changeatob[[j]][2] == full_result3$main_name)
  base_doccount[,combcolnum] = base_doccount[,combcolnum] + base_doccount[,oldcolnum]
  torm_manual = c(torm_manual, oldcolnum)
}  

## Manual name removal
## torm_manual = match(c("Sir Thomas", "Sir John", "Lady Mary", "Lady Margaret", "King Henry", "King John", "Lady Elizabeth", "Sir George", "Mary Anne", "Edward I", "Sir James", "Fort Williams", "Sir Francis"), full_result3$main_name)
## None of these seem to be there still.
all_rm = unique(c(torm_manual, bad_nodes_names, bad_nodes_dates))


## Remove undated entities.
na_dates = union(which(is.na(min_vec)), which(is.na(max_vec)))

doc_appear = rep(0, times = ncol(base_doccount))

for(j in 1:nrow(base_doccount)) {
  doc_appear = doc_appear + base_doccount[j,]
  if (j %% 1000 == 0) {print(j)}
}

for(k in seq_along(need_torandomize)) {
  if (k %% 1000 == 0) { print(k) }
  locs = need_torandomize[[k]]$Matches
  w = need_torandomize[[k]]$Weights
  wts = w / sum(w)
  doc_appear[locs] = doc_appear[locs] + wts
}
setdiff(which(doc_appear > 20), c(all_rm, na_dates)) -> z
length(z)

full_result3$main_name[z[which(full_result3$ID[z] >= 30000000)]]



TEMP_RM = na_dates

## Remove columns of base_doccount

f_count_mat = base_doccount[,z]
f_col_Ids = col_Ids[z]
f_row_Docs = row_Docs

f_randlist = need_torandomize
for(j in seq_along(f_randlist)) {
  f_randlist[[j]]$DocSeg = which(f_row_Docs == f_randlist[[j]]$DocSeg)
  f_randlist[[j]]$Weights = f_randlist[[j]]$Weights / sum(f_randlist[[j]]$Weights)
  cat(j, " ")
}

## fix Matches.

for(j in seq_along(f_randlist)) {
  ids = full_result3$ID[f_randlist[[j]]$Matches]
  f_randlist[[j]]$Matches = match(ids, f_col_Ids)
  cat(j, " ")
}

rownames(f_count_mat) <- f_row_Docs
colnames(f_count_mat) <- f_col_Ids
save(f_count_mat, f_col_Ids, f_row_Docs, f_randlist, file = "../../private_data/odnb_data_proc/ODNB_fincounts.Rdata")

save(f_count_mat, f_randlist, file = "041514_sparsedm.Rdata")

head(need_torandomize)







## todo: Create code to generate sub-matrices

sample_colnums = function(x = NULL) {
  ## If x is null, sample completely from f_randlist.
  ## If x is indexed, only sample if $DocSeg is in x.
  if (is.null(x)) {
    res = data.frame(
      Counts = sapply(f_randlist, function(x) { x$Count }),
      DocSeg = sapply(f_randlist, function(x) { x$DocSeg }),
      ColNum = sapply(f_randlist, function(x) { sample(x$Matches, prob = x$Weights, size = 1) }) )
    
  } else {
    DocSeg = sapply(f_randlist, function(x) {x$DocSeg})
    which(!is.na(match(DocSeg, x))) -> tokeep
    res = data.frame(
      Counts = sapply(f_randlist[tokeep], function(x) {x$Count}),
      DocSeg = DocSeg[tokeep],
      ColNum = sapply(f_randlist[tokeep], function(x) { sample(x$Matches, prob = x$Weights, size = 1)} ))
  }
  return(res)
}
sample_colnums() -> test
sample_colnums(1:50)


sample_matrix = function(frac = 0.5) {
  ## Requires f_randlist, f_count_mat

  keep_rows = sort(sample(1:nrow(f_count_mat), size = floor(nrow(f_count_mat) * frac)), decreasing = FALSE)
  samp = sample_colnums(keep_rows)

  samp_count_mat = f_count_mat
  for(k in 1:nrow(samp)) {
    if (k %% 1000 == 0) { cat(k) }
    if (!is.na(samp$ColNum[k])) {
      samp_count_mat[samp$DocSeg[k], samp$ColNum[k]] = samp$Counts[k]
    }
  }

  return(samp_count_mat[keep_rows,])
}

sample_matrix() -> test

for(k in 1:100) {
  cat("\n---------------",k,"--------------\n")
  SUBSET_DM = sample_matrix()
  save(SUBSET_DM, file = paste("subsetdm",k,".Rdata", sep = ""))
}
## To do: Fit model



## To do: Remove out-of-temporance links (or, at least, analyze)




find_concurrent_bios = function(ind, adj = 10) {
  ## Assumes existence of full_result3
  
  
  
}
