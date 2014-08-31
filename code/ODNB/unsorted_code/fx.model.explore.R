
output.csv.all.edges <- function(list.edgemats, list.namevecs, 
                                 file = NULL, each.edge.twice = TRUE, 
                                 model.ID) {
  # Function takes a list of edge matrices (list.edgemats),
  #   a list of name vectors (names of people on edges), and
  #   then stores into a csv file (and returns, also) a matrix with 
  #   first two columns Person1/Person2, and remaining columns are 1/0
  #   depending whether edge is found. 
  #   'each.edge.twice = TRUE' => each edge found will be listed twice, as
  #   both persons can be Person1 => abc order will allow one to look at each
  #   person to find his/her estimated connections
  #   'model.ID' = model 'name'; char vec of same length as the two lists.
  #    This is used in the data frame's column names
  # .csv file allows for processing in Excel (optional; leave as NULL if not needed)
  
  upper.bound.names = sum(sapply(X=list.edgemats, FUN = sum))
  N = length(list.edgemats)
  
  result.matrix = matrix("", nrow = upper.bound.names, ncol = N+2)
  next.ind = 1
  
  for(i in 1:N) { #Do each edge matrix in order
    curmat = list.edgemats[[i]]
    curnames = list.namevecs[[i]]
    
    temp = rep(0, times = N) # All 0 except for proper entry
    temp[i] = 1
    
    # progress
    total.toproc = dim(curmat)[1] * (dim(curmat)[1] - 1) / 2
    processed = 0
    for (r in 2:(dim(curmat)[1])) {
      # Progress checking
      if (r %% 10 == 0) {
        print(paste("Edge matrix",i," ::: Progress =",round(processed/total.toproc, 3)))
      }
      
      for (c in intersect(which(curmat[r,] == 1), 1:(r-1))) {
        
        if (i == 1) { # first matrix => add immediately
          result.matrix[next.ind,] = c(curnames[r], curnames[c],temp)
          next.ind = next.ind + 1
        } else { #not first matrix: look for match first
          first.match = intersect(which(curnames[r] == result.matrix[,1]),
                                  which(curnames[c] == result.matrix[,2]))
          second.match = intersect(which(curnames[r] == result.matrix[,2]),
                                   which(curnames[c] == result.matrix[,1]))
          if (length(first.match) == 1) {
            result.matrix[first.match,2+i] = 1
          } else if (length(second.match) == 1) {
            result.matrix[second.match,2+i] = 1
          } else { #no match => add new
            result.matrix[next.ind,] = c(curnames[r], curnames[c],temp)
            next.ind = next.ind + 1
          }
        }
        
      }
      processed = processed + (r-1)
    }
  }
  result.matrix = result.matrix[1:(next.ind - 1),]
  result = result.matrix
  names(result) = c("Person1", "Person2", model.ID)
  
  if (each.edge.twice) { #Make copy
    result.new = result
    result.new[,1] = result[,2]
    result.new[,2] = result[,1]
    result = rbind(result, result.new)
  }
  
  if (!is.null(file)) {
    write.csv(result, file = file) 
  }
  
  return(result)
}

find.nodenum.from.name <- function(query, names, exclude = NULL) {
  # Finds all matches of query within 'names', a vector of node labels
  # query can be a regular expression. 
  # Ex: find.nodenum.from.name(query = "Marlow", names = ...)
  test = grep(query, names)
  result = cbind(test, names[test])
  colnames(result) = c("Node Number", "Person")
  if (!is.null(exclude)) {
    bad = match(exclude, test)
    bad = bad[!is.na(bad)]
    if (length(bad > 0)) {
    result = result[-bad,]
    }
  }

  return(result)
}


## Function to give all connections given node number (can be vector of numbers).
find.names.from.nodenum <- function(edgemat.small, edgemat.big, names,
                                    nodenum, two.lists = FALSE) {
  # Function takes in a list of nodenumbers where edges are desired; and
  #   possibly two edge matrices (with same name nodes) and outputs 
  #   all the found edges of the small edge matrix (unless two.lists = TRUE)
  #   in which cases it makes lists of matches for both,
  #   and big edgelist only. 
  
  result = list()
  
  nodenum = as.numeric(nodenum)
  for(j in 1:length(nodenum)) {
    result[[j]] = list()
    result[[j]][[1]] = c("Person is:", names[nodenum])
    matches = which(edgemat.small[nodenum[j],] == 1) 
    result[[j]][[2]] = cbind(matches, names[matches])
    colnames(result[[j]][[2]]) = c("Node Number", "Person (small edge matrix)")
    
    if(two.lists) {
      matrix2 = edgemat.big - edgemat.small
      matches = which(matrix2[nodenum[j],] == 1)
      result[[j]][[3]] = cbind(matches, names[matches])
      colnames(result[[j]][[3]]) = c("Node Number", "Person (big edge matrix, not small)")
    }
  }
  return(result)
}

model.comparison <- function(edgemat1, edgemat2, all.nodes = TRUE, nodenum = NULL,
                             model1.ID = "Model 1", model2.ID = "Model 2", 
                             need.diag = TRUE) {
  # This function checks agreement of edges between edgemat1, edgemat2.
  # All nodes will be checked, unless all.nodes = F, nodenum = vec of interesting nodes
  # model1.ID, model2.ID are just names to make output more sensible
  # need.diag = TRUE if input has any nonzero in lower left triangle
  
  #Clearing lower diagonal (so edges are only counted once)
  d = dim(edgemat1)[1]
  
  if (need.diag) {
  for(i in 1:(d-1)) {
    edgemat1[i,i:d] = 0
    edgemat2[i,i:d] = 0
  }
  }
  
  # Subset of nodes to look at, if necessary
  if (!all.nodes) {
    edgemat1 = edgemat1[nodenum,]
    edgemat2 = edgemat2[nodenum,]
  }
  
  equals = (edgemat1 + edgemat2 == 2)
  
  # Results will be displayed as follows: Table of: 
  # Two rows
  # columns are # total nodes, # total matching edges, total edges, frac edges seen in other model
  res = matrix(0.0, nrow = 2, ncol = 4)
  rownames(res) = c(model1.ID, model2.ID) 
  colnames(res) = c("# Nodes", "# Matched Edges", "# Total Edges", "# Frac edges match")
  res[,1] = d
  res[,2] = sum(equals)
  res[,3] = c(sum(edgemat1), sum(edgemat2)) 
  res[,4] = res[,2] / res[,3]
    
  return(res)
  
}


# This function is not fixed; intention is to sample different types of 
#   edges, given a specific node. 
# sample.for.nodenum <- function(nodenum, num.high = 8, num.med = 6, num.none = 6, names = reordered.names, no.saint = TRUE) {
#   temp = find.names.from.nodenum(nodenum = nodenum, two.lists = TRUE)[[1]]
#   high.dat = temp[[2]]
#   med.dat = temp[[3]]
#   high.ind = as.numeric(high.dat[,1])
#   med.ind = as.numeric(med.dat[,1])
#   no.ind = setdiff(1:2773, union(med.ind, high.ind))
#   if (no.saint) {
#     saints = 2112:2223
#     high.ind = setdiff(high.ind, saints)
#     med.ind = setdiff(med.ind, saints)
#     no.ind = setdiff(no.ind, saints)
#   }
#   if (num.high > length(high.ind)) { 
#     print("Not enough high-confidence connections, so all are used")  
#     num.high = length(high.ind)
#   } 
#   if (num.med > length(med.ind)) {
#     print("Not enough med-confidence connections, so all are used")
#     num.med = length(med.ind)
#   }
#   results = matrix("",ncol = 3, nrow = num.high+num.med+num.none)
#   t1 = sample(high.ind, size = num.high)
#   results[1:num.high,] = cbind(t1,names[t1],1)
#   t1 = sample(med.ind, size = num.med)
#   results[(1:num.med)+num.high,] = cbind(t1,names[t1],2)
#   t1 = sample(no.ind, size = num.none)
#   results[(1:num.none)+num.med+num.high,] = cbind(t1,names[t1],3)
#   
#   DIM = num.none + num.med + num.high
#   aaa = sample(10000:50000, size = DIM) * 3 + as.numeric(results[,3])
#   results[,3] = aaa
#   
#   rearrangement = sample(1:DIM, size = DIM)
#   colnames(results) = c("Node Number", "Name", "Code")
#   return(results[rearrangement,])
# }
