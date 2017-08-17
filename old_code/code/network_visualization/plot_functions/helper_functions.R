#@S This file contains helper functions for the network plotting. 
#@L See function documentation for specific details for each of these functions

center_of_points = function(xycoords) {
  #@F ----------------------------------------
  #@F Function 'center_of_points'
  #@F Args (Input): xycoords: two column matrix (more columns would work too...)
  #@F Purpose: returns the mean of the min/max of each column.
  #@F Output: a vector of length ncol(xycords)
  #@F ----------------------------------------
  
  return((apply(xycoords, 2, max) + apply(xycoords, 2, min))/2)
}

match_two_columns = function(col1, col2, target1, target2) {
  #@F ----------------------------------------
  #@F Function 'match_two_columns'
  #@F Args (Input): col1, col2, target1, target2
  #@F   col1, col2: vectors of same type, same length
  #@F   target1, target2: singletons of same type as vectors
  #@F Purpose: This function returns a logical vector, where for each of the
  #@F   corresponding entries in col1 and col2, returns TRUE if those entries 
  #@F   match exactly the values of target1 and target2, in any possible order.
  #@F Example: col1 = ["a" "b"], col2 = ["c" "a"], target1 = "c", target2 = "a"
  #@F   output = [TRUE FALSE] (since "a" "c" matches "c" "a" in some order)
  #@F Output: logical vector, length of length(col1)
  #@F ----------------------------------------
  
  match1 = col1 == target1 & col2 == target2
  match2 = col2 == target1 & col1 == target2
  return(match1 | match2)
}

topk = function(dat, k, thres) {
  #@F ----------------------------------------
  #@F Function 'topk'
  #@F Args (Input): dat, k, thres
  #@F   dat = vector of values
  #@F   k = integer: the ideal number of values to return
  #@F   thres = number: at most 'k' values are returned; only values >= thres are returned
  #@F Purpose: Returns the indices of the 'k' largest values in 'dat' that are 
  #@F   larger than 'thres'
  #@F Output: A vector of indices, length <= 'k'
  #@F ----------------------------------------
  
  k = min(k, sum(dat >= thres))
  return(order(dat,decreasing= TRUE)[1:k])
}

center_matrix = function(mat, center = c(0,0)) {
  #@F ----------------------------------------
  #@F Function 'center_matrix'
  #@F Args (Input): mat, center
  #@F   mat = two-column matrix of coordinates [x,y]
  #@F   center = length-2 vector of coordinates to place centroid of coords in 'mat'
  #@F Purpose: Re-locates all the coordinates in 'mat' to be centered at 'center'
  #@F Output: Two-column matrix of coordinates
  #@F ----------------------------------------
  
  cent_mat = mat
  cent_mat[,1] = center[1]
  cent_mat[,2] = center[2]
  return(mat - apply(mat, 2, mean) + cent_mat)
}

scale_matrix = function(mat, sd_coord = c(1,1)) {
  #@F ----------------------------------------
  #@F Function 'scale_matrix'
  #@F Args (Input): mat, sd_coord
  #@F   mat = two-column matrix of coordinates [x,y]
  #@F   center = length-2 vector of values to use as the one-SD coordinate. 
  #@F Purpose: Rescales the coordinates in either column separately
  #@F Output: Two-column matrix of coordinates
  #@F Notes: This requires a centered matrix for appropriate results. 
  #@F ----------------------------------------
  res_mat = mat
  res_mat[,1] = mat[,1] / sd(mat[,1]) * sd_coord[1]
  res_mat[,2] = mat[,2] / sd(mat[,2]) * sd_coord[2]
  return(res_mat)
}

rotate_matrix = function(mat, rotate = 0) {
  #@F ----------------------------------------
  #@F Function 'rotate_matrix'
  #@F Args (Input): mat, rotate
  #@F   mat = two-column matrix of coordinates [x,y]
  #@F   rotate = single numeric: how many radians to rotate the matrix
  #@F Purpose: Re-locates all the coordinates in mat (rotates them 'rotate' radians)
  #@F Output: Two-column matrix of coordinates
  #@F ----------------------------------------
  
  rot_mat = matrix(0, nrow = 2, ncol = 2)
  rot_mat[1,1] = cos(rotate)
  rot_mat[1,2] = sin(rotate)
  rot_mat[2,1] = -sin(rotate)
  rot_mat[2,2] = cos(rotate)
  
  res = rot_mat %*% t(mat)
  return(t(res))
}

edgeMaker <- function(whichRow, len = 100, curved = TRUE, layoutCoordinates, adjacencyList){
  #@F ----------------------------------------
  #@F Function 'edgeMaker'
  #@F Args (Input): whichRow, len, curved, layoutCoordinates, adjacencyList
  #@F   whichRow = integer, referring to which row of 'adjacencyList' to draw
  #@F   len = integer: number of edge segments to create
  #@F   curved = logical: draw curved edge or straight edge
  #@F   layoutCoordinates = coordinates of the nodes in network
  #@F   adjacencyList = matrix (two columns, of pairs of adjacent edges)
  #@F Purpose: Creates the edge object for plotting curved edges (or straight edges)
  #@F Output: A data frame containing information to plot the edge
  #@F Notes: # Function pulled from 
  #@F   http://is-r.tumblr.com/post/38459242505/beautiful-network-diagrams-with-ggplot2
  #@F   Some adjustments made (removed need for global variable)
  #@F ----------------------------------------
  
  fromC <- layoutCoordinates[adjacencyList[whichRow, 1], ]  # Origin
  toC <- layoutCoordinates[adjacencyList[whichRow, 2], ]  # Terminus
  
  # Add curve:
  graphCenter <- colMeans(layoutCoordinates)  # Center of the overall graph
  bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
  distance1 <- sum((graphCenter - bezierMid)^2)
  if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
    bezierMid <- c(toC[1], fromC[2])
  }  # To select the best Bezier midpoint
  bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
  if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
  
  edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
                            c(fromC[2], bezierMid[2], toC[2]),  # X & y
                            evaluation = len))  # Bezier path coordinates
  edge$Sequence <- 1:len  # For size and colour weighting in plot
  edge$Group <- paste(adjacencyList[whichRow, 1:2], collapse = ">")
  return(edge)
}

generate_default_distmatrix = function(net) {
  #@F ----------------------------------------
  #@F Function 'generate_default_distmatrix'
  #@F Args (Input): net = network object (nonstandard class)
  #@F Purpose: This generates geodesic distance matrix as used in 
  #@F   gplot.layout.kamadakawai by default
  #@F Output: Distance matrix
  #@F Notes: Copied code (with no significant modification) from gplot.layout.kamadakawai()
  #@F ----------------------------------------
  
  d = symmetrize(net, return.as.edgelist = TRUE)
  d[, 3] <- 1/d[, 3]
  elen <- geodist(d, ignore.eval = FALSE)$gdist
  elen[elen == Inf] <- max(elen[is.finite(elen)]) * 1.25
  return(elen)
}
