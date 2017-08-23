##@S This file contains functions to determine node locations.
##@S Code in here is not optimized...

randomize_loc_inside = function(type = c("circle", "rectangle"), rad = 1, center = c(0,0), length = 1, width = 1) {
  ## TODO: [Document] Function
  
  if (type == "circle") {
    badloc = TRUE
    loc = c(3000, 3000)
    while(badloc) {
      loc = runif(n=2, min = -rad, max = rad)
      if (sqrt(sum(loc^2)) <= rad) {
        badloc = FALSE
      }
    }
    
    loc = loc + center
  } else if (type == "rectangle") {
    loc = c(center[1] + runif(n=1, min = -length/2, max = length/2),
      center[2] + runif(n=1, min = -width/2, max = width/2))
  }
  return(loc)
}

##randomize_loc_inside(type = "rectangle")
##randomize_loc_inside(type = "circle", center = c(3,1), rad = 0.1)

assign_locations = function(
  node_groups, group_rads, group_centers,
  edge_list, pG, pC, pE, pM) {
  
  ## TODO: Allow arbitrary area functions... not necessarily circular.
  ## TODO: [Document] Function
  
  ## first: find optimal-ish locations within each group's area
  ## is this section even necessary? does it change runtime much? [probalby not...?]
  node_locs = matrix(0, nrow = length(node_groups), ncol = 2)
  node_locs = t(sapply(1:length(node_groups),
    function(x) {return(
      randomize_loc_inside(type = "circle", rad = group_rads[node_groups[x]],
                           center = group_centers[node_groups[x],]))}))

  ini_guess = as.vector(node_locs)
  ctrs = as.vector(group_centers[node_groups,])


  rs = group_rads[node_groups]

  edge_mat = edge_list # a list of all pairs of edges
  minfx = function(x) {
    xy = matrix(x, ncol = 2)
    ctr_xy = matrix(ctrs, ncol = 2)
    ctr_dists = sum(apply( (xy - ctr_xy)/rs, 1, function(x) { max(sqrt(sum(x^2)), 1) - 1 }))

    btwn_dists = sum(pM / dist(xy)^2)

    edge_dists = sum(
      apply(xy[edge_list[,1],] - xy[edge_list[,2],], 1, function(x) {sqrt(sum(x^2))})
      )
    
    return(pG * ctr_dists + pC * btwn_dists)
  }

  opt = optim(par = ini_guess, fn = minfx, method = "BFGS")
  opt_res = matrix(opt$par, ncol = 2)
  return(as.data.frame(opt_res))
}


## resxy = assign_locations(node_groups = c(1,1,1,2,2,2,2,2,3),
##                  group_rads = c(0.3,0.5,0.1),
##                  group_centers = rbind(c(2,0), c(-2,0), c(0,0)))

## xy = matrix(resxy$par, ncol = 2)
## plot(xy, col = c(1,1,1,2,2,2,2,2,3), xlim = c(-3, 3), ylim = c(-3, 3))
