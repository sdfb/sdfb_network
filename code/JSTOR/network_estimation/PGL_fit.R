library(glmnet)

load("")

fit <- function(y, X) {
  glmnet(X, y,
       family="poisson", lambda = 1, #new_lambda, 
       maxit = 500, #new_maxit , 
       thres = 10^(-6))
}