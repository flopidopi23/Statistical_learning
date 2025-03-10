# clear all vars
rm(list = ls())
gc()
library(OpenImageR)
library(dplyr)

PCA.fun = function(X){
  X = X %>% dplyr::select(-c("Label","ID"))
  X = as.matrix(X)
  n = nrow(X)
  mu = colMeans(X)
  # center data
  G = sweep(X, 2, mu, FUN = "-")
  # Compute the covariance matrix
  cov_matrix =(1/(n-1))*(G %*% t(G))
  ## Calculate Eigenvalues and Eigenvectors and sort
  eig = eigen(cov_matrix)
  eig_val = eig$values
  eig_vec = eig$vectors
  ei_vec_large = t(G)%*%eig_vec
  # sort
  sort_index <- order(eig_val, decreasing = TRUE)
  eig_val_sorted <- eig_val[sort_index]
  eig_vec_sorted <- ei_vec_large[, sort_index] 
  #variability of each PC
  D = eig_val_sorted/sum(eig_val_sorted)
  return(list("Eigen Vector"= eig_vec_sorted,"D"=D))
  }

#test = PCA.fun(ax)
#aux_data = as.data.frame(test)
#aux_data$Label = ax$Label
#cumsum(test$D)
