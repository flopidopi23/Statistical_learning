# clear all vars
rm(list = ls())
gc()
library(OpenImageR)


PCA.fun = function(X){
  n = nrow(X)
  mu = colMeans(X)
  # center data
  G = sweep(X, 2, mu, FUN = "-")
  # Compute the covariance matrix
  cov_matrix = (t(G) %*% G) / (n - 1)
  ## Calculate Eigenvalues and Eigenvectors and sort
  eig = eigen(cov_matrix)
  eig_val = eig$values
  eig_vec = eig$vectors
  # sort
  sort_index <- order(eig_val, decreasing = TRUE)
  eig_val_sorted <- eig_val[sort_index]
  eig_vec_sorted <- eig_vec[, sort_index] 
  #variability of each PC
  D = eig_val_sorted/sum(eig_val_sorted)
  
  ## Calculate Principal Components 
  pc1 = G %*% eig_vec_sorted[,1]
  pc1_image = matrix(pc1,nrow=nrow(red), ncol=ncol(green)) 
  
  pc2 = G %*% eig_vec_sorted[,2]
  pc2_image = matrix(pc2,nrow=nrow(red), ncol=ncol(green))
  
  pc3 = G %*% eig_vec_sorted[,3]
  pc3_image = matrix(pc3,nrow=nrow(red), ncol=ncol(green))
  return(list("Eigen Vector"= eig_vec_sorted,"D"=D,"PC1"=pc1,"PC2"=pc2,"PC3"=pc3))
}


Im = readImage("Training/1AT.jpg")

red = Im[,,1]
green=Im[,,2]
blue=Im[,,3]

test = cbind(as.vector(red), as.vector(green), as.vector(blue))
PCA.fun(test)
aux=PCA.fun(test)
aux$D
aux$`Eigen Vector`
aux$PC1
