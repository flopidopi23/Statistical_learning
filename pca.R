# clear all vars
rm(list = ls())
gc()



### Get Data
Im = readImage("Training/1AT.jpg")

red = Im[,,1]
green=Im[,,2]
blue=Im[,,3]

X = cbind(as.vector(red), as.vector(green), as.vector(blue))

### Helper Functions

column_means = function(X) {
  n = nrow(X)  # Number of rows (observations)
  return(apply(X, 2, sum) / n)  # Sum each column and divide by n
}

### PCA
## Calculate Covariance Matrix

n = nrow(X)
mu = colMeans(X)

# center
X_centered = sweep(X, 2, mu, FUN = "-")

# Compute the covariance matrix
cov_matrix = (t(X_centered) %*% X_centered) / (n - 1)


## Calculate Eigenvalues and Eigenvectors and sort
eig = eigen(cov_matrix)
eig_val = eig$values
eig_vec = eig$vectors

# sort
sort_index <- order(eig_val, decreasing = TRUE)
eig_val_sorted <- eig_val[sort_index]
eig_vec_sorted <- eig_vec[, sort_index] 


## Calculate Principal Components & print
pc1 = X_centered %*% eig_vec_sorted[,1]
pc1_image = matrix(-pc1,nrow=nrow(red), ncol=ncol(green)) #### sign change WHY ?

pc2 = X_centered %*% eig_vec_sorted[,2]
pc2_image = matrix(pc2,nrow=nrow(red), ncol=ncol(green))

pc3 = X_centered %*% eig_vec_sorted[,3]
pc3_image = matrix(pc3,nrow=nrow(red), ncol=ncol(green))

imageShow(t(pc1_image))
imageShow(t(pc2_image))
imageShow(t(pc3_image))

# show

### Verify similarity with library
out = princomp(X)

pc1_package = matrix(out$scores[,1],nrow=nrow(red), ncol=ncol(green))
pc2_package = matrix(out$scores[,2],nrow=nrow(red), ncol=ncol(green))
pc3_package = matrix(out$scores[,3],nrow=nrow(red), ncol=ncol(green))

cor(pc1, out$scores[,1])
cor(pc2, out$scores[,2])
cor(pc3, out$scores[,3])





