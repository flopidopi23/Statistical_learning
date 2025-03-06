# clear all vars
rm(list = ls())
gc()
library(OpenImageR)
library(dplyr)
library(MASS) #to inverse matrix without error of singularity


FDA.fun <- function(X, labels) {
  labels.x <- as.numeric(as.factor(labels))
  #datafrma to matrix
  features <- as.matrix(X)
  
  # Compute total mean
  mean_total <- colMeans(features)
  n <- ncol(features)  # Number of features
  n_samples <- nrow(features)  # Number of samples
  
  # Number of classes
  classes <- unique(labels.x)
  K <- length(classes)
  
  #  Sw (within-class scatter) and Sb (between-class scatter)
  Sw <- matrix(0, n, n)
  Sb <- matrix(0, n, n)
  
  for (class in classes) {
    # Select data for the current class
    class_idx <- which(labels.x == class)
    class_data <- features[class_idx, , drop = FALSE]
    
    # Mean within class
    mean_class <- colMeans(class_data)
    
    #  scatter within class
    Si <- t(class_data - mean_class) %*% (class_data - mean_class)
    Sw <- Sw + Si
    
    # Compute between-class scatter
    Nk <- nrow(class_data)
    mean_diff <- matrix(mean_class - mean_total, ncol = 1)
    Sb <- Sb + Nk * (mean_diff %*% t(mean_diff))
  }
  
  #resolve problems with singularity
  Sw_inv <- ginv(Sw)
  #eigen
  eig <- eigen(Sw_inv %*% Sb)
  
  num_components <- min(K-1, ncol(Sw))
  P <- eig$vectors[, 1:num_components, drop = FALSE]
  
  # Eigenvalues
  eigenvalues <- eig$values[1:num_components]
  
  # Compute discriminant scores (D)
  D <- eigenvalues / sum(eigenvalues)
  
  # Project data onto new discriminant subspace
  transformed_data <- features %*% P
  
  return(list("Projection" = P, "D" = D, "Transformed Data" = transformed_data))
}

# Example usage
# Assume df is your 150x108000 dataframe with labels in the last column
# df$Label should be a factor or numeric class labels

result <- FDA.fun(aux_data[,1:12], aux_data$Label)
length(result$D)
