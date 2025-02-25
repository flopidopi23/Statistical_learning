#libraries
library(OpenImageR)
library(EBImage)
library(grid)
# Kmeans

# Load Image
load_and_flatten <- function(image_path, target_size = c(100, 100)) {
  img <- readImage(image_path)          
  img <- resize(img, w = target_size[1], h = target_size[2])  # Resize
  img <- channel(img, "gray") # greyscaled       
  img_vector <- as.vector(img)
  return(img_vector)
}

# Load training images
image_paths <- list.files("Training_alpha", full.names = TRUE)
train_data <- t(sapply(image_paths, load_and_flatten))  # each row = flattend image
train_labels <- rep(1, length(image_paths))  # Label all training images as '1' (person's face)

#load_and_flatten_rgb <- function(image_path, target_size = c(100, 100)) {
#  img <- readImage(image_path)  # Load the image
#  img <- resize(img, w = target_size[1], h = target_size[2])  # Resize
  
#  # Flatten the RGB image while keeping all channels
#  img_vector <- as.vector(img)  # Converts the 3D array into a 1D vector
  
#  return(img_vector)
#}

#train_data2 <- t(sapply(image_paths, load_and_flatten_rgb))  # each row = flattend image

### KNN Algo
knn_manual <- function(train_data, train_labels, test_data, k, threshold) {
  predictions <- c()
  
  # TODO: k <= n_images
  
  for (i in 1:nrow(test_data)) {
    distances <- as.matrix(dist(rbind(test_data[i, ], train_data)))[1, -1]
    k_neighbors <- order(distances)[1:k]
    neighbor_labels <- train_labels[k_neighbors] # get labels of neighbors
    predicted_label <- names(sort(table(neighbor_labels), decreasing = TRUE))[1] # make frequency table and pick most frequent 
    
    # Check treshhold for class 0
    avg_distance <- mean(distances[k_neighbors])
    
    if (avg_distance > threshold) {
      predicted_label <- "0"
    } else {
      # Majority vote among k neighbors
      predicted_label <- names(sort(table(neighbor_labels), decreasing = TRUE))[1]
    }
    
    predictions <- c(predictions, predicted_label)
  }
  
  return(predictions)
}

#debug(knn_manual)


### Set threshold for valid classification
estimate_threshold_percentile <- function(train_data, train_labels, percentile = 0.90) {
  class_1 <- train_data[train_labels == 1, , drop = FALSE]
  dist_1 <- as.matrix(dist(class_1))
  
  # Extract upper triangle distances
  threshold <- quantile(dist_1[upper.tri(dist_1)], percentile)
  
  return(threshold)
}


test_image <- load_and_flatten("Test_alpha/20BT.jpg")  # Load & flatten test image
test_data <- matrix(test_image, nrow = 1)  # Ensure test data format


# Compute threshold
threshold <- estimate_threshold_percentile(train_data, train_labels,percentile= 0.90)
print(threshold)

k <- 4  # Choose k value
prediction <- knn_manual(train_data, train_labels, test_data, k,threshold=threshold)

print(paste("Person_ID:", prediction))
