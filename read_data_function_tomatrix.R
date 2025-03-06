library(EBImage)  
library(gtools)   
library(OpenImageR)

read_all_images <- function(folder_path) {
  image_files <- list.files(folder_path, full.names = TRUE, pattern = "\\.(jpg|png|jpeg|tiff|bmp)$", ignore.case = TRUE)
  
  # Sort filenames
  image_files <- mixedsort(image_files)  
  
  # Extract labels from filenames
  extract_label <- function(filename) {
    base_name <- tools::file_path_sans_ext(basename(filename))  
    label <- gsub("[^0-9]", "", base_name)  # Extract numeric part
    return(as.numeric(label))
  }
  
  read_data <- function(image_path) {
    img <- readImage(image_path)  
    red_aux   <- as.vector(img[,,1])
    green_aux <- as.vector(img[,,2])
    blue_aux  <- as.vector(img[,,3])
    
    # Combine all channels into a single row
    img_vector <- c(red_aux, green_aux, blue_aux)
    return(img_vector)  # Return as a vector
  }
  
  image_list <- lapply(image_files, function(file) {
    img_data <- read_data(file)  
    label <- extract_label(file)  # Extract label
    return(c(img_data, label))  # Append label at the end
  })
  
  ax <- do.call(rbind, image_list)  # Convert list to matrix
  return(ax)
}

folder_path <- "Training_alpha"
ax <- read_all_images(folder_path)