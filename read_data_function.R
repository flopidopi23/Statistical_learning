library(EBImage)  
library(gtools)   

read_all_images <- function(folder_path) {
  image_files <- list.files(folder_path, full.names = TRUE, pattern = "\\.(jpg|png|jpeg|tiff|bmp)$", ignore.case = TRUE)
  
  # Sort filenames
  image_files <- mixedsort(image_files)  
  
  # Extract labels from filenames
  extract_label <- function(filename) {
    base_name <- tools::file_path_sans_ext(basename(filename))  
    label <- gsub("[^0-9]", "", base_name)  # Extract numeric part
    return(label)
  }
  
  image_list <- lapply(image_files, function(file) {
    img_data <- read_data(file)  
    img_data$Label <- extract_label(file)  # Assign label 
    return(img_data)
  })
  
  ax <- do.call(rbind, image_list)
  return(ax)
}

folder_path <- "Training"#"C:/Users/flore/Desktop/git/Statistical_learning/Training"
ax <- read_all_images(folder_path)
