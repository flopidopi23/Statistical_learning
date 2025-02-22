library(OpenImageR)
Im = readImage("Training/1AT.jpg")
red = Im[,,1]
green = Im[,,2]
blue = Im[,,3]

data = cbind(as.vector(red),as.vector(green),as.vector(blue))

read_data = function(nombre){
  aux_read = readImage(nombre)
  red_aux = Im[,,1]
  green_aux = Im[,,2]
  blue_aux = Im[,,3]
  data_aux = data.frame(Red = as.vector(red_aux),
                        Green = as.vector(green_aux),
                        Blue = as.vector(blue_aux))
  return(data_aux)
}
uno = read_data("C:/Users/flore/Desktop/git/Statistical_learning/Training/1AT.jpg")
dos = read_data("C:/Users/flore/Desktop/git/Statistical_learning/Training/1BT.jpg")
ax = rbind(uno,dos)
head(uno)
head(dos)
data = rbind(uno,dos)

read_data <- function(image_path) {
  img <- readImage(image_path)  
  red_aux   <- as.vector(img[,,1])
  green_aux <- as.vector(img[,,2])
  blue_aux  <- as.vector(img[,,3])
  
  # Combine all channels into a single row
  img_vector <- c(red_aux, green_aux, blue_aux)
  
  return(as.data.frame(t(img_vector)))  # Transpose to make it a row
}
