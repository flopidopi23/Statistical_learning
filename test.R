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
uno = read_data("Training/1AT.jpg")
dos = read_data("Training/1BT.jpg")
head(uno)
head(dos)
data = rbind(uno,dos)
