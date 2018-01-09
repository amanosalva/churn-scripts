#Función que reconoce el producto según el primer valor del campo de la trama
identificaProducto <- function(trama,tokenp){
  registro <- as.vector(do.call(rbind,strsplit(trama,tokenp)))
  #El primer campo del vector es el producto
  producto <- registro[1]
  
  
  return(producto)
}

