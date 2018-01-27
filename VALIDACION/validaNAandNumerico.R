aaa <- BCCA_TDP_DATA_PURE

aaa$edad_n<- validaNAandNumerico(aaa$edad_n)



validaNAandNumerico <- function(vector){
  nuevoVector <- ifelse(is.na(vector) | !is.numeric(vector) , paste('ERROooR',vector,sep='|'), vector)
  return(nuevoVector)                      
}

aaa$edad_n


aaa$edad_n <- ifelse(
  
  
  #Validando espacios en blanco
  is.na(aaa$edad_n) |
    
    
    #Validando que el campo solo tenga valor "Postpago"
    !is.numeric(aaa$edad_n)
  
  
  ,paste('ERROR',aaa$edad_n,sep='|') , aaa$edad_n)

View(aaa)
