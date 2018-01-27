#Partir archivos en TRAIN y TEST

#Paths de prueba:
#path_destino_train <- "C:/Byte/Machine Learning/BCCA - Oficial/1. Datasets/1. No transformado/80% train/BCCA_TDP_DATA_POSTPAGO_80_target.csv"
#path_destino_test  <- "C:/Byte/Machine Learning/BCCA - Oficial/1. Datasets/1. No transformado/20% test/BCCA_TDP_DATA_POSTPAGO_20_target.csv"

trainAndTest <- function(dataset, train_prob, test_prob, path_destino_train, path_destino_test){
  
  if(train_prob + test_prob != 1){
    message("La suma de los porcentajes debe ser 1")
  }else{

      ind            <- sample(2,nrow(dataset), replace=TRUE, prob=c(train_prob, test_prob))
      dataset_TRAIN  <- dataset[ind==1, ]
      dataset_TEST   <- dataset[ind==2, ]
      
      write.csv(dataset_TRAIN, path_destino_train, row.names = FALSE)
      write.csv(dataset_TEST, path_destino_test, row.names = FALSE)
   }
  
}

