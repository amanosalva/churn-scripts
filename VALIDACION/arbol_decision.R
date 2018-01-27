#Árbol de decisión
#Prueba con el árbol de decisión - PREPAGO
library("rpart")
library("rpart.plot")
library("C50")

#Subiendo el dataset Postpago - transformado
path_origen_postpago <- "C:/Byte/Machine Learning/BCCA - Oficial/1. Datasets/2. Transformado/80% train/BCCA_TDP_DATA_TRANS_POSTPAGO_80_target.csv"

BCCA_TDP_DATA_POSTPAGO <- read_csv(path_origen_postpago, 
                                  col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                                   "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                                   "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                                   "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                                   "fld146_n"= col_double(), "cant1m_n"= col_double(), 
                                                   "mont1m_n"= col_double(), "cant3m_n"= col_double(), 
                                                   "mont3m_n"= col_double(), "cant6m_n"= col_double(), 
                                                   "mont6m_n"= col_double(), "ctotal_n"= col_double(), 
                                                   "mtotal_n"= col_double()))


#Creamos set de entrenamiento y de test
#80% entrenamiento - 20% validación
ind        <- sample(2,nrow(BCCA_TDP_DATA_POSTPAGO), replace=TRUE, prob=c(0.8, 0.2))
trainData  <- BCCA_TDP_DATA_POSTPAGO[ind==1, ]
testData   <- BCCA_TDP_DATA_POSTPAGO[ind==2, ]

ArbolRpart <- rpart(target_c ~ ., method = "class", data = trainData)

print(ArbolRpart)
rpart.plot(ArbolRpart)  # extra=4:probabilidad de observaciones por clase

testPredRpart <- predict(ArbolRpart, newdata = testData, type = "class")

#Visualizamos una matriz de confusión
table(testPredRpart, testData$target_c)

#Estadística del modelo.
#Calculamos el % de aciertos.
sum(testPredRpart == testData$target_c)/length(testData$target_c)*100
