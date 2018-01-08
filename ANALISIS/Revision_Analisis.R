#autor: amanosalva
#Data generada con las correcciones de José Coca y José Neyra
#Script que genera dataset puro, a partir del dataset crudo BCCA_TDP_DATA.
#Activar librería ByteAnalyticsUtils

#Importación de data cruda.
library(ByteAnalitycsUtils)
library(readr)
BCCA_TDP_DATA_RAW <- read_csv("D:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/Corrección final dataset/BCCA_TDP_DATA_RAW.csv", 
                              col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                               "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                               "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                               "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                               "fld146_n"= col_double(),
                                               "cant1m_n"= col_double(), "mont1m_n"= col_double(), "cant3m_n"= col_double(), "mont3m_n"= col_double(), "cant6m_n"= col_double(), "mont6m_n"= col_double(), "ctotal_n"= col_double(), "mtotal_n"= col_double()))
BCCA_TDP_DATA_RAW_SAVE <- BCCA_TDP_DATA_RAW
BCCA_TDP_DATA_RAW <- eliminaCaracteristica(BCCA_TDP_DATA_RAW, c("X1"))


#Visualización de campos nulos.
exportaValoresFaltantes(dataset = BCCA_TDP_DATA_RAW, path = "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Documentos/BCCA_TDP_DATA - Documentación/CuadroResumenNA_cor_.csv")


#Visualización general de la data.
#Cabecera
exporta100primeros(BCCA_TDP_DATA_RAW,"C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Documentos/BCCA_TDP_DATA - Documentación/BCCA_TDP_DATA_RAW_HEAD_cor.csv")

#Cola
exporta100ultimos(BCCA_TDP_DATA_RAW,"C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Documentos/BCCA_TDP_DATA - Documentación/BCCA_TDP_DATA_RAW_TAIL_cor.csv")

#Características que serán eliminadas por poseer el tag "NO VA"
caracteristicasInutiles <- c("tcnfol_n","tccpro_c","tcclip_c","tcctse_c","tctico_c",
                             "cordir_n","tcinyn_c","ivtipi_c","tcsera_c","tccesq_n",
                             "tcaesq_n","tcmesq_n","tcdesq_n","ivgase_c","tcdsga_c",
                             "tcccli_n","tccdia_n","tccmes_n","tccano_n","tcndia_n",
                             "tcnmes_n","tcnano_n","tcesci_c","tcsexo_c", "tcntel_n")
BCCA_TDP_DATA_RAW <- eliminaCaracteristica(BCCA_TDP_DATA_RAW,caracteristicasInutiles)

#Características que serán eliminadas por valores faltantes y tienen poca relevancia en la generación de modelos.
caracteristicasInutiles <- c("genero_c","ecivil_c","X1_1","X2","X3","gama_c","depart_c",
                             "provi_c","disti_c", "tcsecn_c")
BCCA_TDP_DATA_RAW <- eliminaCaracteristica(BCCA_TDP_DATA_RAW,caracteristicasInutiles)
rm(caracteristicasInutiles)

#Características que serán eliminadas por relación directa con el target
caracteristicasInutiles <- c("f3_n","f4_n","f5_n","f6_n","f7_n","f8_n","f9_n","f10_n",
  "f11_n","f12_n","f13_n","f14_n","f15_n","f16_n","f17_n",
  "f18_n","f19_n","f20_n","f21_n")

BCCA_TDP_DATA_RAW <- eliminaCaracteristica(BCCA_TDP_DATA_RAW,caracteristicasInutiles)

names(BCCA_TDP_DATA_RAW)
rm(caracteristicasInutiles)

#Transformación de los registros NA a 0 en características que expresan cantidad, (según lo documentado como comentario en la sección CuadroResumenNA).
BCCA_TDP_DATA_RAW$cant1m_n <- ifelse(is.na(BCCA_TDP_DATA_RAW$cant1m_n), 0, BCCA_TDP_DATA_RAW$cant1m_n)
sum(is.na(BCCA_TDP_DATA_RAW$cant1m_n))
BCCA_TDP_DATA_RAW$mont1m_n <- ifelse(is.na(BCCA_TDP_DATA_RAW$mont1m_n), 0, BCCA_TDP_DATA_RAW$mont1m_n)
sum(is.na(BCCA_TDP_DATA_RAW$mont1m_n))
BCCA_TDP_DATA_RAW$cant3m_n <- ifelse(is.na(BCCA_TDP_DATA_RAW$cant3m_n), 0, BCCA_TDP_DATA_RAW$cant3m_n)
sum(is.na(BCCA_TDP_DATA_RAW$cant3m_n))
BCCA_TDP_DATA_RAW$mont3m_n <- ifelse(is.na(BCCA_TDP_DATA_RAW$mont3m_n), 0, BCCA_TDP_DATA_RAW$mont3m_n)
sum(is.na(BCCA_TDP_DATA_RAW$mont3m_n))
BCCA_TDP_DATA_RAW$cant6m_n <- ifelse(is.na(BCCA_TDP_DATA_RAW$cant6m_n), 0, BCCA_TDP_DATA_RAW$cant6m_n)
sum(is.na(BCCA_TDP_DATA_RAW$cant6m_n))
BCCA_TDP_DATA_RAW$mont6m_n <- ifelse(is.na(BCCA_TDP_DATA_RAW$mont6m_n), 0, BCCA_TDP_DATA_RAW$mont6m_n)
sum(is.na(BCCA_TDP_DATA_RAW$mont6m_n))
BCCA_TDP_DATA_RAW$ctotal_n <- ifelse(is.na(BCCA_TDP_DATA_RAW$ctotal_n), 0, BCCA_TDP_DATA_RAW$ctotal_n)
sum(is.na(BCCA_TDP_DATA_RAW$ctotal_n))
BCCA_TDP_DATA_RAW$mtotal_n <- ifelse(is.na(BCCA_TDP_DATA_RAW$mtotal_n), 0, BCCA_TDP_DATA_RAW$mtotal_n)
sum(is.na(BCCA_TDP_DATA_RAW$mtotal_n))
BCCA_TDP_DATA_RAW$costpl_n <- ifelse(is.na(BCCA_TDP_DATA_RAW$costpl_n), 0, BCCA_TDP_DATA_RAW$costpl_n)
sum(is.na(BCCA_TDP_DATA_RAW$costpl_n))
BCCA_TDP_DATA_RAW$cnttoc_n <- ifelse(is.na(BCCA_TDP_DATA_RAW$cnttoc_n), 0, BCCA_TDP_DATA_RAW$cnttoc_n)
sum(is.na(BCCA_TDP_DATA_RAW$cnttoc_n))
BCCA_TDP_DATA_RAW$cntnrc_n <- ifelse(is.na(BCCA_TDP_DATA_RAW$cntnrc_n), 0, BCCA_TDP_DATA_RAW$cntnrc_n)
sum(is.na(BCCA_TDP_DATA_RAW$cntnrc_n))
BCCA_TDP_DATA_RAW$cnttmc_n <- ifelse(is.na(BCCA_TDP_DATA_RAW$cnttmc_n), 0, BCCA_TDP_DATA_RAW$cnttmc_n)
sum(is.na(BCCA_TDP_DATA_RAW$cnttmc_n))


#Proceso de separación de la data por producto.
BCCA_TDP_DATA_RAW_PREPAGO<- subset(BCCA_TDP_DATA_RAW, BCCA_TDP_DATA_RAW$produc_c == "prepago")
BCCA_TDP_DATA_RAW_POSTPAGO<- subset(BCCA_TDP_DATA_RAW, BCCA_TDP_DATA_RAW$produc_c == "postpago")
BCCA_TDP_DATA_RAW_CONTROL<- subset(BCCA_TDP_DATA_RAW, BCCA_TDP_DATA_RAW$produc_c == "control")


#Visualización de campos nulos PREPAGO.
exportaValoresFaltantes(dataset = BCCA_TDP_DATA_RAW_PREPAGO, path = "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Documentos/BCCA_TDP_DATA - Documentación/Análisis por producto/PREPAGO/CuadroResumenNA_BCCA_TDP_DATA_PREPAGO.csv")

#Visualización de campos nulos POSTPAGO.
exportaValoresFaltantes(dataset = BCCA_TDP_DATA_RAW_POSTPAGO, path = "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Documentos/BCCA_TDP_DATA - Documentación/Análisis por producto/POSTPAGO/CuadroResumenNA_BCCA_TDP_DATA_POSTPAGO.csv")

#Visualización de campos nulos CONTROL.
exportaValoresFaltantes(dataset = BCCA_TDP_DATA_RAW_CONTROL, path = "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Documentos/BCCA_TDP_DATA - Documentación/Análisis por producto/CONTROL/CuadroResumenNA_BCCA_TDP_DATA_CONTROL.csv")



################################################################################################################################################################################
#Análisis de valores faltantes para cada producto respecto al target.

#Prepago:

#Edad
length(BCCA_TDP_DATA_RAW_PREPAGO$edad_n[is.na(BCCA_TDP_DATA_RAW_PREPAGO$edad_n) & BCCA_TDP_DATA_RAW_PREPAGO$target_c == "0"])
length(BCCA_TDP_DATA_RAW_PREPAGO$edad_n[is.na(BCCA_TDP_DATA_RAW_PREPAGO$edad_n) & BCCA_TDP_DATA_RAW_PREPAGO$target_c == "1"])

#antig_n
length(BCCA_TDP_DATA_RAW_PREPAGO$antig_n[is.na(BCCA_TDP_DATA_RAW_PREPAGO$antig_n) & BCCA_TDP_DATA_RAW_PREPAGO$target_c == "0"])
length(BCCA_TDP_DATA_RAW_PREPAGO$antig_n[is.na(BCCA_TDP_DATA_RAW_PREPAGO$antig_n) & BCCA_TDP_DATA_RAW_PREPAGO$target_c == "1"])

#numlin_n
length(BCCA_TDP_DATA_RAW_PREPAGO$numlin_n[is.na(BCCA_TDP_DATA_RAW_PREPAGO$numlin_n) & BCCA_TDP_DATA_RAW_PREPAGO$target_c == "0"])
length(BCCA_TDP_DATA_RAW_PREPAGO$numlin_n[is.na(BCCA_TDP_DATA_RAW_PREPAGO$numlin_n) & BCCA_TDP_DATA_RAW_PREPAGO$target_c == "1"])

#eqanti_n
length(BCCA_TDP_DATA_RAW_PREPAGO$eqanti_n[is.na(BCCA_TDP_DATA_RAW_PREPAGO$eqanti_n) & BCCA_TDP_DATA_RAW_PREPAGO$target_c == "0"])
length(BCCA_TDP_DATA_RAW_PREPAGO$eqanti_n[is.na(BCCA_TDP_DATA_RAW_PREPAGO$eqanti_n) & BCCA_TDP_DATA_RAW_PREPAGO$target_c == "1"])

#Postpago:

#Edad
length(BCCA_TDP_DATA_RAW_POSTPAGO$edad_n[is.na(BCCA_TDP_DATA_RAW_POSTPAGO$edad_n) & BCCA_TDP_DATA_RAW_POSTPAGO$target_c == "0"])
length(BCCA_TDP_DATA_RAW_POSTPAGO$edad_n[is.na(BCCA_TDP_DATA_RAW_POSTPAGO$edad_n) & BCCA_TDP_DATA_RAW_POSTPAGO$target_c == "1"])

#antig_n
length(BCCA_TDP_DATA_RAW_POSTPAGO$antig_n[is.na(BCCA_TDP_DATA_RAW_POSTPAGO$antig_n) & BCCA_TDP_DATA_RAW_POSTPAGO$target_c == "0"])
length(BCCA_TDP_DATA_RAW_POSTPAGO$antig_n[is.na(BCCA_TDP_DATA_RAW_POSTPAGO$antig_n) & BCCA_TDP_DATA_RAW_POSTPAGO$target_c == "1"])

#numlin_n
length(BCCA_TDP_DATA_RAW_POSTPAGO$numlin_n[is.na(BCCA_TDP_DATA_RAW_POSTPAGO$numlin_n) & BCCA_TDP_DATA_RAW_POSTPAGO$target_c == "0"])
length(BCCA_TDP_DATA_RAW_POSTPAGO$numlin_n[is.na(BCCA_TDP_DATA_RAW_POSTPAGO$numlin_n) & BCCA_TDP_DATA_RAW_POSTPAGO$target_c == "1"])

#eqanti_n
length(BCCA_TDP_DATA_RAW_POSTPAGO$eqanti_n[is.na(BCCA_TDP_DATA_RAW_POSTPAGO$eqanti_n) & BCCA_TDP_DATA_RAW_POSTPAGO$target_c == "0"])
length(BCCA_TDP_DATA_RAW_POSTPAGO$eqanti_n[is.na(BCCA_TDP_DATA_RAW_POSTPAGO$eqanti_n) & BCCA_TDP_DATA_RAW_POSTPAGO$target_c == "1"])


#Control:

#Edad
length(BCCA_TDP_DATA_RAW_CONTROL$edad_n[is.na(BCCA_TDP_DATA_RAW_CONTROL$edad_n) & BCCA_TDP_DATA_RAW_CONTROL$target_c == "0"])
length(BCCA_TDP_DATA_RAW_CONTROL$edad_n[is.na(BCCA_TDP_DATA_RAW_CONTROL$edad_n) & BCCA_TDP_DATA_RAW_CONTROL$target_c == "1"])


#antig_n
length(BCCA_TDP_DATA_RAW_CONTROL$antig_n[is.na(BCCA_TDP_DATA_RAW_CONTROL$antig_n) & BCCA_TDP_DATA_RAW_CONTROL$target_c == "0"])
length(BCCA_TDP_DATA_RAW_CONTROL$antig_n[is.na(BCCA_TDP_DATA_RAW_CONTROL$antig_n) & BCCA_TDP_DATA_RAW_CONTROL$target_c == "1"])

#numlin_n
length(BCCA_TDP_DATA_RAW_CONTROL$numlin_n[is.na(BCCA_TDP_DATA_RAW_CONTROL$numlin_n) & BCCA_TDP_DATA_RAW_CONTROL$target_c == "0"])
length(BCCA_TDP_DATA_RAW_CONTROL$numlin_n[is.na(BCCA_TDP_DATA_RAW_CONTROL$numlin_n) & BCCA_TDP_DATA_RAW_CONTROL$target_c == "1"])

#eqanti_n
length(BCCA_TDP_DATA_RAW_CONTROL$eqanti_n[is.na(BCCA_TDP_DATA_RAW_CONTROL$eqanti_n) & BCCA_TDP_DATA_RAW_CONTROL$target_c == "0"])
length(BCCA_TDP_DATA_RAW_CONTROL$eqanti_n[is.na(BCCA_TDP_DATA_RAW_CONTROL$eqanti_n) & BCCA_TDP_DATA_RAW_CONTROL$target_c == "1"])

#Conclusiones del análisis:
## La relación de valores faltantes para no portados y valores faltantes para portados que posee cada producto es casi de 1 a 2  ()
## La variable numlin_n no aportará al modelo ya que la mayoría de los valores faltantes son para los faltantes.
#(Esto debe ir documentado en alguna parte); por lo tanto esa variable será eliminada de los tres productos.

####################################################################################################################################

BCCA_TDP_DATA_RAW_PREPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_RAW_PREPAGO, c("numlin_n"))
BCCA_TDP_DATA_RAW_POSTPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_RAW_POSTPAGO, c("numlin_n"))
BCCA_TDP_DATA_RAW_CONTROL <- eliminaCaracteristica(BCCA_TDP_DATA_RAW_CONTROL, c("numlin_n"))

#Verificando balanceo
barplot(table(BCCA_TDP_DATA_RAW_PREPAGO$target_c))
barplot(table(BCCA_TDP_DATA_RAW_POSTPAGO$target_c))
barplot(table(BCCA_TDP_DATA_RAW_CONTROL$target_c))




#Proceso de purificación
#Eliminamos las características mapeadas innecesarias:

#De lo analizado en el documento que muestra los valores faltantes, se crea un nuevo dataset sin valores faltantes.





BCCA_TDP_DATA_PURE_PREPAGO<- subset(BCCA_TDP_DATA_RAW_PREPAGO, !is.na(BCCA_TDP_DATA_RAW_PREPAGO$edad_n) &
                              !is.na(BCCA_TDP_DATA_RAW_PREPAGO$antig_n) &
                              !is.na(BCCA_TDP_DATA_RAW_PREPAGO$eqaseg_c) &
                              !is.na(BCCA_TDP_DATA_RAW_PREPAGO$eqanti_n) & !is.na(BCCA_TDP_DATA_RAW_PREPAGO$costpl_n))

BCCA_TDP_DATA_PURE_POSTPAGO<- subset(BCCA_TDP_DATA_RAW_POSTPAGO, !is.na(BCCA_TDP_DATA_RAW_POSTPAGO$edad_n) &
                              !is.na(BCCA_TDP_DATA_RAW_POSTPAGO$antig_n) &
                              !is.na(BCCA_TDP_DATA_RAW_POSTPAGO$eqaseg_c) &
                              !is.na(BCCA_TDP_DATA_RAW_POSTPAGO$eqanti_n) & !is.na(BCCA_TDP_DATA_RAW_POSTPAGO$costpl_n))

BCCA_TDP_DATA_PURE_CONTROL<- subset(BCCA_TDP_DATA_RAW_CONTROL, !is.na(BCCA_TDP_DATA_RAW_CONTROL$edad_n) &
                              !is.na(BCCA_TDP_DATA_RAW_CONTROL$antig_n)&
                              !is.na(BCCA_TDP_DATA_RAW_CONTROL$eqaseg_c) &
                              !is.na(BCCA_TDP_DATA_RAW_CONTROL$eqanti_n) & !is.na(BCCA_TDP_DATA_RAW_CONTROL$costpl_n))

#Verificación

sum(is.na(BCCA_TDP_DATA_PURE_PREPAGO$cant1m_n))
sum(is.na(BCCA_TDP_DATA_PURE_PREPAGO$mont1m_n))
sum(is.na(BCCA_TDP_DATA_PURE_PREPAGO$cant3m_n))
sum(is.na(BCCA_TDP_DATA_PURE_PREPAGO$mont3m_n))
sum(is.na(BCCA_TDP_DATA_PURE_PREPAGO$cant6m_n))
sum(is.na(BCCA_TDP_DATA_PURE_PREPAGO$mont6m_n))
sum(is.na(BCCA_TDP_DATA_PURE_PREPAGO$ctotal_n))
sum(is.na(BCCA_TDP_DATA_PURE_PREPAGO$mtotal_n))
sum(is.na(BCCA_TDP_DATA_PURE_PREPAGO$costpl_n))
sum(is.na(BCCA_TDP_DATA_PURE_PREPAGO$cnttoc_n))
sum(is.na(BCCA_TDP_DATA_PURE_PREPAGO$cntnrc_n))
sum(is.na(BCCA_TDP_DATA_PURE_PREPAGO$cnttmc_n))

sum(is.na(BCCA_TDP_DATA_RAW_POSTPAGO$cant1m_n))
sum(is.na(BCCA_TDP_DATA_RAW_POSTPAGO$mont1m_n))
sum(is.na(BCCA_TDP_DATA_RAW_POSTPAGO$cant3m_n))
sum(is.na(BCCA_TDP_DATA_RAW_POSTPAGO$mont3m_n))
sum(is.na(BCCA_TDP_DATA_RAW_POSTPAGO$cant6m_n))
sum(is.na(BCCA_TDP_DATA_RAW_POSTPAGO$mont6m_n))
sum(is.na(BCCA_TDP_DATA_RAW_POSTPAGO$ctotal_n))
sum(is.na(BCCA_TDP_DATA_RAW_POSTPAGO$mtotal_n))
sum(is.na(BCCA_TDP_DATA_RAW_POSTPAGO$costpl_n))
sum(is.na(BCCA_TDP_DATA_RAW_POSTPAGO$cnttoc_n))
sum(is.na(BCCA_TDP_DATA_RAW_POSTPAGO$cntnrc_n))
sum(is.na(BCCA_TDP_DATA_RAW_POSTPAGO$cnttmc_n))

sum(is.na(BCCA_TDP_DATA_RAW_CONTROL$cant1m_n))
sum(is.na(BCCA_TDP_DATA_RAW_CONTROL$mont1m_n))
sum(is.na(BCCA_TDP_DATA_RAW_CONTROL$cant3m_n))
sum(is.na(BCCA_TDP_DATA_RAW_CONTROL$mont3m_n))
sum(is.na(BCCA_TDP_DATA_RAW_CONTROL$cant6m_n))
sum(is.na(BCCA_TDP_DATA_RAW_CONTROL$mont6m_n))
sum(is.na(BCCA_TDP_DATA_RAW_CONTROL$ctotal_n))
sum(is.na(BCCA_TDP_DATA_RAW_CONTROL$mtotal_n))
sum(is.na(BCCA_TDP_DATA_RAW_CONTROL$costpl_n))
sum(is.na(BCCA_TDP_DATA_RAW_CONTROL$cnttoc_n))
sum(is.na(BCCA_TDP_DATA_RAW_CONTROL$cntnrc_n))
sum(is.na(BCCA_TDP_DATA_RAW_CONTROL$cnttmc_n))

#Hay algún NA en toda la data pura?
sum(is.na(BCCA_TDP_DATA_PURE_PREPAGO))
sum(is.na(BCCA_TDP_DATA_PURE_POSTPAGO))
sum(is.na(BCCA_TDP_DATA_PURE_CONTROL))


#Eliminación de registros duplicados en la data pura:
BCCA_TDP_DATA_PURE_PREPAGO <- eliminaDuplicados(BCCA_TDP_DATA_PURE_PREPAGO)
BCCA_TDP_DATA_PURE_POSTPAGO<- eliminaDuplicados(BCCA_TDP_DATA_PURE_POSTPAGO)
BCCA_TDP_DATA_PURE_CONTROL <- eliminaDuplicados(BCCA_TDP_DATA_PURE_CONTROL)

#Se mueve el target al final
target_c_temp <- BCCA_TDP_DATA_PURE_PREPAGO$target_c
BCCA_TDP_DATA_PURE_PREPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_PREPAGO, c("target_c"))
target_c <- target_c_temp
BCCA_TDP_DATA_PURE_PREPAGO <- cbind(BCCA_TDP_DATA_PURE_PREPAGO, target_c)
rm(target_c, target_c_temp)
names(BCCA_TDP_DATA_PURE_PREPAGO)

target_c_temp <- BCCA_TDP_DATA_PURE_POSTPAGO$target_c
BCCA_TDP_DATA_PURE_POSTPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_POSTPAGO, c("target_c"))
target_c <- target_c_temp
BCCA_TDP_DATA_PURE_POSTPAGO <- cbind(BCCA_TDP_DATA_PURE_POSTPAGO, target_c)
rm(target_c, target_c_temp)
names(BCCA_TDP_DATA_PURE_POSTPAGO)

target_c_temp <- BCCA_TDP_DATA_PURE_CONTROL$target_c
BCCA_TDP_DATA_PURE_CONTROL <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_CONTROL, c("target_c"))
target_c <- target_c_temp
BCCA_TDP_DATA_PURE_CONTROL <- cbind(BCCA_TDP_DATA_PURE_CONTROL, target_c)
rm(target_c, target_c_temp)
names(BCCA_TDP_DATA_PURE_CONTROL)


#Verificando el balanceo de datos:
barplot(table(BCCA_TDP_DATA_PURE_PREPAGO$target_c))
barplot(table(BCCA_TDP_DATA_PURE_POSTPAGO$target_c))
barplot(table(BCCA_TDP_DATA_PURE_CONTROL$target_c))

#Aplicando algoritmo de balanceo de datos: Oversampling
#Instalando y activando paquetes necesarios para el balanceo
paquetes <- c("unbalanced")
usarPaquetes(paquetes)

#Separando en output (target), input (las demás características)
BCCA_TDP_DATA_RAW_pure_size <- ncol(BCCA_TDP_DATA_RAW_pure)
output <- BCCA_TDP_DATA_RAW_pure[,BCCA_TDP_DATA_RAW_pure_size]
input <- BCCA_TDP_DATA_RAW_pure[,-BCCA_TDP_DATA_RAW_pure_size]
data <- ubBalance(X=input, Y=as.factor(output), type="ubOver", k=0)
#oversampled dataset
overData <- data.frame(data$X, target_c=data$Y)
#Ver distribución del target
barplot(table(overData$target_c))



BCCA_TDP_DATA_RAW_pure <- overData
rm(input,output,overData,data,BCCA_TDP_DATA_RAW_pure_size)
names(BCCA_TDP_DATA_RAW_pure)

#Exportación de los 100 primeros y 100 últimos:
exporta100primeros(BCCA_TDP_DATA_RAW_pure, "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/v.Oficial.puro/BCCA_TDP_DATA_RAW_pure_HEAD.csv")
exporta100ultimos(BCCA_TDP_DATA_RAW_pure, "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/v.Oficial.puro/BCCA_TDP_DATA_RAW_pure_TAIL.csv")


#Exportación de resúmenes de los 3 productos:
#Ver en que directorio se exportará los resúmenes:
getwd()
write.table(summary(BCCA_TDP_DATA_PREPAGO), "ResumenEstadisticoPREPAGO.csv", sep=";", row.names=FALSE, col.names=TRUE, quote=FALSE)
write.table(summary(BCCA_TDP_DATA_POSTPAGO), "ResumenEstadisticoPOSTPAGO.csv", sep=";", row.names=FALSE, col.names=TRUE, quote=FALSE)
write.table(summary(BCCA_TDP_DATA_CONTROL), "ResumenEstadisticoCONTROL.csv", sep=";", row.names=FALSE, col.names=TRUE, quote=FALSE)

#Técnicas de filtrado
BCCA_TDP_DATA_PREPAGO_save <- BCCA_TDP_DATA_PREPAGO
BCCA_TDP_DATA_PREPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PREPAGO, "produc_c")
library("FSelector")
importanciaIG_prepago <- information.gain(target_c~.,BCCA_TDP_DATA_PREPAGO)


#Exportación de la data
write.csv(BCCA_TDP_DATA_PURE_PREPAGO,"D:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA DATASETS V1/BCCA_TDP_DATA_PURE_PREPAGO.csv", row.names = FALSE)
write.csv(BCCA_TDP_DATA_PURE_POSTPAGO,"D:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA DATASETS V1/BCCA_TDP_DATA_PURE_POSTPAGO.csv", row.names = FALSE)
write.csv(BCCA_TDP_DATA_PURE_CONTROL,"D:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA DATASETS V1/BCCA_TDP_DATA_PURE_CONTROL.csv", row.names = FALSE)
