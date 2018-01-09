#autor: amanosalva
#Data generada con las correcciones de José Coca y José Neyra
#Script que genera dataset puro, a partir del dataset crudo BCCA_TDP_DATA.
#Activar librería ByteAnalyticsUtils

#Importación de data cruda.
library(ByteAnalitycsUtils)
library(readr)
BCCA_TDP_DATA_RAW <- read_csv("C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/RAW DATA - FINAL/BCCA_TDP_DATA_RAW.csv", 
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
                             "provi_c","disti_c", "tcsecn_c", "serietd_c","eqaseg_c")
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

####################################################################################################################################

BCCA_TDP_DATA_RAW_PREPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_RAW_PREPAGO, c("numlin_n"))
BCCA_TDP_DATA_RAW_POSTPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_RAW_POSTPAGO, c("numlin_n"))
BCCA_TDP_DATA_RAW_CONTROL <- eliminaCaracteristica(BCCA_TDP_DATA_RAW_CONTROL, c("numlin_n"))



#No corresponde al negocio prepago
BCCA_TDP_DATA_RAW_PREPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_RAW_PREPAGO, c("linper_c","costpl_n","conren_c",
                                                                                  "cnttoc_n","cntnrc_n","cnttmc_n",
                                                                                  "cntctd_n","cntctl_n","cntctl_1_n",
                                                                                  "cntctl_2_n","fld115_n","fld116_n",
                                                                                  "fld117_n","fld119_c","fld120_c",
                                                                                  "fld121_c","fld122_c","fld138_n",
                                                                                  "fld139_n","fld140_n","fld141_n",
                                                                                  "fld142_n","fld143_n","fld144_n",
                                                                                  "fld145_n","fld146_n"))



#No corresponde al negocio postpago
BCCA_TDP_DATA_RAW_POSTPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_RAW_POSTPAGO ,c("pli_sn_c","cant1m_n","mont1m_n",
                                                                                   "cant3m_n","mont3m_n","cant6m_n",
                                                                                   "mont6m_n","ctotal_n","mtotal_n")) 


#No corresponde al negocio control
BCCA_TDP_DATA_RAW_CONTROL <- eliminaCaracteristica(BCCA_TDP_DATA_RAW_CONTROL,c("pli_sn_c","cant1m_n","mont1m_n",
                                                                                 "cant3m_n","mont3m_n","cant6m_n",
                                                                                 "mont6m_n","ctotal_n","mtotal_n")) 






#Problema en tráfico por depuración
BCCA_TDP_DATA_RAW_PREPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_RAW_PREPAGO, c("fld055_n","fld056_n","fld059_n",
                                                                                  "fld060_n","fld063_n","fld064_n",
                                                                                  "fld067_n","fld068_n","fld071_n",
                                                                                  "fld072_n"))

BCCA_TDP_DATA_RAW_POSTPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_RAW_POSTPAGO, c("fld055_n","fld056_n","fld059_n",
                                                                                    "fld060_n","fld063_n","fld064_n",
                                                                                    "fld067_n","fld068_n","fld071_n",
                                                                                    "fld072_n"))

BCCA_TDP_DATA_RAW_CONTROL <- eliminaCaracteristica(BCCA_TDP_DATA_RAW_CONTROL, c("fld055_n","fld056_n","fld059_n",
                                                                                  "fld060_n","fld063_n","fld064_n",
                                                                                  "fld067_n","fld068_n","fld071_n",
                                                                                  "fld072_n"))


#Verificando balanceo
barplot(table(BCCA_TDP_DATA_RAW_PREPAGO$target_c))
barplot(table(BCCA_TDP_DATA_RAW_POSTPAGO$target_c))
barplot(table(BCCA_TDP_DATA_RAW_CONTROL$target_c))




#Proceso de purificación
#Eliminamos las características mapeadas innecesarias:

#De lo analizado en el documento que muestra los valores faltantes, se crea un nuevo dataset sin valores faltantes.





BCCA_TDP_DATA_PURE_PREPAGO<- subset(BCCA_TDP_DATA_RAW_PREPAGO, !is.na(BCCA_TDP_DATA_RAW_PREPAGO$edad_n) &
                              !is.na(BCCA_TDP_DATA_RAW_PREPAGO$antig_n) &
                              !is.na(BCCA_TDP_DATA_RAW_PREPAGO$eqanti_n))

BCCA_TDP_DATA_PURE_POSTPAGO<- subset(BCCA_TDP_DATA_RAW_POSTPAGO, !is.na(BCCA_TDP_DATA_RAW_POSTPAGO$edad_n) &
                              !is.na(BCCA_TDP_DATA_RAW_POSTPAGO$antig_n) &
                              !is.na(BCCA_TDP_DATA_RAW_POSTPAGO$eqanti_n) & !is.na(BCCA_TDP_DATA_RAW_POSTPAGO$costpl_n))

BCCA_TDP_DATA_PURE_CONTROL<- subset(BCCA_TDP_DATA_RAW_CONTROL, !is.na(BCCA_TDP_DATA_RAW_CONTROL$edad_n) &
                              !is.na(BCCA_TDP_DATA_RAW_CONTROL$antig_n)&
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



sum(is.na(BCCA_TDP_DATA_RAW_POSTPAGO$costpl_n))
sum(is.na(BCCA_TDP_DATA_RAW_POSTPAGO$cnttoc_n))
sum(is.na(BCCA_TDP_DATA_RAW_POSTPAGO$cntnrc_n))
sum(is.na(BCCA_TDP_DATA_RAW_POSTPAGO$cnttmc_n))

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

#Checkpoint
BCCA_TDP_DATA_PURE_PREPAGO_SAVE <- BCCA_TDP_DATA_PURE_PREPAGO
BCCA_TDP_DATA_PURE_POSTPAGO_SAVE <- BCCA_TDP_DATA_PURE_POSTPAGO
BCCA_TDP_DATA_PURE_CONTROL_SAVE <- BCCA_TDP_DATA_PURE_CONTROL


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

library(tibble)
library(data.table)

#Moviendo el producto al inicio

produc_c_temp <- BCCA_TDP_DATA_PURE_PREPAGO$produc_c
BCCA_TDP_DATA_PURE_PREPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_PREPAGO, c("produc_c"))
produc_c <- produc_c_temp
BCCA_TDP_DATA_PURE_PREPAGO <- add_column(BCCA_TDP_DATA_PURE_PREPAGO, produc_c, .before = "edad_n")
names(BCCA_TDP_DATA_PURE_PREPAGO)
rm(produc_c)


produc_c_temp <- BCCA_TDP_DATA_PURE_POSTPAGO$produc_c
BCCA_TDP_DATA_PURE_POSTPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_POSTPAGO, c("produc_c"))
produc_c <- produc_c_temp
BCCA_TDP_DATA_PURE_POSTPAGO <- add_column(BCCA_TDP_DATA_PURE_POSTPAGO, produc_c, .before = "edad_n")
names(BCCA_TDP_DATA_PURE_POSTPAGO)
rm(produc_c)


produc_c_temp <- BCCA_TDP_DATA_PURE_CONTROL$produc_c
BCCA_TDP_DATA_PURE_CONTROL <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_CONTROL, c("produc_c"))
produc_c <- produc_c_temp
BCCA_TDP_DATA_PURE_CONTROL <- add_column(BCCA_TDP_DATA_PURE_CONTROL, produc_c, .before = "edad_n")
names(BCCA_TDP_DATA_PURE_CONTROL)
rm(produc_c)


#Exportación de los tres datasets:
write.csv(BCCA_TDP_DATA_PURE_PREPAGO,"C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA_DATA_PURE/BCCA_TDP_DATA_PURE_PREPAGO.csv", row.names = FALSE)
write.csv(BCCA_TDP_DATA_PURE_POSTPAGO,"C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA_DATA_PURE/BCCA_TDP_DATA_PURE_POSTPAGO.csv", row.names = FALSE)
write.csv(BCCA_TDP_DATA_PURE_CONTROL,"C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA_DATA_PURE/BCCA_TDP_DATA_PURE_CONTROL.csv", row.names = FALSE)


######################
#APLICACIÓN DE LAS TÉCNICAS DE FILTRADO.(TecnicasFiltrado_datasetChurn_pp.R)

#Eliminación de las características según peso para cada producto:
caracteristicasInutilesTFPREPAGO <- c("rec1to_n","rec1fi_n","rec1pr_n","rec1np_n","rec1an_n","rec1pe_n","rec1to_1_n","rec1fi_1_n","rec1pr_1_n",
                                      "rec1np_1_n","rec1an_1_n","rec1pe_1_n","rec1to_2_n","rec1fi_2_n","rec1pr_2_n","rec1np_2_n","rec1an_2_n",
                                      "rec1pe_2_n","rec2to_n","rec2fi_n","rec2pr_n","rec2np_n","rec2an_n","rec2pe_n","rec2to_1_n","rec2fi_1_n",
                                      "rec2pr_1_n","rec2np_1_n","rec2an_1_n","rec2pe_1_n","rec2to_2_n","rec2fi_2_n","rec2pr_2_n","rec2np_2_n",
                                      "rec2an_2_n","rec2pe_2_n","rec3to_n","rec3fi_n","rec3pr_n","rec3np_n","rec3an_n","rec3pe_n","rec3to_1_n",
                                      "rec3fi_1_n","rec3pr_1_n","rec3np_1_n","rec3an_1_n","rec3pe_1_n","rec3to_2_n","rec3fi_2_n","rec3pr_2_n",
                                      "rec3np_2_n","rec3an_2_n","rec3pe_2_n","rec4to_n","rec4fi_n","rec4pr_n","rec4np_n","rec4an_n","rec4pe_n",
                                      "rec4to_1_n","rec4fi_1_n","rec4pr_1_n","rec4np_1_n","rec4an_1_n","rec4pe_1_n","rec4np_2_n","rec4an_2_n",
                                      "rec4pe_2_n","rep1to_n","rep1fi_n","rep1an_n","rep1pe_n","rep1an_1_n","rep1pe_1_n","rep1an_2_n","rep1pe_2_n",
                                      "rep2to_n","rep2fi_n","rep2an_n","rep2pe_n","rep2an_1_n","rep2pe_1_n","rep2an_2_n","rep2pe_2_n","rep3to_n",
                                      "rep3fi_n","rep3an_n","rep3pe_n","rep3to_1_n","rep3fi_1_n","rep3an_1_n","rep3pe_1_n","rep3an_2_n","rep3pe_2_n",
                                      "rep4to_n","rep4fi_n","rep4an_n","rep4pe_n","rep4to_1_n","rep4fi_1_n","rep4an_1_n","rep4pe_1_n","rep4to_2_n",
                                      "rep4fi_2_n","rep4an_2_n","rep4pe_2_n","fld043_n","fld044_n","fld047_n","fld048_n","fld051_n",
                                      "fld052_n","fld075_n","fld076_n","rec2to_3_n","rep4to_3_n","tccopl_n")

BCCA_TDP_DATA_PURE_PREPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_PREPAGO, caracteristicasInutilesTFPREPAGO)
rm(caracteristicasInutilesTFPREPAGO)


caracteristicasInutilesTFPOSTPAGO <- c("rec1pr_n","rec1np_n","rec1an_n","rec1np_1_n","rec1an_1_n","rec1pe_1_n","rec1np_2_n","rec1an_2_n","rec1pe_2_n",
                                       "rec2to_n","rec2fi_n","rec2pr_n","rec2np_n","rec2an_n","rec2pe_n","rec2to_1_n","rec2fi_1_n","rec2pr_1_n",
                                       "rec2np_1_n","rec2an_1_n","rec2pe_1_n","rec2to_2_n","rec2fi_2_n","rec2pr_2_n","rec2np_2_n","rec2an_2_n","rec2pe_2_n",
                                       "rec3to_n","rec3fi_n","rec3pr_n","rec3np_n","rec3an_n","rec3pe_n","rec3to_1_n","rec3fi_1_n","rec3pr_1_n",
                                       "rec3np_1_n","rec3an_1_n","rec3pe_1_n","rec3to_2_n","rec3fi_2_n","rec3pr_2_n","rec3np_2_n","rec3an_2_n",
                                       "rec3pe_2_n","rec4to_n","rec4fi_n","rec4pr_n","rec4np_n","rec4an_n","rec4pe_n","rec4to_1_n","rec4fi_1_n",
                                       "rec4pr_1_n","rec4np_1_n","rec4an_1_n","rec4pe_1_n","rec4to_2_n","rec4fi_2_n","rec4pr_2_n","rec4np_2_n",
                                       "rec4an_2_n","rec4pe_2_n","rep1an_n","rep1pe_n","rep1an_1_n","rep1pe_1_n","rep1an_2_n","rep1pe_2_n","rep2an_n",
                                       "rep2pe_n","rep2to_1_n","rep2fi_1_n","rep2an_1_n","rep2pe_1_n","rep2to_2_n","rep2fi_2_n","rep2an_2_n",
                                       "rep2pe_2_n","rep3to_n","rep3fi_n","rep3an_n","rep3pe_n","rep3to_1_n","rep3fi_1_n","rep3an_1_n",
                                       "rep3pe_1_n","rep3an_2_n","rep3pe_2_n","rep4an_n","rep4pe_n","rep4to_1_n","rep4fi_1_n","rep4an_1_n",
                                       "rep4pe_1_n","rep4an_2_n","rep4pe_2_n","fld043_n","fld044_n","fld047_n","fld048_n","fld051_n","fld052_n",
                                       "fld075_n","fld076_n","rec2to_3_n","rec3to_3_n","rec4to_3_n")

BCCA_TDP_DATA_PURE_POSTPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_POSTPAGO, caracteristicasInutilesTFPOSTPAGO)
rm(caracteristicasInutilesTFPOSTPAGO)

caracteristicasInutilesTFCONTROL <- c("rec1np_n","rec1an_n","rec1to_1_n","rec1fi_1_n","rec1pr_1_n","rec1np_1_n","rec1an_1_n","rec1pe_1_n","rec1np_2_n",
                                      "rec1an_2_n","rec1pe_2_n","rec2to_n","rec2fi_n","rec2pr_n","rec2np_n","rec2an_n","rec2pe_n","rec2to_1_n","rec2fi_1_n",
                                      "rec2pr_1_n","rec2np_1_n","rec2an_1_n","rec2pe_1_n","rec2to_2_n","rec2fi_2_n","rec2pr_2_n","rec2np_2_n","rec2an_2_n",
                                      "rec2pe_2_n","rec3fi_n","rec3pr_n","rec3np_n","rec3an_n","rec3pe_n","rec3to_1_n","rec3fi_1_n","rec3pr_1_n","rec3np_1_n",
                                      "rec3an_1_n","rec3pe_1_n","rec3to_2_n","rec3fi_2_n","rec3pr_2_n","rec3np_2_n","rec3an_2_n","rec3pe_2_n","rec4to_n",
                                      "rec4fi_n","rec4pr_n","rec4np_n","rec4an_n","rec4pe_n","rec4to_1_n","rec4fi_1_n","rec4pr_1_n","rec4np_1_n","rec4an_1_n",
                                      "rec4pe_1_n","rec4to_2_n","rec4fi_2_n","rec4pr_2_n","rec4np_2_n","rec4an_2_n","rec4pe_2_n","rep1an_n","rep1pe_n",
                                      "rep1an_1_n","rep1pe_1_n","rep1an_2_n","rep1pe_2_n","rep2an_n","rep2pe_n","rep2an_1_n","rep2pe_1_n","rep2an_2_n",
                                      "rep2pe_2_n","rep3to_n","rep3fi_n","rep3an_n","rep3pe_n","rep3to_1_n","rep3fi_1_n","rep3an_1_n","rep3pe_1_n",
                                      "rep3an_2_n","rep3pe_2_n","rep4an_n","rep4pe_n","rep4to_1_n","rep4fi_1_n","rep4an_1_n","rep4pe_1_n","rep4an_2_n",
                                      "rep4pe_2_n","fld043_n","fld044_n","fld047_n","fld048_n","fld051_n","fld052_n","fld075_n","fld076_n",
                                      "rec3to_3_n","rec4to_3_n")

BCCA_TDP_DATA_PURE_CONTROL <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_CONTROL, caracteristicasInutilesTFCONTROL)
rm(caracteristicasInutilesTFCONTROL)

######################
BCCA_TDP_DATA_PREPAGO <- BCCA_TDP_DATA_PURE_PREPAGO
BCCA_TDP_DATA_POSTPAGO <- BCCA_TDP_DATA_PURE_POSTPAGO
BCCA_TDP_DATA_CONTROL <- BCCA_TDP_DATA_PURE_CONTROL


write.csv(BCCA_TDP_DATA_PREPAGO,"C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA_DATA_PURE/FILTRADA LISTA PARA TRANSFORMAR/BCCA_TDP_DATA_PREPAGO.csv", row.names = FALSE)
write.csv(BCCA_TDP_DATA_POSTPAGO,"C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA_DATA_PURE/FILTRADA LISTA PARA TRANSFORMAR/BCCA_TDP_DATA_POSTPAGO.csv", row.names = FALSE)
write.csv(BCCA_TDP_DATA_CONTROL,"C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA_DATA_PURE/FILTRADA LISTA PARA TRANSFORMAR/BCCA_TDP_DATA_CONTROL.csv", row.names = FALSE)


#Prueba con el árbol de decisión - PREPAGO
library("rpart")
library("rpart.plot")
library("C50")


#Creamos set de entrenamiento y de test
#80% entrenamiento - 20% validación
ind        <- sample(2,nrow(BCCA_TDP_DATA_PREPAGO), replace=TRUE, prob=c(0.8, 0.2))
trainData  <- BCCA_TDP_DATA_PREPAGO[ind==1, ]
testData   <- BCCA_TDP_DATA_PREPAGO[ind==2, ]

ArbolRpart <- rpart(target_c ~ ., method = "class", data = trainData)

print(ArbolRpart)
rpart.plot(ArbolRpart)  # extra=4:probabilidad de observaciones por clase

testPredRpart <- predict(ArbolRpart, newdata = testData, type = "class")

#Visualizamos una matriz de confusión
table(testPredRpart, testData$target_c)

#Estadística del modelo.
#Calculamos el % de aciertos.
sum(testPredRpart == testData$target_c)/length(testData$target_c)*100




############################################################################################################
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

