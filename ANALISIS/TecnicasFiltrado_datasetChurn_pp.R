library(ByteAnalitycsUtils)
library(readr)
BCCA_TDP_DATA_PURE_PREPAGO <- read_csv("D:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA DATASETS V1/BCCA_TDP_DATA_PURE_PREPAGO.csv", 
                              col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                               "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                               "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                               "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                               "fld146_n"= col_double(),
                                               "cant1m_n"= col_double(), "mont1m_n"= col_double(), "cant3m_n"= col_double(), "mont3m_n"= col_double(), "cant6m_n"= col_double(), "mont6m_n"= col_double(), "ctotal_n"= col_double(), "mtotal_n"= col_double()))


BCCA_TDP_DATA_PURE_POSTPAGO <- read_csv("D:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA DATASETS V1/BCCA_TDP_DATA_PURE_POSTPAGO.csv", 
                                       col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                                        "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                                        "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                                        "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                                        "fld146_n"= col_double(),
                                                        "cant1m_n"= col_double(), "mont1m_n"= col_double(), "cant3m_n"= col_double(), "mont3m_n"= col_double(), "cant6m_n"= col_double(), "mont6m_n"= col_double(), "ctotal_n"= col_double(), "mtotal_n"= col_double()))




BCCA_TDP_DATA_PURE_CONTROL <- read_csv("D:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA DATASETS V1/BCCA_TDP_DATA_PURE_CONTROL.csv", 
                                       col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                                        "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                                        "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                                        "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                                        "fld146_n"= col_double(),
                                                        "cant1m_n"= col_double(), "mont1m_n"= col_double(), "cant3m_n"= col_double(), "mont3m_n"= col_double(), "cant6m_n"= col_double(), "mont6m_n"= col_double(), "ctotal_n"= col_double(), "mtotal_n"= col_double()))



library("FSelector")

BCCA_TDP_DATA_PURE_PREPAGO_TF <- BCCA_TDP_DATA_PURE_PREPAGO
BCCA_TDP_DATA_PURE_PREPAGO_TF <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_PREPAGO_TF, "produc_c")

importanciaIG_prepago <- information.gain(target_c~.,BCCA_TDP_DATA_PURE_PREPAGO_TF)
importanciaGR_prepago <- gain.ratio(target_c~.,BCCA_TDP_DATA_PURE_PREPAGO_TF)
importanciaSU_prepago <- symmetrical.uncertainty(target_c~.,BCCA_TDP_DATA_PURE_PREPAGO_TF)
#Solo con data continua: importanciaCorr <- rank.correlation(fraud~.,DataFraudeCrediticioSim)
importanciadevariables_prepago <- cbind(GananciaDeInformacion = importanciaIG_prepago,RadioGanancia = importanciaGR_prepago, IncentidumbreSimetrica = importanciaSU_prepago)
names(importanciadevariables_prepago) <- c("GananciaDeInformacion", "RadioGanancia", "IncentidumbreSimetrica")
View(importanciadevariables_prepago)
write.csv(importanciadevariables_prepago,"D:/Byte/Machine Learning/Predictivo Port Out - Oficial/Documentos/BCCA_TDP_DATA - Documentación/Técnicas de filtrado/feature_selection_prepaid.csv")


#Postpago
BCCA_TDP_DATA_PURE_POSTPAGO_TF <- BCCA_TDP_DATA_PURE_POSTPAGO
BCCA_TDP_DATA_PURE_POSTPAGO_TF <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_POSTPAGO_TF, "produc_c")

importanciaIG_postpago <- information.gain(target_c~.,BCCA_TDP_DATA_PURE_POSTPAGO_TF)
importanciaGR_postpago <- gain.ratio(target_c~.,BCCA_TDP_DATA_PURE_POSTPAGO_TF)
importanciaSU_postpago<- symmetrical.uncertainty(target_c~.,BCCA_TDP_DATA_PURE_POSTPAGO_TF)
#Solo con data continua: importanciaCorr <- rank.correlation(fraud~.,DataFraudeCrediticioSim)
importanciadevariables_postpago <- cbind(GananciaDeInformacion = importanciaIG_postpago,RadioGanancia = importanciaGR_postpago, IncentidumbreSimetrica = importanciaSU_postpago)
names(importanciadevariables_postpago) <- c("GananciaDeInformacion", "RadioGanancia", "IncentidumbreSimetrica")
View(importanciadevariables_postpago)
write.csv(importanciadevariables_postpago,"D:/Byte/Machine Learning/Predictivo Port Out - Oficial/Documentos/BCCA_TDP_DATA - Documentación/Técnicas de filtrado/feature_selection_postpaid.csv")


#Control
BCCA_TDP_DATA_PURE_CONTROL_TF <- BCCA_TDP_DATA_PURE_CONTROL
BCCA_TDP_DATA_PURE_CONTROL_TF <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_CONTROL_TF, "produc_c")

importanciaIG_control <- information.gain(target_c~.,BCCA_TDP_DATA_PURE_CONTROL_TF)
importanciaGR_control <- gain.ratio(target_c~.,BCCA_TDP_DATA_PURE_CONTROL_TF)
importanciaSU_control <- symmetrical.uncertainty(target_c~.,BCCA_TDP_DATA_PURE_CONTROL_TF)
#Solo con data continua: importanciaCorr <- rank.correlation(fraud~.,DataFraudeCrediticioSim)
importanciadevariables_control <- cbind(GananciaDeInformacion = importanciaIG_control,RadioGanancia = importanciaGR_control, IncentidumbreSimetrica = importanciaSU_control)
names(importanciadevariables_control) <- c("GananciaDeInformacion", "RadioGanancia", "IncentidumbreSimetrica")
View(importanciadevariables_control)
write.csv(importanciadevariables_control,"D:/Byte/Machine Learning/Predictivo Port Out - Oficial/Documentos/BCCA_TDP_DATA - Documentación/Técnicas de filtrado/feature_selection_control.csv")




BCCA_TDP_DATA_PREPAGO_save <- BCCA_TDP_DATA_PREPAGO
BCCA_TDP_DATA_PREPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PREPAGO, "produc_c")
library("FSelector")
importanciaIG_prepago <- information.gain(target_c~.,BCCA_TDP_DATA_PREPAGO)



BCCA_TDP_DATA_PREPAGO_save <- BCCA_TDP_DATA_PREPAGO
BCCA_TDP_DATA_PREPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PREPAGO, "produc_c")
library("FSelector")
importanciaIG_prepago <- information.gain(target_c~.,BCCA_TDP_DATA_PREPAGO)