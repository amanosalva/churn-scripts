
#Transformo el dataset para TRAIN - listo para generar el modelo en wso2
path_origen_postpago <- "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Sprint 2/Datasets/sin transformar (renzo)/POSTPAGO/POSTPAGO_TRAIN/BCCA_TDP_DATA_POSTPAGO.csv"
path_destino <- "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Sprint 2/Datasets/transformado (anthony)/Dataset postpago 80 del puro - transformado - con target"
lista_output <- transformacionDatasetChurnWithTarget(path_origen_postpago, path_destino, "","","B")
lista_output


#Transformo TEST para predicción en wso2


path_origen_postpago_test <- "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Sprint 2/Datasets/sin transformar (renzo)/POSTPAGO/POSTPAGO_TEST/BCCA_TDP_DATA_POSTPAGO.csv"
path_destino_postpago_test <- "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Sprint 2/Datasets/transformado (anthony)/Dataset postpago 20 transformado sin target"
BCCA_TDP_DATA_POSTPAGO_TEST <- read_csv(path_origen_postpago_test, 
                                   col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                                    "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                                    "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                                    "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                                    "fld146_n"= col_double(), "cant1m_n"= col_double(), 
                                                    "mont1m_n"= col_double(), "cant3m_n"= col_double(), 
                                                    "mont3m_n"= col_double(), "cant6m_n"= col_double(), 
                                                    "mont6m_n"= col_double(), "ctotal_n"= col_double(), 
                                                    "mtotal_n"= col_double()))



lista_output <- transformacionDatasetChurn(path_origen_postpago_test, path_destino_postpago_test, "","","B")
lista_output




