#Separación del dataset POSTPAGO analizado filtrado SIN TRANSFORMAR.

path_origen_postpago <- "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA_DATA_PURE/FILTRADA LISTA PARA TRANSFORMAR/BCCA_TDP_DATA_POSTPAGO.csv"
path_origen_prepago <- "C:/Byte/Machine Learning/BCCA - Oficial/1. Datasets/2. Filtradas/BCCA_TDP_DATA_PREPAGO.csv"
path_origen_prepago <- "C:/Byte/Machine Learning/BCCA - Oficial/1. Datasets/2. Filtradas/BCCA_TDP_DATA_CONTROL.csv"



BCCA_TDP_DATA_PURE <- read_csv(path_origen_prepago, 
                               col_types = list("cnttoc_n"= col_double(), 
                                                "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                                "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                                "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                                "fld146_n"= col_double(), "cant1m_n"= col_double(), 
                                                "mont1m_n"= col_double(), "cant3m_n"= col_double(), 
                                                "mont3m_n"= col_double(), "cant6m_n"= col_double(), 
                                                "mont6m_n"= col_double(), "ctotal_n"= col_double(), 
                                                "mtotal_n"= col_double()))


BCCA_TDP_DATA_PURE <- read_csv(path_origen_postpago, 
                               col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                                "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                                "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                                "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                                "fld146_n"= col_double(), "cant1m_n"= col_double(), 
                                                "mont1m_n"= col_double(), "cant3m_n"= col_double(), 
                                                "mont3m_n"= col_double(), "cant6m_n"= col_double(), 
                                                "mont6m_n"= col_double(), "ctotal_n"= col_double(), 
                                                "mtotal_n"= col_double()))


BCCA_TDP_DATA_PURE <- read_csv(path_origen_postpago, 
                               col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                                "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                                "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                                "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                                "fld146_n"= col_double(), "cant1m_n"= col_double(), 
                                                "mont1m_n"= col_double(), "cant3m_n"= col_double(), 
                                                "mont3m_n"= col_double(), "cant6m_n"= col_double(), 
                                                "mont6m_n"= col_double(), "ctotal_n"= col_double(), 
                                                "mtotal_n"= col_double()))



#Borrando la variable tccopl_n, ya que son códigos
BCCA_TDP_DATA_PURE <- eliminaCaracteristica(BCCA_TDP_DATA_PURE, c("tccopl_n"))

ind        <- sample(2,nrow(BCCA_TDP_DATA_PURE), replace=TRUE, prob=c(0.8, 0.2))
BCCA_TDP_DATA_PURE_TRAIN  <- BCCA_TDP_DATA_PURE[ind==1, ]
BCCA_TDP_DATA_PURE_TEST   <- BCCA_TDP_DATA_PURE[ind==2, ]



#Borrar el target en el test para que sea procesado en BCCA
BCCA_TDP_DATA_PURE_TEST <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_TEST, c("target_c"))
length(names(BCCA_TDP_DATA_PURE_TEST))



#Exportando ambos datasets:
#Con este se transformará y se generará el modelo en wso2
write.csv(BCCA_TDP_DATA_PURE_TRAIN, "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Sprint 2/Datasets/sin transformar (renzo)/POSTPAGO/POSTPAGO_TRAIN/BCCA_TDP_DATA_POSTPAGO.csv", row.names = FALSE)

#Con este se validará tanto en wso2 (luego de transformar) como en BCCA (sin transformar)
write.csv(BCCA_TDP_DATA_PURE_TEST, "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Sprint 2/Datasets/sin transformar (renzo)/POSTPAGO/POSTPAGO_TEST/BCCA_TDP_DATA_POSTPAGO.csv", row.names = FALSE)
#Aquí se agrega manualmente el campo telefono (por ahora)



