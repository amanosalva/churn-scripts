#Poceso de separación de la data
## Observación: La data para postpago ya fue generada, solo se hará este proceso para prepago y control

library(ByteAnalitycsUtils)
library(readr)

path_origen_prepago <- "C:/Byte/Machine Learning/BCCA - Oficial/1. Datasets/1. No transformado/100%/BCCA_TDP_DATA_PREPAGO.csv"
#path_origen_postpago <- "C:/Byte/Machine Learning/BCCA - Oficial/1. Datasets/1. No transformado/100%/BCCA_TDP_DATA_POSTPAGO.csv"
path_origen_control <- "C:/Byte/Machine Learning/BCCA - Oficial/1. Datasets/1. No transformado/100%/BCCA_TDP_DATA_CONTROL.csv"


BCCA_TDP_DATA_PREPAGO <- read_csv(path_origen_prepago, 
                               col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                                "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                                "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                                "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                                "fld146_n"= col_double(), "cant1m_n"= col_double(), 
                                                "mont1m_n"= col_double(), "cant3m_n"= col_double(), 
                                                "mont3m_n"= col_double(), "cant6m_n"= col_double(), 
                                                "mont6m_n"= col_double(), "ctotal_n"= col_double(), 
                                                "mtotal_n"= col_double()))


BCCA_TDP_DATA_CONTROL <- read_csv(path_origen_control, 
                               col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                                "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                                "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                                "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                                "fld146_n"= col_double(), "cant1m_n"= col_double(), 
                                                "mont1m_n"= col_double(), "cant3m_n"= col_double(), 
                                                "mont3m_n"= col_double(), "cant6m_n"= col_double(), 
                                                "mont6m_n"= col_double(), "ctotal_n"= col_double(), 
                                                "mtotal_n"= col_double()))


path_destino_train_prepago <- "C:/Byte/Machine Learning/BCCA - Oficial/1. Datasets/1. No transformado/100%/80% train/BCCA_TDP_DATA_PREPAGO_80_target.csv"
path_destino_test_prepago  <- "C:/Byte/Machine Learning/BCCA - Oficial/1. Datasets/1. No transformado/100%/20% test/BCCA_TDP_DATA_PREPAGO_20_no_target.csv"

path_destino_train_control <- "C:/Byte/Machine Learning/BCCA - Oficial/1. Datasets/1. No transformado/100%/80% train/BCCA_TDP_DATA_CONTROL_80_target.csv"
path_destino_test_control  <- "C:/Byte/Machine Learning/BCCA - Oficial/1. Datasets/1. No transformado/100%/20% test/BCCA_TDP_DATA_CONTROL_20_no_target.csv"




trainAndTest(BCCA_TDP_DATA_PREPAGO, 0.8, 0.2, path_destino_train_prepago, path_destino_test_prepago)
trainAndTest(BCCA_TDP_DATA_CONTROL, 0.8, 0.2, path_destino_train_control, path_destino_test_control)




####Proceso de transformación

path_origen_train_prepago <- path_destino_train_prepago
path_origen_train_control <- path_destino_train_control

path_destino_train_transformado_prepago <- "C:/Byte/Machine Learning/BCCA - Oficial/1. Datasets/2. Transformado/80% train/"
#BCCA_TDP_DATA_PREPAGO_80_target_trans.csv"
path_destino_train_transformado_control <- "C:/Byte/Machine Learning/BCCA - Oficial/1. Datasets/2. Transformado/80% train/"
#BCCA_TDP_DATA_CONTROL_80_target_trans.csv"

lista_output_prepago <- transformacionDatasetChurnWithTarget(path_origen_train_prepago, path_destino_train_transformado_prepago,"","","B")
lista_output_control <- transformacionDatasetChurnWithTarget(path_origen_train_control, path_destino_train_transformado_control,"","","B")

