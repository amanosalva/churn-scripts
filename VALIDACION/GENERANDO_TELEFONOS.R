#Corrigiendo clavos.

#Debo extraer 15784 registros

#Subir el dataset RAW para extraer los teléfonos

library(ByteAnalitycsUtils)
library(readr)
BCCA_TDP_DATA_RAW <- read_csv("C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/RAW DATA - FINAL/BCCA_TDP_DATA_RAW.csv", 
                              col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                               "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                               "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                               "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                               "fld146_n"= col_double(),
                                               "cant1m_n"= col_double(), "mont1m_n"= col_double(), "cant3m_n"= col_double(), "mont3m_n"= col_double(), "cant6m_n"= col_double(), "mont6m_n"= col_double(), "ctotal_n"= col_double(), "mtotal_n"= col_double()))



BCCA_TDP_DATA_RAW$tcntel_n[length(BCCA_TDP_DATA_RAW$tcntel_n) == 10]

primeros <- head(BCCA_TDP_DATA_RAW, n = 15784)

telefonos <- primeros$tcntel_n

#Exporto la columna de los teléfonos
write.csv(telefonos, "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA_DATA_PURE/TELEFONOS/telefonos.csv")





