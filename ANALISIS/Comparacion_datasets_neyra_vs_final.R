#Revisión y comparación de datasets:
#BCCA_TDP_CHURN (con todas las correcciones.)
#V4DATOFIN_raw_cor.csv
library(readr)

path_dataset1 <- "D:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/Corrección Coca - Neyra/V4DATOFIN_raw_cor.csv"
path_dataset2 <- "D:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/Corrección final dataset/BCCA_TDP_DATA_RAW.csv"

V4DATOFIN_raw_cor <- read_csv(path_dataset1, 
                              col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                               "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                               "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                               "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                               "fld146_n"= col_double(), "cant1m_n"= col_double(), 
                                               "mont1m_n"= col_double(), "cant3m_n"= col_double(), 
                                               "mont3m_n"= col_double(), "cant6m_n"= col_double(), 
                                               "mont6m_n"= col_double(), "ctotal_n"= col_double(), 
                                               "mtotal_n"= col_double()))

BCCA_TDP_DATA_RAW <- read_csv(path_dataset2, 
                              col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                               "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                               "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                               "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                               "fld146_n"= col_double(), "cant1m_n"= col_double(), 
                                               "mont1m_n"= col_double(), "cant3m_n"= col_double(), 
                                               "mont3m_n"= col_double(), "cant6m_n"= col_double(), 
                                               "mont6m_n"= col_double(), "ctotal_n"= col_double(), 
                                               "mtotal_n"= col_double()))

#Separando datasets por producto:



#Comparación de variables neyra:
VECTOR_PRODUCTOS <- c("prepago","postpago", "control")
V4DATOFIN_PREPAGO<- subset(V4DATOFIN_raw_cor, V4DATOFIN_raw_cor$produc_c == VECTOR_PRODUCTOS[1])
V4DATOFIN_POSTPAGO<- subset(V4DATOFIN_raw_cor, V4DATOFIN_raw_cor$produc_c == VECTOR_PRODUCTOS[2])
V4DATOFIN_CONTROL<- subset(V4DATOFIN_raw_cor, V4DATOFIN_raw_cor$produc_c == VECTOR_PRODUCTOS[3])

BCCA_TDP_DATA_PREPAGO<- subset(BCCA_TDP_DATA_RAW, BCCA_TDP_DATA_RAW$produc_c == VECTOR_PRODUCTOS[1])
BCCA_TDP_DATA_POSTPAGO<- subset(BCCA_TDP_DATA_RAW, BCCA_TDP_DATA_RAW$produc_c == VECTOR_PRODUCTOS[2])
BCCA_TDP_DATA_CONTROL<- subset(BCCA_TDP_DATA_RAW, BCCA_TDP_DATA_RAW$produc_c == VECTOR_PRODUCTOS[3])


#eqaseg_c
comparaVariablesCategoricas("eqaseg_c",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesCategoricas("eqaseg_c",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1to_3_n
comparaVariablesNumericas("rec1to_3_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1to_3_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1to_n
comparaVariablesNumericas("rec1to_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1to_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1fi_n
comparaVariablesNumericas("rec1fi_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1fi_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1pr_n
comparaVariablesNumericas("rec1pr_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1pr_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1np_n
comparaVariablesNumericas("rec1np_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1np_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1an_n
comparaVariablesNumericas("rec1an_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1an_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1pe_n
comparaVariablesNumericas("rec1pe_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1pe_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1to_1_n
comparaVariablesNumericas("rec1to_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1to_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1fi_1_n
comparaVariablesNumericas("rec1fi_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1fi_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1pr_1_n
comparaVariablesNumericas("rec1pr_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1pr_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1np_1_n
comparaVariablesNumericas("rec1np_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1np_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1an_1_n
comparaVariablesNumericas("rec1an_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1an_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1pe_1_n
comparaVariablesNumericas("rec1pe_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1pe_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1to_2_n
comparaVariablesNumericas("rec1to_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1to_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1fi_2_n
comparaVariablesNumericas("rec1fi_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1fi_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1pr_2_n
comparaVariablesNumericas("rec1pr_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1pr_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1np_2_n
comparaVariablesNumericas("rec1np_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1np_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1an_2_n
comparaVariablesNumericas("rec1an_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1an_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec1pe_2_n
comparaVariablesNumericas("rec1pe_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec1pe_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2to_3_n
comparaVariablesNumericas("rec2to_3_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2to_3_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2to_n
comparaVariablesNumericas("rec2to_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2to_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2fi_n
comparaVariablesNumericas("rec2fi_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2fi_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2pr_n
comparaVariablesNumericas("rec2pr_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2pr_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

 #rec2np_n
comparaVariablesNumericas("rec2np_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2np_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2an_n
comparaVariablesNumericas("rec2an_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2an_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2pe_n
comparaVariablesNumericas("rec2pe_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2pe_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2to_1_n
comparaVariablesNumericas("rec2to_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2to_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2fi_1_n
comparaVariablesNumericas("rec2fi_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2fi_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2pr_1_n
comparaVariablesNumericas("rec2pr_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2pr_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2np_1_n
comparaVariablesNumericas("rec2np_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2np_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2an_1_n
comparaVariablesNumericas("rec2an_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2an_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2pe_1_n
comparaVariablesNumericas("rec2pe_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2pe_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2to_2_n
comparaVariablesNumericas("rec2to_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2to_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2fi_2_n
comparaVariablesNumericas("rec2fi_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2fi_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2pr_2_n
comparaVariablesNumericas("rec2pr_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2pr_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2np_2_n
comparaVariablesNumericas("rec2np_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2np_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2an_2_n
comparaVariablesNumericas("rec2an_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2an_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec2pe_2_n
comparaVariablesNumericas("rec2pe_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec2pe_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3to_3_n
comparaVariablesNumericas("rec3to_3_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3to_3_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3to_n
comparaVariablesNumericas("rec3to_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3to_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3fi_n
comparaVariablesNumericas("rec3fi_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3fi_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3pr_n
comparaVariablesNumericas("rec3pr_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3pr_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3np_n
comparaVariablesNumericas("rec3np_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3np_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3an_n
comparaVariablesNumericas("rec3an_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3an_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3pe_n
comparaVariablesNumericas("rec3pe_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3pe_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3to_1_n
comparaVariablesNumericas("rec3to_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3to_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3fi_1_n
comparaVariablesNumericas("rec3fi_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3fi_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3pr_1_n
comparaVariablesNumericas("rec3pr_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3pr_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3np_1_n
comparaVariablesNumericas("rec3np_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3np_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3an_1_n
comparaVariablesNumericas("rec3an_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3an_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3pe_1_n
comparaVariablesNumericas("rec3pe_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3pe_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3to_2_n
comparaVariablesNumericas("rec3to_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3to_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3fi_2_n
comparaVariablesNumericas("rec3fi_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3fi_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3pr_2_n
comparaVariablesNumericas("rec3pr_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3pr_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3np_2_n
comparaVariablesNumericas("rec3np_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3np_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3an_2_n
comparaVariablesNumericas("rec3an_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3an_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec3pe_2_n
comparaVariablesNumericas("rec3pe_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec3pe_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4to_3_n
comparaVariablesNumericas("rec4to_3_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4to_3_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4to_n
comparaVariablesNumericas("rec4to_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4to_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4fi_n
comparaVariablesNumericas("rec4fi_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4fi_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4pr_n
comparaVariablesNumericas("rec4pr_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4pr_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4np_n
comparaVariablesNumericas("rec4np_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4np_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4an_n
comparaVariablesNumericas("rec4an_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4an_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4pe_n
comparaVariablesNumericas("rec4pe_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4pe_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4to_1_n
comparaVariablesNumericas("rec4to_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4to_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4fi_1_n
comparaVariablesNumericas("rec4fi_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4fi_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4pr_1_n
comparaVariablesNumericas("rec4pr_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4pr_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4np_1_n
comparaVariablesNumericas("rec4np_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4np_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4an_1_n
comparaVariablesNumericas("rec4an_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4an_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4pe_1_n
comparaVariablesNumericas("rec4pe_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4pe_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4to_2_n
comparaVariablesNumericas("rec4to_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4to_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4fi_2_n
comparaVariablesNumericas("rec4fi_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4fi_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4pr_2_n
comparaVariablesNumericas("rec4pr_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4pr_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4np_2_n
comparaVariablesNumericas("rec4np_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4np_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4an_2_n
comparaVariablesNumericas("rec4an_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4an_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rec4pe_2_n
comparaVariablesNumericas("rec4pe_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rec4pe_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep1to_3_n
comparaVariablesNumericas("rep1to_3_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep1to_3_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep1to_n
comparaVariablesNumericas("rep1to_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep1to_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep1fi_n
comparaVariablesNumericas("rep1fi_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep1fi_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep1an_n
comparaVariablesNumericas("rep1an_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep1an_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep1pe_n
comparaVariablesNumericas("rep1pe_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep1pe_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep1to_1_n
comparaVariablesNumericas("rep1to_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep1to_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep1fi_1_n
comparaVariablesNumericas("rep1fi_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep1fi_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep1an_1_n
comparaVariablesNumericas("rep1an_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep1an_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep1pe_1_n
comparaVariablesNumericas("rep1pe_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep1pe_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep1to_2_n
comparaVariablesNumericas("rep1to_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep1to_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep1fi_2_n
comparaVariablesNumericas("rep1fi_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep1fi_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep1an_2_n
comparaVariablesNumericas("rep1an_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep1an_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep1pe_2_n
comparaVariablesNumericas("rep1pe_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep1pe_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep2to_3_n
comparaVariablesNumericas("rep2to_3_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep2to_3_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep2to_n
comparaVariablesNumericas("rep2to_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep2to_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep2fi_n
comparaVariablesNumericas("rep2fi_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep2fi_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep2an_n
comparaVariablesNumericas("rep2an_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep2an_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep2pe_n
comparaVariablesNumericas("rep2pe_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep2pe_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep2to_1_n
comparaVariablesNumericas("rep2to_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep2to_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep2fi_1_n
comparaVariablesNumericas("rep2fi_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep2fi_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep2an_1_n
comparaVariablesNumericas("rep2an_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep2an_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep2pe_1_n
comparaVariablesNumericas("rep2pe_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep2pe_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep2to_2_n
comparaVariablesNumericas("rep2to_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep2to_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep2fi_2_n
comparaVariablesNumericas("rep2fi_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep2fi_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep2an_2_n
comparaVariablesNumericas("rep2an_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep2an_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep2pe_2_n
comparaVariablesNumericas("rep2pe_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep2pe_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep3to_3_n 
comparaVariablesNumericas("rep3to_3_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep3to_3_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep3to_n
comparaVariablesNumericas("rep3to_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep3to_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep3fi_n
comparaVariablesNumericas("rep3fi_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep3fi_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep3an_n
comparaVariablesNumericas("rep3an_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep3an_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep3pe_n
comparaVariablesNumericas("rep3pe_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep3pe_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep3to_1_n
comparaVariablesNumericas("rep3to_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep3to_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep3fi_1_n
comparaVariablesNumericas("rep3fi_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep3fi_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep3an_1_n
comparaVariablesNumericas("rep3an_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep3an_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep3pe_1_n
comparaVariablesNumericas("rep3pe_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep3pe_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep3to_2_n
comparaVariablesNumericas("rep3to_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep3to_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep3fi_2_n
comparaVariablesNumericas("rep3fi_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep3fi_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep3an_2_n
comparaVariablesNumericas("rep3an_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep3an_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep3pe_2_n
comparaVariablesNumericas("rep3pe_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep3pe_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep4to_3_n
comparaVariablesNumericas("rep4to_3_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep4to_3_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep4to_n
comparaVariablesNumericas("rep4to_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep4to_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep4fi_n
comparaVariablesNumericas("rep4fi_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep4fi_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep4an_n
comparaVariablesNumericas("rep4an_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep4an_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep4pe_n
comparaVariablesNumericas("rep4pe_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep4pe_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep4to_1_n
comparaVariablesNumericas("rep4to_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep4to_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep4fi_1_n
comparaVariablesNumericas("rep4fi_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep4fi_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep4an_1_n
comparaVariablesNumericas("rep4an_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep4an_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep4pe_1_n
comparaVariablesNumericas("rep4pe_1_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep4pe_1_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep4to_2_n
comparaVariablesNumericas("rep4to_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep4to_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep4fi_2_n
comparaVariablesNumericas("rep4fi_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep4fi_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep4an_2_n
comparaVariablesNumericas("rep4an_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep4an_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#rep4pe_2_n
comparaVariablesNumericas("rep4pe_2_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("rep4pe_2_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#cnttoc_n
comparaVariablesNumericas("cnttoc_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("cnttoc_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#cntnrc_n
comparaVariablesNumericas("cntnrc_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("cntnrc_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)

#cnttmc_n
comparaVariablesNumericas("cnttmc_n",V4DATOFIN_PREPAGO,V4DATOFIN_POSTPAGO,V4DATOFIN_CONTROL)
comparaVariablesNumericas("cnttmc_n",BCCA_TDP_DATA_PREPAGO,BCCA_TDP_DATA_POSTPAGO,BCCA_TDP_DATA_CONTROL)


