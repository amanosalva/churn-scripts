#Validación de resultados:
path_bcca <- "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Sprint 2/Resultados/BCCA_TDP_DATA_POSTPAGO_RESULT.csv"
path_wso2 <- "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Sprint 2/Resultados/Predictions_WSO2.csv"

library(readr)


BCCA_TDP_DATA_POSTPAGO_RESULT <- read_csv(path_bcca, 
                                          col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                                           "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                                           "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                                           "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                                           "fld146_n"= col_double(), "cant1m_n"= col_double(), 
                                                           "mont1m_n"= col_double(), "cant3m_n"= col_double(), 
                                                           "mont3m_n"= col_double(), "cant6m_n"= col_double(), 
                                                           "mont6m_n"= col_double(), "ctotal_n"= col_double(), 
                                                           "mtotal_n"= col_double()))

Predictions_WSO2 <- read_csv(path_wso2, 
                                          col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                                           "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                                           "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                                           "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                                           "fld146_n"= col_double(), "cant1m_n"= col_double(), 
                                                           "mont1m_n"= col_double(), "cant3m_n"= col_double(), 
                                                           "mont3m_n"= col_double(), "cant6m_n"= col_double(), 
                                                           "mont6m_n"= col_double(), "ctotal_n"= col_double(), 
                                                           "mtotal_n"= col_double()))


(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$fld146_n     ==    Predictions_WSO2$fld146_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$cntnrc_n     ==    Predictions_WSO2$cntnrc_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$fld141_n     ==    Predictions_WSO2$fld141_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$tccopl_n     ==    Predictions_WSO2$tccopl_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$antig_n      ==    Predictions_WSO2$antig_n    )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$eqanti_n     ==    Predictions_WSO2$eqanti_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$costpl_n     ==    Predictions_WSO2$costpl_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$fld138_n     ==    Predictions_WSO2$fld138_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$cnttoc_n     ==    Predictions_WSO2$cnttoc_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$fld140_n     ==    Predictions_WSO2$fld140_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$linper_c     ==    Predictions_WSO2$linper_c   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$cnttmc_n     ==    Predictions_WSO2$cnttmc_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$conren_c     ==    Predictions_WSO2$conren_c   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$fld142_n     ==    Predictions_WSO2$fld142_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$cntctd_n     ==    Predictions_WSO2$cntctd_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$fld144_n     ==    Predictions_WSO2$fld144_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$fld143_n     ==    Predictions_WSO2$fld143_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rec1pe_n     ==    Predictions_WSO2$rec1pe_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$fld139_n     ==    Predictions_WSO2$fld139_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$edad_n       ==    Predictions_WSO2$edad_n     )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$cntctl_n     ==    Predictions_WSO2$cntctl_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$fld117_n     ==    Predictions_WSO2$fld117_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$cntctl_2_n   ==    Predictions_WSO2$cntctl_2_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep2to_n     ==    Predictions_WSO2$rep2to_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep2fi_n     ==    Predictions_WSO2$rep2fi_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep4to_n     ==    Predictions_WSO2$rep4to_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep4fi_n     ==    Predictions_WSO2$rep4fi_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep1to_n     ==    Predictions_WSO2$rep1to_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep1fi_n     ==    Predictions_WSO2$rep1fi_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$fld121_c     ==    Predictions_WSO2$fld121_c   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$fld145_n     ==    Predictions_WSO2$fld145_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$fld119_c     ==    Predictions_WSO2$fld119_c   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep4to_2_n   ==    Predictions_WSO2$rep4to_2_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep4fi_2_n   ==    Predictions_WSO2$rep4fi_2_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep4to_3_n   ==    Predictions_WSO2$rep4to_3_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$fld120_c     ==    Predictions_WSO2$fld120_c   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$fld122_c     ==    Predictions_WSO2$fld122_c   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rec1to_n     ==    Predictions_WSO2$rec1to_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$cntctl_1_n   ==    Predictions_WSO2$cntctl_1_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rec1fi_1_n   ==    Predictions_WSO2$rec1fi_1_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rec1pr_2_n   ==    Predictions_WSO2$rec1pr_2_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rec1to_2_n   ==    Predictions_WSO2$rec1to_2_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rec1fi_n     ==    Predictions_WSO2$rec1fi_n   )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rec1to_1_n   ==    Predictions_WSO2$rec1to_1_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rec1pr_1_n   ==    Predictions_WSO2$rec1pr_1_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rec1fi_2_n   ==    Predictions_WSO2$rec1fi_2_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rec1to_3_n   ==    Predictions_WSO2$rec1to_3_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep3to_2_n   ==    Predictions_WSO2$rep3to_2_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep3fi_2_n   ==    Predictions_WSO2$rep3fi_2_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep1to_3_n   ==    Predictions_WSO2$rep1to_3_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep3to_3_n   ==    Predictions_WSO2$rep3to_3_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep1to_1_n   ==    Predictions_WSO2$rep1to_1_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep1fi_1_n   ==    Predictions_WSO2$rep1fi_1_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep1to_2_n   ==    Predictions_WSO2$rep1to_2_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep1fi_2_n   ==    Predictions_WSO2$rep1fi_2_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))
(sum(BCCA_TDP_DATA_POSTPAGO_RESULT$rep2to_3_n   ==    Predictions_WSO2$rep2to_3_n )== nrow(BCCA_TDP_DATA_POSTPAGO_RESULT))






