#Prueba de validadores

path_origen_postpago <- "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA_DATA_PURE/FILTRADA LISTA PARA TRANSFORMAR/sintarget/PRUEBA_VALIDATION/BCCA_TDP_DATA_POSTPAGO.csv"

library(readr)
library(rjson)
print(paste("IMPORTANDO DATASET DESDE ", path_origen_postpago), sep="")
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




#Constantes:
SEPARADOR_ERROR <- '|'

#Output
lista <- list(NULL,NULL,NULL,NULL)

huboError <- function(feature1,feature2){
  lista_flag_and_resultado <- list(NULL,NULL)
  if(identical(feature1,feature2)){
    lista_flag_and_resultado <- list(TRUE,feature2)
    return(lista_flag_and_resultado)}
  else{
    lista_flag_and_resultado <- list(FALSE,NULL)
    return(lista_flag_and_resultado)}
}

#Funciones de validación
validaNAOrNoNumerico <- function(vector){
  nuevoVector <- ifelse(is.na(vector) | !is.numeric(vector) , paste('ERROR',vector,sep=SEPARADOR_ERROR), vector)
  lista_resultante <- huboError(vector,nuevoVector)
  
  return(lista_resultante)                      
}

validaNAOrNoNumericoOrNegativo <- function(vector){
  nuevoVector <- ifelse(is.na(vector) | !is.numeric(vector) | vector < 0, paste('ERROR',vector,sep=SEPARADOR_ERROR), vector)
  lista_resultante <- huboError(vector,nuevoVector)
  
  return(lista_resultante) 
}

validaNAOrNoCaracter <- function(vector){
  nuevoVector <- ifelse(is.na(vector) | !is.character(vector) , paste('ERROR',vector,sep=SEPARADOR_ERROR), vector)
  lista_resultante <- huboError(vector,nuevoVector)
  
  return(lista_resultante) 
}

validaNAOrNoCaracterOrNoValor_s_n <- function(vector){
  nuevoVector <- ifelse(is.na(vector) | !is.character(vector) | (vector != 'n' & vector != 's'), paste('ERROR',vector,sep=SEPARADOR_ERROR), vector)
  lista_resultante <- huboError(vector,nuevoVector)
  
  return(lista_resultante) 
  
}

validaNAOrNoCaracterOrNoValor_S_N <- function(vector){
  nuevoVector <- ifelse(is.na(vector) | !is.character(vector) | (vector != 'N' & vector != 'S'), paste('ERROR',vector,sep=SEPARADOR_ERROR), vector)
  lista_resultante <- huboError(vector,nuevoVector)
  
  return(lista_resultante) 
}


flag_sin_errores <- TRUE


#Validando campos (se tiene que validar todos los campos)

######## Validando producto: 
BCCA_TDP_DATA_PURE$produc_c <- ifelse(
  

    #Validando espacios en blanco
    is.na(BCCA_TDP_DATA_PURE$produc_c) |
  
  
    #Validando que el campo solo tenga valor "Postpago"
    BCCA_TDP_DATA_PURE$produc_c != 'postpago'
  

,paste('ERROR',BCCA_TDP_DATA_PURE$produc_c,sep=SEPARADOR_ERROR) , BCCA_TDP_DATA_PURE$produc_c)



lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$edad_n)        ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$edad_n        <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$antig_n)       ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$antig_n       <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$eqanti_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$eqanti_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoCaracterOrNoValor_s_n(BCCA_TDP_DATA_PURE$linper_c  ) ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$linper_c      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rec1to_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rec1to_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rec1fi_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rec1fi_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rec1pe_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rec1pe_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rec1to_1_n)    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rec1to_1_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rec1fi_1_n)    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rec1fi_1_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rec1pr_1_n)    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rec1pr_1_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rec1to_2_n)    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rec1to_2_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rec1fi_2_n)    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rec1fi_2_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rec1pr_2_n)    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rec1pr_2_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$costpl_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$costpl_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoCaracterOrNoValor_s_n(BCCA_TDP_DATA_PURE$conren_c  ) ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$conren_c      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$cnttoc_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$cnttoc_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$cntnrc_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$cntnrc_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$cnttmc_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$cnttmc_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$cntctd_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$cntctd_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$cntctl_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$cntctl_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$cntctl_1_n)    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$cntctl_1_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$cntctl_2_n)    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$cntctl_2_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$cntsol_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$cntsol_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$tccopl_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$tccopl_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep1to_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep1to_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep1fi_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep1fi_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep1to_1_n)    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep1to_1_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep1fi_1_n)    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep1fi_1_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep1to_2_n)    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep1to_2_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep1fi_2_n)    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep1fi_2_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep2to_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep2to_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep2fi_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep2fi_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep3to_2_n)    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep3to_2_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep3fi_2_n)    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep3fi_2_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep4to_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep4to_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep4fi_n  )    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep4fi_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep4to_2_n)    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep4to_2_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep4fi_2_n)    ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep4fi_2_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld041_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld041_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld042_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld042_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld045_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld045_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld046_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld046_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld049_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld049_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld050_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld050_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld053_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld053_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld054_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld054_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld057_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld057_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld058_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld058_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld061_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld061_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld062_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld062_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld065_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld065_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld066_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld066_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld069_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld069_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld070_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld070_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld073_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld073_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld074_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld074_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld115_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld115_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld116_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld116_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld117_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld117_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoCaracterOrNoValor_S_N(BCCA_TDP_DATA_PURE$fld119_c  ) ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld119_c      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoCaracterOrNoValor_S_N(BCCA_TDP_DATA_PURE$fld120_c  ) ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld120_c      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoCaracterOrNoValor_S_N(BCCA_TDP_DATA_PURE$fld121_c  ) ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld121_c      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoCaracterOrNoValor_S_N(BCCA_TDP_DATA_PURE$fld122_c  ) ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld122_c      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld138_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld138_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld139_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld139_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld140_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld140_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld141_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld141_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld142_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld142_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld143_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld143_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld144_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld144_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld145_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld145_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld146_n  )              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld146_n      <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$rec1to_3_n)              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rec1to_3_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$rep1to_3_n)              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep1to_3_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$rep2to_3_n)              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep2to_3_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$rep3to_3_n)              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep3to_3_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}
lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$rep4to_3_n)              ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep4to_3_n    <- lista_resultante[[2]] ; flag_sin_errores <- FALSE ; lista_resultante <- list(NULL,NULL)}


#Extrayendo datasets con registros error:

if(!flag_sin_errores){

dataset_errores <- subset(BCCA_TDP_DATA_PURE, 
              grepl("ERROR", BCCA_TDP_DATA_PURE$produc_c)   | 
              grepl("ERROR", BCCA_TDP_DATA_PURE$edad_n)     |
              grepl("ERROR", BCCA_TDP_DATA_PURE$antig_n   ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$eqanti_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$linper_c  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rec1to_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rec1fi_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rec1pe_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rec1to_1_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rec1fi_1_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rec1pr_1_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rec1to_2_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rec1fi_2_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rec1pr_2_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$costpl_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$conren_c  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$cnttoc_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$cntnrc_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$cnttmc_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$cntctd_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$cntctl_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$cntctl_1_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$cntctl_2_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$cntsol_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$tccopl_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep1to_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep1fi_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep1to_1_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep1fi_1_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep1to_2_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep1fi_2_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep2to_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep2fi_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep3to_2_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep3fi_2_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep4to_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep4fi_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep4to_2_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep4fi_2_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld041_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld042_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld045_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld046_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld049_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld050_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld053_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld054_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld057_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld058_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld061_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld062_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld065_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld066_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld069_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld070_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld073_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld074_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld115_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld116_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld117_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld119_c  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld120_c  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld121_c  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld122_c  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld138_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld139_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld140_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld141_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld142_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld143_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld144_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld145_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$fld146_n  ) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rec1to_3_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep1to_3_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep2to_3_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep3to_3_n) |
              grepl("ERROR", BCCA_TDP_DATA_PURE$rep4to_3_n) )



lista[[4]] <- as.data.frame.list(dataset_errores)
  
write.csv(dataset_errores, "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA_DATA_PURE/FILTRADA LISTA PARA TRANSFORMAR/sintarget/PRUEBA_VALIDATION/destino/erroresxd.csv", row.names = FALSE)

}

#lista[[4]]
#View(dataset_errores)

#print(toJSON(dataset_errores))
