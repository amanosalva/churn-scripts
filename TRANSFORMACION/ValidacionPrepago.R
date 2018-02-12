#Validación PREPAGO

validaCamposPrepago <- function(BCCA_TDP_DATA_PURE, SEPARADOR_ERROR, lista_output){
  NOMBRE_DATASET_ERRORS <- "BCCA_TDP_FILE_ERRORS_"
  EXTENSION_ARCHIVO_DESTINO <- ".csv"
  #SubFunciones de validación
  
  HuboError <- function(feature1,feature2){
    lista_flag_and_resultado <- list(NULL,NULL)
    if(!identical(feature1,feature2)){
      lista_flag_and_resultado <- list(TRUE,feature2)
      return(lista_flag_and_resultado)}
    else{
      lista_flag_and_resultado <- list(FALSE,NULL)
      return(lista_flag_and_resultado)}
  }
  
  
  validaTelefono <- function(vector){
    lista_resultante <- list(NULL,NULL)
    nuevoVector <- ifelse(is.na(vector) | nchar(trimws(vector)) != 9 | sub("^\\s+", "", vector) == "" |  grepl("[^0-9|0-9.|0-9e+0-9]", vector) | vector < 0 , paste('ERROR',vector,sep=SEPARADOR_ERROR), vector)
    lista_resultante <- HuboError(vector,nuevoVector)
    
    return(lista_resultante)                      
  }
  
  validaNAOrNoNumerico <- function(vector){
    lista_resultante <- list(NULL,NULL)
    nuevoVector <- ifelse(is.na(vector) | sub("^\\s+", "", vector) == "" |  grepl("[^-0-9|0-9.|0-9e+0-9|0-9E+0-9]", vector) , paste('ERROR',vector,sep=SEPARADOR_ERROR), vector)
    lista_resultante <- HuboError(vector,nuevoVector)
    
    return(lista_resultante)                      
  }
  
  validaNAOrNoNumericoOrNegativo <- function(vector){
    lista_resultante <- list(NULL,NULL)
    nuevoVector <- ifelse(is.na(vector) | sub("^\\s+", "", vector) == "" | grepl("[^0-9|0-9.|0-9e+0-9|0-9E+0-9]", vector)  | vector < 0, paste('ERROR',vector,sep=SEPARADOR_ERROR), vector)
    lista_resultante <- HuboError(vector,nuevoVector)
    
    return(lista_resultante) 
  }
  
  validaNAOrNoCaracter <- function(vector){
    lista_resultante <- list(NULL,NULL)
    nuevoVector <- ifelse(is.na(vector) | !is.character(vector) , paste('ERROR',vector,sep=SEPARADOR_ERROR), vector)
    lista_resultante <- HuboError(vector,nuevoVector)
    
    return(lista_resultante) 
  }
  
  validaNAOrNoCaracterOrNoValor_s_n <- function(vector){
    lista_resultante <- list(NULL,NULL)
    nuevoVector <- ifelse(is.na(vector) | !is.character(vector) | (vector != 'n' & vector != 's'), paste('ERROR',vector,sep=SEPARADOR_ERROR), vector)
    lista_resultante <- HuboError(vector,nuevoVector)
    
    return(lista_resultante) 
    
  }
  
  validaNAOrNoCaracterOrNoValor_si_no <- function(vector){
    lista_resultante <- list(NULL,NULL)
    nuevoVector <- ifelse(is.na(vector) | !is.character(vector) | (vector != 'no' & vector != 'si'), paste('ERROR',vector,sep=SEPARADOR_ERROR), vector)
    lista_resultante <- HuboError(vector,nuevoVector)
    
    return(lista_resultante) 
    
  }
  
  validaNAOrNoCaracterOrNoValor_S_N <- function(vector){
    lista_resultante <- list(NULL,NULL)
    nuevoVector <- ifelse(is.na(vector) | !is.character(vector) | (vector != 'N' & vector != 'S'), paste('ERROR',vector,sep=SEPARADOR_ERROR), vector)
    lista_resultante <- HuboError(vector,nuevoVector)
    
    return(lista_resultante) 
  }
  
  
  flag_con_errores <- FALSE
  
  print("---Validando todos los campos")
  
  
  ######## Validando producto: 
  BCCA_TDP_DATA_PURE$produc_c <- ifelse(
    
    
    #Validando espacios en blanco
    is.na(BCCA_TDP_DATA_PURE$produc_c) |
      
      
      #Validando que el campo solo tenga valor "prepago"
      BCCA_TDP_DATA_PURE$produc_c != 'prepago'
    
    
    ,paste('ERROR',BCCA_TDP_DATA_PURE$produc_c,sep=SEPARADOR_ERROR) , BCCA_TDP_DATA_PURE$produc_c)
  
  lista_resultante <- list(NULL,NULL)
  #lista_resultante <- validaTelefono(BCCA_TDP_DATA_PURE$tcntel_n); if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$tcntel_n  <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)}
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$edad_n    )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$edad_n     <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$antig_n   )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$antig_n    <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$eqanti_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$eqanti_n   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rec4to_2_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rec4to_2_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rec4fi_2_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rec4fi_2_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rec4pr_2_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rec4pr_2_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$cntsol_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$cntsol_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep1to_1_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep1to_1_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep1fi_1_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep1fi_1_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep1to_2_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep1to_2_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep1fi_2_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep1fi_2_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep2to_1_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep2to_1_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep2fi_1_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep2fi_1_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep2to_2_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep2to_2_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep2fi_2_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep2fi_2_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep3to_2_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep3to_2_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep3fi_2_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep3fi_2_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld041_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld041_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld042_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld042_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld045_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld045_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld046_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld046_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld049_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld049_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld050_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld050_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld053_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld053_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld054_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld054_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld057_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld057_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld058_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld058_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld061_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld061_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld062_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld062_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld065_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld065_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld066_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld066_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld069_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld069_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld070_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld070_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld073_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld073_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$fld074_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$fld074_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoCaracterOrNoValor_S_N(BCCA_TDP_DATA_PURE$pli_sn_c  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$pli_sn_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$cant1m_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$cant1m_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$mont1m_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$mont1m_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$cant3m_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$cant3m_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$mont3m_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$mont3m_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$cant6m_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$cant6m_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$mont6m_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$mont6m_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$ctotal_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$ctotal_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumerico(BCCA_TDP_DATA_PURE$mtotal_n  )  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$mtotal_c   <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rec1to_3_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rec1to_3_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rec3to_3_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rec3to_3_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rec4to_3_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rec4to_3_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep1to_3_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep1to_3_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep2to_3_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep2to_3_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  lista_resultante <- validaNAOrNoNumericoOrNegativo(BCCA_TDP_DATA_PURE$rep3to_3_n)  ; if(lista_resultante[[1]]){ BCCA_TDP_DATA_PURE$rep3to_3_n <- lista_resultante[[2]] ; flag_con_errores <- TRUE ; lista_resultante <- list(NULL,NULL)} 
  
  #Extrayendo datasets con registros error:
  if(flag_con_errores){
    #Se agregará columna al inicio, indicando el número de línea (registro).
    nlines <- 2:(nrow(BCCA_TDP_DATA_PURE)+1)
    BCCA_TDP_DATA_PURE <- cbind(nlines,BCCA_TDP_DATA_PURE)
    colnames(BCCA_TDP_DATA_PURE)[1] <- "N° Fila"
    
    dataset_errores <- subset(BCCA_TDP_DATA_PURE, 
                              grepl("ERROR", BCCA_TDP_DATA_PURE$produc_c  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$edad_n    )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$antig_n   )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$eqanti_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rec4to_2_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rec4fi_2_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rec4pr_2_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$cntsol_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rep1to_1_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rep1fi_1_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rep1to_2_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rep1fi_2_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rep2to_1_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rep2fi_1_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rep2to_2_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rep2fi_2_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rep3to_2_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rep3fi_2_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld041_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld042_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld045_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld046_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld049_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld050_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld053_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld054_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld057_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld058_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld061_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld062_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld065_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld066_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld069_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld070_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld073_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$fld074_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$pli_sn_c  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$cant1m_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$mont1m_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$cant3m_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$mont3m_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$cant6m_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$mont6m_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$ctotal_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$mtotal_n  )  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rec1to_3_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rec3to_3_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rec4to_3_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rep1to_3_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rep2to_3_n)  |
                                grepl("ERROR", BCCA_TDP_DATA_PURE$rep3to_3_n)  )
    
    
    if(nrow(dataset_errores) > 180000){
      dataset_errores <- head(dataset_errores,180000)
    }
    
    #path_destino_errores <- paste(path_destino,'validation/',NOMBRE_DATASET_ERRORS, format(Sys.time(), "%d_%m_%y_%H_%M_%S"),EXTENSION_ARCHIVO_DESTINO, sep = '')
    path_destino_errores <- paste(path_destino,'validation/',NOMBRE_DATASET_ERRORS,EXTENSION_ARCHIVO_DESTINO, sep = '')
    lista_output <- list('INCORRECT_VALUES_IN_THE_FEATURES',NULL,NULL,NULL)
    write.csv(dataset_errores, path_destino_errores, row.names = FALSE, na = "")
    
    if(nrow(BCCA_TDP_DATA_PURE) == 0){
      return(lista_output)
    }else{
      lista_output <- list('INCORRECT_VALUES_IN_THE_FEATURES',NULL,NULL, path_destino_errores)
      return(lista_output)
    }
    
  }
  else{
    return(lista_output)
    stop('Se detuvo el proceso')
  }
  
} 