#@author: amanosalva
#@function: transformacionDatasetChurn(path_origen, path_destino, trama, token, opcion)
#@parameters: 
## - path_origen: DirecciÃ³n origen desde donde se leerÃ¡ el dataset en formato csv. VÃ¡lido para opcion="B"
## - path_destino: DirecciÃ³n destino donde se guardarÃ¡ los datasets transformado. Este path no contendrÃ¡ el nombre del archivo ni su extensiÃ³n. VÃ¡lido para opcion="B"
## - trama: Cadena que contiene un registro, donde cada caracterÃ�stica estarÃ¡ separado por token. VÃ¡lido para opcion="O"
## - token: Separador en la trama. VÃ¡lido para opcion="O"
## - opcion: Valores posibles: "B":Batch , "O":Online.
##          Si se elije la opciÃ³n por Batch (B), R cargarÃ¡ el dataset completo desde la ruta path_origen y exportarÃ¡ los datasets resultantes por producto a path_destino. En este caso, la funciÃ³n retorna null.
##          Si se elije la opciÃ³n Online (O), R recibirÃ¡ como parÃ¡metro la trama representando un registro, separado por el token y retornarÃ¡ como cadena la trama transformada, dependiendo del producto, separado por coma.
#Observaciones: * El path origen incluye el nombre del archivo con la extensiÃ³n, ademÃ¡s, como separador se utiliza "/".
#               


transformacionDatasetChurn <- function(path_origen, trama, token, opcion){
  
  out <- tryCatch(
    {  
      #Constantes
      VECTOR_PRODUCTOS <- c("prepago","postpago", "control")
      
      
      #Función para eliminar características innecesarias.
      eliminaCaracteristica <- function(dataset, caracteristicas){
        dataset <- dataset[, !(names(dataset) %in% caracteristicas)]
        return(dataset)
      }
      
      
      
      switch(opcion, 
             #Opcion Batch
             B = {
               #Lee csv
               library(readr)
               V4DATOFIN_pure <- read_csv(path_origen, 
                                          col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                                           "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                                           "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                                           "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                                           "fld146_n"= col_double(), "cant1m_n"= col_double(), 
                                                           "mont1m_n"= col_double(), "cant3m_n"= col_double(), 
                                                           "mont3m_n"= col_double(), "cant6m_n"= col_double(), 
                                                           "mont6m_n"= col_double(), "ctotal_n"= col_double(), 
                                                           "mtotal_n"= col_double()))
               
               #Verifica niveles por producto
               if(length(levels(as.factor(V4DATOFIN_pure$produc_c))) == 1){
                 flagVariosNiveles <- FALSE
                 productos <- levels(as.factor(V4DATOFIN_pure$produc_c))
                 
               }else{
                 flagVariosNiveles <- TRUE
                 productos <- levels(as.factor(V4DATOFIN_pure$produc_c))
                 
                 
                 
                 #Separa dataset por niveles del producto
               }
               
               
             },
             
             O = {
               flag_trama_prepago <- FALSE
               flag_trama_postpago <- FALSE
               flag_trama_control <- FALSE
               
               #FunciÃ³n para recibir tramas asignÃ¡ndole los nombres especÃ�ficos del dataset puro.
               recibeTrama <- function(trama,tokenp){
                 registro <- as.data.frame(do.call(rbind,strsplit(trama,tokenp)))
                 names(registro) <- c("edad_n","antig_n","numlin_n","produc_c", 
                                      "gama_c","eqaseg_c","eqanti_n","linper_c",
                                      "costpl_n","conren_c","cnttoc_n","cntnrc_n",
                                      "cnttmc_n","cntctd_n","cntctl_n","cntctl_1_n",
                                      "cntctl_2_n","cntsol_n","f19_n","f20_n","f21_n",
                                      "fld041_n","fld045_n","fld049_n","fld053_n",
                                      "fld054_n","fld055_n","fld056_n","fld057_n",
                                      "fld058_n","fld059_n","fld060_n","fld061_n",
                                      "fld062_n","fld063_n","fld064_n","fld065_n",
                                      "fld066_n","fld067_n","fld068_n","fld069_n",
                                      "fld070_n","fld071_n","fld072_n","fld073_n",
                                      "fld115_n","fld116_n","fld117_n","fld119_c",
                                      "fld120_c","fld121_c","fld122_c","fld138_n",
                                      "fld139_n","fld140_n","fld141_n","fld142_n",
                                      "fld143_n","fld144_n","fld145_n","fld146_n",
                                      "pli_sn_c","cant1m_n","mont1m_n","cant3m_n",
                                      "mont3m_n","cant6m_n","mont6m_n","ctotal_n",
                                      "mtotal_n","rec1to_3_n","rec2to_3_n","rec3to_3_n",
                                      "rec4to_3_n","rep1to_3_n","rep2to_3_n","rep3to_3_n",
                                      "rep4to_3_n","depart_c","provi_c","target_c")
                 #AsignaciÃ³n del tipo de dato. Convierte factor a numÃ©rico
                 registro[,c("edad_n","antig_n","numlin_n", "eqanti_n","costpl_n","cnttoc_n","cntnrc_n",
                             "cnttmc_n","cntctd_n","cntctl_n","cntctl_1_n","cntctl_2_n","cntsol_n",
                             "f19_n","f20_n","f21_n","fld041_n","fld045_n","fld049_n","fld053_n",
                             "fld054_n","fld055_n","fld056_n","fld057_n","fld058_n","fld059_n",
                             "fld060_n","fld061_n","fld062_n","fld063_n","fld064_n","fld065_n",
                             "fld066_n","fld067_n","fld068_n","fld069_n","fld070_n","fld071_n",
                             "fld072_n","fld073_n","fld115_n","fld116_n","fld117_n", "fld138_n","fld139_n","fld140_n","fld141_n",
                             "fld142_n","fld143_n","fld144_n","fld145_n","fld146_n","cant1m_n",
                             "mont1m_n","cant3m_n","mont3m_n","cant6m_n","mont6m_n","ctotal_n",
                             "mtotal_n","rec1to_3_n","rec2to_3_n","rec3to_3_n","rec4to_3_n",
                             "rep1to_3_n","rep2to_3_n","rep3to_3_n","rep4to_3_n")] <- as.numeric(as.character(unlist(registro[,c("edad_n","antig_n","numlin_n","eqanti_n","costpl_n","cnttoc_n","cntnrc_n",
                                                                                                                                 "cnttmc_n","cntctd_n","cntctl_n","cntctl_1_n","cntctl_2_n","cntsol_n",
                                                                                                                                 "f19_n","f20_n","f21_n","fld041_n","fld045_n","fld049_n","fld053_n","fld054_n",
                                                                                                                                 "fld055_n","fld056_n","fld057_n","fld058_n","fld059_n","fld060_n","fld061_n",
                                                                                                                                 "fld062_n","fld063_n","fld064_n","fld065_n","fld066_n","fld067_n","fld068_n",
                                                                                                                                 "fld069_n","fld070_n","fld071_n","fld072_n","fld073_n","fld115_n","fld116_n","fld117_n","fld138_n","fld139_n",
                                                                                                                                 "fld140_n","fld141_n","fld142_n","fld143_n","fld144_n","fld145_n","fld146_n",
                                                                                                                                 "cant1m_n","mont1m_n","cant3m_n","mont3m_n","cant6m_n","mont6m_n","ctotal_n",
                                                                                                                                 "mtotal_n","rec1to_3_n","rec2to_3_n","rec3to_3_n","rec4to_3_n","rep1to_3_n",
                                                                                                                                 "rep2to_3_n","rep3to_3_n","rep4to_3_n")])))
                 #AsignaciÃ³n del tipo de dato. Convierte factor a character
                 registro[,c("produc_c","gama_c","eqaseg_c", 
                             "linper_c","conren_c",
                             "fld119_c",
                             "fld120_c","fld121_c","fld122_c", 
                             "pli_sn_c","depart_c","provi_c","target_c")] <- as.character(unlist(registro[,c("produc_c","gama_c","eqaseg_c", "linper_c","conren_c",
                                                                                                             "fld119_c","fld120_c","fld121_c","fld122_c", "pli_sn_c","depart_c","provi_c","target_c")]))
                 
                 return(registro)
               }
               V4DATOFIN_pure <- recibeTrama(trama,token)},
             
             {
               stop("OPCION INVÁLIDA")
               
             })                                                 
      
      #Separando dataset PREPAGO
      if(VECTOR_PRODUCTOS[1] %in% productos){
        V4DATOFIN_pure_prepago<- subset(V4DATOFIN_pure, V4DATOFIN_pure$produc_c == VECTOR_PRODUCTOS[1])
        V4DATOFIN_pure_prepago <- eliminaCaracteristica(V4DATOFIN_pure_prepago, c("produc_c"))
        producto_trama <- VECTOR_PRODUCTOS[1]
        flag_trama_prepago <- TRUE
        print("TRANSFORMANDO DATA PREPAGO")
        
        
        #EliminaciÃ³n de las variables que no corresponden a cada campo según documentación por producto. (VALIDADO CON ROBERTO)
        V4DATOFIN_pure_prepago <- eliminaCaracteristica(V4DATOFIN_pure_prepago, c("linper_c","costpl_n","conren_c",
                                                                                  "cnttoc_n","cntnrc_n","cnttmc_n",
                                                                                  "cntctd_n","cntctl_n","cntctl_1_n",
                                                                                  "cntctl_2_n","fld115_n","fld116_n",
                                                                                  "fld117_n","fld119_c","fld120_c",
                                                                                  "fld121_c","fld122_c","fld138_n",
                                                                                  "fld139_n","fld140_n","fld141_n",
                                                                                  "fld142_n","fld143_n","fld144_n",
                                                                                  "fld145_n","fld146_n"))
        
        #EliminaciÃ³n por tÃ©cnicas de filtrado
        V4DATOFIN_pure_prepago <- eliminaCaracteristica(V4DATOFIN_pure_prepago, c("mont6m_n","cant6m_n","rep2to_3_n",
                                                                                  "fld064_n","fld061_n","depart_c",
                                                                                  "fld062_n","fld058_n","rep1to_3_n",
                                                                                  "fld059_n","mont3m_n","fld055_n",
                                                                                  "fld071_n","cant3m_n","fld073_n",
                                                                                  "fld063_n","fld068_n","rec2to_3_n",
                                                                                  "rec3to_3_n","rec4to_3_n","rep3to_3_n",
                                                                                  "rep4to_3_n"))
        
        #Eliminando variables con problema de trÃ¡fico (mÃ¡ximo 2 meses)
        V4DATOFIN_pure_prepago <- eliminaCaracteristica(V4DATOFIN_pure_prepago, c("fld055_n","fld056_n","fld059_n",
                                                                                  "fld060_n","fld063_n","fld064_n",
                                                                                  "fld067_n","fld068_n","fld071_n",
                                                                                  "fld072_n"))
        
        #Eliminando variables directas o innecesarias
        V4DATOFIN_pure_prepago <- eliminaCaracteristica(V4DATOFIN_pure_prepago, c("f19_n","f20_n","f21_n","provi_c","depart_c"))
        
        
        #numlin_n
        numlin_n <- V4DATOFIN_pure_prepago$numlin_n
        V4DATOFIN_pure_prepago$numlin_n <- ifelse(numlin_n > 0 & numlin_n <= 1, "(0-1]", 
                                                  ifelse(numlin_n > 1 & numlin_n <= 2, "(1-2]", 
                                                         ifelse(numlin_n > 2 & numlin_n <= 3, "(2-3]", 
                                                                ifelse(numlin_n > 3 & numlin_n <= 5, "(3-5]", 
                                                                       ifelse(numlin_n > 5 & numlin_n <= 10, "(5-10]", 
                                                                              ifelse(numlin_n > 10, "(10-inf)", numlin_n))))))
        
        
        rm(numlin_n)
        
        #cntsol_n
        cntsol_n <- V4DATOFIN_pure_prepago$cntsol_n
        V4DATOFIN_pure_prepago$cntsol_n <- ifelse(cntsol_n > 0, "si", "no")
        rm(cntsol_n)
        
        #fld041_n
        fld041_n <- V4DATOFIN_pure_prepago$fld041_n
        V4DATOFIN_pure_prepago$fld041_n <- ifelse(fld041_n > 0, "si", "no")
        rm(fld041_n)
        
        #fld045_n
        fld045_n <- V4DATOFIN_pure_prepago$fld045_n
        V4DATOFIN_pure_prepago$fld045_n <- ifelse(fld045_n > 0, "si", "no")
        rm(fld045_n)
        
        #fld049_n
        fld049_n <- V4DATOFIN_pure_prepago$fld049_n
        V4DATOFIN_pure_prepago$fld049_n <- ifelse(fld049_n > 0, "si", "no")
        rm(fld049_n)
        
        #fld053_n
        fld053_n <- V4DATOFIN_pure_prepago$fld053_n
        V4DATOFIN_pure_prepago$fld053_n <- ifelse(fld053_n == 0, "[0]", 
                                                  ifelse(fld053_n > 0 & fld053_n <= 50, "(0-50]", 
                                                         ifelse(fld053_n > 50 & fld053_n <= 100, "(50-100]", 
                                                                ifelse(fld053_n > 100 & fld053_n <= 300, "(100-300]", 
                                                                       ifelse(fld053_n > 300, "(300-inf)", fld053_n)))))
        rm(fld053_n)
        
        #fld054_n
        fld054_n <- V4DATOFIN_pure_prepago$fld054_n
        V4DATOFIN_pure_prepago$fld054_n <- ifelse(fld054_n == 0, "[0]", 
                                                  ifelse(fld054_n > 0 & fld054_n <= 5, "(0-5]", 
                                                         ifelse(fld054_n > 5 & fld054_n <= 10, "(5-10]", 
                                                                ifelse(fld054_n > 10 & fld054_n <= 15, "(10-15]",
                                                                       ifelse(fld054_n > 15 & fld054_n <= 40, "(15-40]",
                                                                              ifelse(fld054_n > 40, "(40-inf)", fld054_n))))))
        
        rm(fld054_n)
        
        
        #fld057_n
        fld057_n <- V4DATOFIN_pure_prepago$fld057_n
        V4DATOFIN_pure_prepago$fld057_n <- ifelse(fld057_n == 0, "[0]", 
                                                  ifelse(fld057_n > 0 & fld057_n <= 50, "(0-50]", 
                                                         ifelse(fld057_n > 50 & fld057_n <= 100, "(50-100]", 
                                                                ifelse(fld057_n > 100 & fld057_n <= 300, "(100-300]", 
                                                                       ifelse(fld057_n > 300, "(300-inf)", fld057_n)))))
        rm(fld057_n)
        
        
        #fld065_n
        fld065_n <- V4DATOFIN_pure_prepago$fld065_n
        V4DATOFIN_pure_prepago$fld065_n <- ifelse(fld065_n > 0, "si", "no")
        rm(fld065_n)
        
        
        #fld066_n
        fld066_n <- V4DATOFIN_pure_prepago$fld066_n
        V4DATOFIN_pure_prepago$fld066_n <- ifelse(fld066_n > 0, "si", "no")
        rm(fld066_n)
        
        #fld069_n
        fld069_n <- V4DATOFIN_pure_prepago$fld069_n
        V4DATOFIN_pure_prepago$fld069_n <- ifelse(fld069_n == 0, "[0]", 
                                                  ifelse(fld069_n > 0 & fld069_n <= 50, "(0-50]", 
                                                         ifelse(fld069_n > 50 & fld069_n <= 150, "(50-150]",
                                                                ifelse(fld069_n > 150 & fld069_n <= 300, "(150-300]", 
                                                                       ifelse(fld069_n > 300 & fld069_n <= 700, "(300-700]", 
                                                                              ifelse(fld069_n > 700, "(700-inf)", fld069_n))))))
        rm(fld069_n)
        
        #fld070_n
        fld070_n <- V4DATOFIN_pure_prepago$fld070_n
        V4DATOFIN_pure_prepago$fld070_n <- ifelse(fld070_n == 0, "[0]", 
                                                  ifelse(fld070_n > 0 & fld070_n <= 25, "(0-25]",
                                                         ifelse(fld070_n > 25 & fld070_n <= 100, "(25-100]", 
                                                                ifelse(fld070_n > 100, "(100-inf)", fld070_n))))
        rm(fld070_n)
        
        #cant1m_n
        cant1m_n <- V4DATOFIN_pure_prepago$cant1m_n
        V4DATOFIN_pure_prepago$cant1m_n <- ifelse(cant1m_n == 0, "[0]", 
                                                  ifelse(cant1m_n > 0 & cant1m_n <= 2, "(0-2]", 
                                                         ifelse(cant1m_n > 2 & cant1m_n <= 5, "(2-5]", 
                                                                ifelse(cant1m_n > 5, "(5-inf)", cant1m_n))))
        rm(cant1m_n)
        
        #mont1m_n 
        mont1m_n <- V4DATOFIN_pure_prepago$mont1m_n
        V4DATOFIN_pure_prepago$mont1m_n <- ifelse(mont1m_n == 0, "[0]",
                                                  ifelse(mont1m_n > 0 & mont1m_n <= 3, "(0-3]", 
                                                         ifelse(mont1m_n > 3 & mont1m_n <= 5, "(3-5]", 
                                                                ifelse(mont1m_n > 5 & mont1m_n <= 10, "(5-10]", 
                                                                       ifelse(mont1m_n > 10 & mont1m_n <= 20, "(10-20]", 
                                                                              ifelse(mont1m_n > 20, "(20-inf)", mont1m_n))))))
        rm(mont1m_n)
        
        #ctotal_n
        ctotal_n <- V4DATOFIN_pure_prepago$ctotal_n
        V4DATOFIN_pure_prepago$ctotal_n <- ifelse(ctotal_n == 0, "[0]", 
                                                  ifelse(ctotal_n > 0 & ctotal_n <= 10, "(0-10]", 
                                                         ifelse(ctotal_n > 10 & ctotal_n <= 20, "(10-20]", 
                                                                ifelse(ctotal_n > 20 & ctotal_n <= 30, "(20-30]", 
                                                                       ifelse(ctotal_n > 30 & ctotal_n <= 40, "(30-40]", 
                                                                              ifelse(ctotal_n > 40, "(40-inf)", ctotal_n))))))
        
        
        rm(ctotal_n)
        
        #mtotal_n
        mtotal_n <- V4DATOFIN_pure_prepago$mtotal_n
        V4DATOFIN_pure_prepago$mtotal_n <- ifelse(mtotal_n == 0, "[0]",
                                                  ifelse(mtotal_n > 0 & mtotal_n <= 18, "(0-18]", 
                                                         ifelse(mtotal_n > 18 & mtotal_n <= 30, "(18-30]",
                                                                ifelse(mtotal_n > 30 & mtotal_n <= 60, "(30-60]",
                                                                       ifelse(mtotal_n > 60 & mtotal_n <= 120, "(60-120]",
                                                                              ifelse(mtotal_n > 120, "(120-inf)", mtotal_n))))))
        
        
        rm(mtotal_n)
        
        
        #rec1to_3_n
        rec1to_3_n <- V4DATOFIN_pure_prepago$rec1to_3_n
        V4DATOFIN_pure_prepago$rec1to_3_n <- ifelse(rec1to_3_n > 0, "si", "no")
        rm(rec1to_3_n)
        
        #Asignando nuevos nombres
        names(V4DATOFIN_pure_prepago) <- c("edad_n","antig_n","numlin_c","gama_c","eqaseg_c","eqanti_n",
                                           "cntsol_c","fld041_c","fld045_c","fld049_c","fld053_c",
                                           "fld054_c","fld057_c","fld065_c","fld066_c","fld069_c",
                                           "fld070_c","pli_sn_c","cant1m_c","mont1m_c",
                                           "ctotal_c","mtotal_c","rec1to_3_c","target_c")
        
      }
      
      #Separando dataset POSTPAGO
      if(VECTOR_PRODUCTOS[2] %in% productos){
        producto_trama <- VECTOR_PRODUCTOS[1]
        V4DATOFIN_pure_postpago<- subset(V4DATOFIN_pure, V4DATOFIN_pure$produc_c == "postpago")
        V4DATOFIN_pure_postpago <- eliminaCaracteristica(V4DATOFIN_pure_postpago, c("produc_c"))
        flag_trama_postpago <- TRUE
        print("TRANSFORMANDO DATA POSTPAGO")
        
        
        #EliminaciÃ³n de las variables que no corresponden a cada campo segÃºn documentaciÃ³n por producto. (VALIDADO CON ROBERTO)
        V4DATOFIN_pure_postpago <- eliminaCaracteristica(V4DATOFIN_pure_postpago, c("pli_sn_c","cant1m_n","mont1m_n",
                                                                                    "cant3m_n","mont3m_n","cant6m_n",
                                                                                    "mont6m_n","ctotal_n","mtotal_n")) 
        
        #Eliminando las variables con menos peso:
        V4DATOFIN_pure_postpago <- eliminaCaracteristica(V4DATOFIN_pure_postpago, c("fld143_n","fld059_n","edad_n","fld120_c",
                                                                                    "cntctl_n","fld144_n","rec2to_3_n","fld139_n",
                                                                                    "f21_n","rep3to_3_n","rep4to_3_n","rec1to_3_n",
                                                                                    "cntctl_2_n","fld117_n","rec3to_3_n","fld119_c",
                                                                                    "fld053_n","cntctl_1_n","gama_c",
                                                                                    "rec4to_3_n","fld121_c","fld063_n","fld145_n",
                                                                                    "fld056_n","fld057_n","fld069_n","fld072_n",
                                                                                    "eqaseg_c","depart_c","fld060_n","fld122_c",
                                                                                    "fld061_n","fld064_n","fld068_n"))
        
        #Eliminando variables con problema de trÃ¡fico (mÃ¡ximo 2 meses)
        V4DATOFIN_pure_postpago <- eliminaCaracteristica(V4DATOFIN_pure_postpago, c("fld055_n","fld056_n","fld059_n",
                                                                                    "fld060_n","fld063_n","fld064_n",
                                                                                    "fld067_n","fld068_n","fld071_n",
                                                                                    "fld072_n"))
        
        #Eliminando variables directas o innecesarias
        V4DATOFIN_pure_postpago <- eliminaCaracteristica(V4DATOFIN_pure_postpago, c("f19_n","f20_n","f21_n","provi_c","depart_c"))
        
        
        
        #numlin_n
        numlin_n <- V4DATOFIN_pure_postpago$numlin_n
        V4DATOFIN_pure_postpago$numlin_n <- ifelse(numlin_n > 0 & numlin_n <= 1, "(0-1]", 
                                                   ifelse(numlin_n > 1 & numlin_n <= 2, "(1-2]", 
                                                          ifelse(numlin_n > 2 & numlin_n <= 3, "(2-3]", 
                                                                 ifelse(numlin_n > 3 & numlin_n <= 5, "(3-5]", 
                                                                        ifelse(numlin_n > 5 & numlin_n <= 10, "(5-10]", 
                                                                               ifelse(numlin_n > 10, "(10-inf)", numlin_n))))))
        
        
        rm(numlin_n)
        
        #cntsol_n
        cntsol_n <- V4DATOFIN_pure_postpago$cntsol_n
        V4DATOFIN_pure_postpago$cntsol_n <- ifelse(cntsol_n > 0, "si", "no")
        rm(cntsol_n)
        
        
        #fld041_n
        fld041_n <- V4DATOFIN_pure_postpago$fld041_n
        V4DATOFIN_pure_postpago$fld041_n <- ifelse(fld041_n > 0, "si", "no")
        rm(fld041_n)
        
        #fld045_n
        fld045_n <- V4DATOFIN_pure_postpago$fld045_n
        V4DATOFIN_pure_postpago$fld045_n <- ifelse(fld045_n > 0, "si", "no")
        rm(fld045_n)
        
        #fld049_n
        fld049_n <- V4DATOFIN_pure_postpago$fld049_n
        V4DATOFIN_pure_postpago$fld049_n <- ifelse(fld049_n > 0, "si", "no")
        rm(fld049_n)
        
        #fld054_n
        fld054_n <- V4DATOFIN_pure_postpago$fld054_n
        V4DATOFIN_pure_postpago$fld054_n <- ifelse(fld054_n == 0, "[0]", 
                                                   ifelse(fld054_n > 0 & fld054_n <= 5, "(0-5]", 
                                                          ifelse(fld054_n > 5 & fld054_n <= 10, "(5-10]", 
                                                                 ifelse(fld054_n > 10 & fld054_n <= 15, "(10-15]",
                                                                        ifelse(fld054_n > 15 & fld054_n <= 40, "(15-40]",
                                                                               ifelse(fld054_n > 40, "(40-inf)", fld054_n))))))
        
        rm(fld054_n)
        
        #fld058_n
        fld058_n <- V4DATOFIN_pure_postpago$fld058_n
        V4DATOFIN_pure_postpago$fld058_n <- ifelse(fld058_n == 0, "[0]", 
                                                   ifelse(fld058_n > 0 & fld058_n <= 5, "(0-5]", 
                                                          ifelse(fld058_n > 5 & fld058_n <= 10, "(5-10]", 
                                                                 ifelse(fld058_n > 10 & fld058_n <= 15, "(10-15]",
                                                                        ifelse(fld058_n > 15 & fld058_n <= 40, "(15-40]",
                                                                               ifelse(fld058_n > 40, "(40-inf)", fld058_n))))))
        
        rm(fld058_n)
        
        #fld062_n
        fld062_n <- V4DATOFIN_pure_postpago$fld062_n
        V4DATOFIN_pure_postpago$fld062_n <- ifelse(fld062_n == 0, "[0]", 
                                                   ifelse(fld062_n > 0 & fld062_n <= 5, "(0-5]", 
                                                          ifelse(fld062_n > 5 & fld062_n <= 10, "(5-10]", 
                                                                 ifelse(fld062_n > 10 & fld062_n <= 15, "(10-15]",
                                                                        ifelse(fld062_n > 15 & fld062_n <= 40, "(15-40]",
                                                                               ifelse(fld062_n > 40, "(40-inf)", fld062_n))))))
        rm(fld062_n)
        
        #fld065_n
        fld065_n <- V4DATOFIN_pure_postpago$fld065_n
        V4DATOFIN_pure_postpago$fld065_n <- ifelse(fld065_n > 0, "si", "no")
        rm(fld065_n)
        
        #fld066_n
        fld066_n <- V4DATOFIN_pure_postpago$fld066_n
        V4DATOFIN_pure_postpago$fld066_n <- ifelse(fld066_n > 0, "si", "no")
        rm(fld066_n)
        
        #fld070_n
        fld070_n <- V4DATOFIN_pure_postpago$fld070_n
        V4DATOFIN_pure_postpago$fld070_n <- ifelse(fld070_n == 0, "[0]", 
                                                   ifelse(fld070_n > 0 & fld070_n <= 25, "(0-25]",
                                                          ifelse(fld070_n > 25 & fld070_n <= 100, "(25-100]", 
                                                                 ifelse(fld070_n > 100, "(100-inf)", fld070_n))))
        rm(fld070_n)
        
        #fld073_n
        fld073_n <- V4DATOFIN_pure_postpago$fld073_n
        V4DATOFIN_pure_postpago$fld073_n <- ifelse(fld073_n > 0, "si", "no")
        rm(fld073_n)
        
        
        #fld115_n
        fld115_n <- V4DATOFIN_pure_postpago$fld115_n
        V4DATOFIN_pure_postpago$fld115_n <- ifelse(fld115_n == 0, "[0]", 
                                                   ifelse(fld115_n > 0 & fld115_n <= 4000, "limitado",
                                                          ifelse(fld115_n > 4000, "ilimitado", fld115_n)))
        rm(fld115_n)
        
        #fld116_n
        fld116_n <- V4DATOFIN_pure_postpago$fld116_n
        V4DATOFIN_pure_postpago$fld116_n <- ifelse(fld116_n == 0, "[0]", 
                                                   ifelse(fld116_n > 0 & fld116_n <= 20000000, "limitado",
                                                          ifelse(fld116_n > 20000000, "ilimitado", fld116_n)))
        rm(fld116_n)
        
        #rep1to_3_n
        rep1to_3_n <- V4DATOFIN_pure_postpago$rep1to_3_n
        V4DATOFIN_pure_postpago$rep1to_3_n <- ifelse(rep1to_3_n > 0, "si", "no")
        rm(rep1to_3_n)
        
        #rep2to_3_n
        rep2to_3_n <- V4DATOFIN_pure_postpago$rep2to_3_n
        V4DATOFIN_pure_postpago$rep2to_3_n <- ifelse(rep2to_3_n > 0, "si", "no")
        rm(rep2to_3_n)
        
        names(V4DATOFIN_pure_postpago) <- c("antig_n","numlin_c","eqanti_n","linper_c","costpl_n",
                                            "conren_c","cnttoc_n","cntnrc_n","cnttmc_n","cntctd_n",
                                            "cntsol_c","fld041_c","fld045_c","fld049_c","fld054_c",
                                            "fld058_c","fld062_c","fld065_c","fld066_c",	"fld070_c",
                                            "fld073_c","fld115_c","fld116_c","fld138_n","fld140_n",
                                            "fld141_n","fld142_n","fld146_n","rep1to_3_c","rep2to_3_c",
                                            "target_c" )
        
        
      }
      
      if(VECTOR_PRODUCTOS[3] %in% productos){
        producto_trama <- VECTOR_PRODUCTOS[1]
        V4DATOFIN_pure_control<- subset(V4DATOFIN_pure, V4DATOFIN_pure$produc_c == "control")
        V4DATOFIN_pure_control <- eliminaCaracteristica(V4DATOFIN_pure_control, c("produc_c"))
        flag_trama_control <- TRUE
        print("TRANSFORMANDO DATA CONTROL")
        
        #Eliminación de las variables que no corresponden a cada campo segÃºn documentaciÃ³n por producto. (VALIDADO CON ROBERTO)
        V4DATOFIN_pure_control <- eliminaCaracteristica(V4DATOFIN_pure_control,c("pli_sn_c","cant1m_n","mont1m_n",
                                                                                 "cant3m_n","mont3m_n","cant6m_n",
                                                                                 "mont6m_n","ctotal_n","mtotal_n")) 
        
        #Eliminando las variables con menos peso:
        V4DATOFIN_pure_control <- eliminaCaracteristica(V4DATOFIN_pure_control, c("fld119_c","fld120_c","rep2to_3_n","fld121_c",
                                                                                  "fld055_n","cntctl_1_n","linper_c","rec3to_3_n",
                                                                                  "rec2to_3_n","fld145_n","fld072_n","edad_n",
                                                                                  "rec1to_3_n","fld059_n","fld073_n","fld117_n",
                                                                                  "rep3to_3_n","rep4to_3_n","rec4to_3n",
                                                                                  "fld056_n","fld069_n","fld122_c","fld063_n",
                                                                                  "eqaseg_c","numlin_n","fld053_n","fld067_n",
                                                                                  "fld060_n","provi_c","gama_c","fld064_n",
                                                                                  "fld057_n","fld061_n","fld068_n"))
        #Eliminando variables con problema de trÃ¡fico (mÃ¡ximo 2 meses)
        V4DATOFIN_pure_control <- eliminaCaracteristica(V4DATOFIN_pure_control, c("fld055_n","fld056_n","fld059_n",
                                                                                  "fld060_n","fld063_n","fld064_n",
                                                                                  "fld067_n","fld068_n","fld071_n",
                                                                                  "fld072_n"))
        
        #Eliminando variables directas o innecesarias
        V4DATOFIN_pure_control <- eliminaCaracteristica(V4DATOFIN_pure_control, c("f19_n","f20_n","f21_n","provi_c","depart_c"))
        
        
        
        #cntctl_n
        cntctl_n <- V4DATOFIN_pure_control$cntctl_n
        V4DATOFIN_pure_control$cntctl_n <- ifelse(cntctl_n > 0, "si", "no")
        rm(cntctl_n)
        
        #cntctl_2_n
        cntctl_2_n <- V4DATOFIN_pure_control$cntctl_2_n
        V4DATOFIN_pure_control$cntctl_2_n <- ifelse(cntctl_2_n > 0, "si", "no")
        rm(cntctl_2_n)
        
        #cntsol_n
        cntsol_n <- V4DATOFIN_pure_control$cntsol_n
        V4DATOFIN_pure_control$cntsol_n <- ifelse(cntsol_n > 0, "si", "no")
        rm(cntsol_n)
        
        #fld041_n
        fld041_n <- V4DATOFIN_pure_control$fld041_n
        V4DATOFIN_pure_control$fld041_n <- ifelse(fld041_n > 0, "si", "no")
        rm(fld041_n)
        
        #fld045_n
        fld045_n <- V4DATOFIN_pure_control$fld045_n
        V4DATOFIN_pure_control$fld045_n <- ifelse(fld045_n > 0, "si", "no")
        rm(fld045_n)
        
        #fld049_n
        fld049_n <- V4DATOFIN_pure_control$fld049_n
        V4DATOFIN_pure_control$fld049_n <- ifelse(fld049_n > 0, "si", "no")
        rm(fld049_n)
        
        #fld054_n
        fld054_n <- V4DATOFIN_pure_control$fld054_n
        V4DATOFIN_pure_control$fld054_n <- ifelse(fld054_n == 0, "[0]", 
                                                  ifelse(fld054_n > 0 & fld054_n <= 5, "(0-5]", 
                                                         ifelse(fld054_n > 5 & fld054_n <= 10, "(5-10]", 
                                                                ifelse(fld054_n > 10 & fld054_n <= 15, "(10-15]",
                                                                       ifelse(fld054_n > 15 & fld054_n <= 40, "(15-40]", 
                                                                              ifelse(fld054_n > 40, "(40-inf)",fld054_n))))))
        rm(fld054_n)
        
        #fld058_n
        fld058_n <- V4DATOFIN_pure_control$fld058_n
        V4DATOFIN_pure_control$fld058_n <- ifelse(fld058_n == 0, "[0]", 
                                                  ifelse(fld058_n > 0 & fld058_n <= 5, "(0-5]", 
                                                         ifelse(fld058_n > 5 & fld058_n <= 10, "(5-10]", 
                                                                ifelse(fld058_n > 10 & fld058_n <= 15, "(10-15]",
                                                                       ifelse(fld058_n > 15 & fld058_n <= 40, "(15-40]", 
                                                                              ifelse(fld058_n > 40, "(40-inf)",fld058_n))))))
        rm(fld058_n) 
        
        #fld062_n
        fld062_n <- V4DATOFIN_pure_control$fld062_n
        V4DATOFIN_pure_control$fld062_n <- ifelse(fld062_n == 0, "[0]", 
                                                  ifelse(fld062_n > 0 & fld062_n <= 5, "(0-5]", 
                                                         ifelse(fld062_n > 5 & fld062_n <= 10, "(5-10]", 
                                                                ifelse(fld062_n > 10 & fld062_n <= 15, "(10-15]",
                                                                       ifelse(fld062_n > 15 & fld062_n <= 40, "(15-40]", 
                                                                              ifelse(fld062_n > 40, "(40-inf)",fld062_n))))))
        rm(fld062_n) 
        
        
        #fld065_n
        fld065_n <- V4DATOFIN_pure_control$fld065_n
        V4DATOFIN_pure_control$fld065_n <- ifelse(fld065_n > 0, "si", "no")
        rm(fld065_n)
        
        #fld066_n
        fld066_n <- V4DATOFIN_pure_control$fld066_n
        V4DATOFIN_pure_control$fld066_n <- ifelse(fld066_n > 0, "si", "no")
        rm(fld066_n)
        
        #fld070_n
        fld070_n <- V4DATOFIN_pure_control$fld070_n
        V4DATOFIN_pure_control$fld070_n <- ifelse(fld070_n == 0, "[0]", 
                                                  ifelse(fld070_n > 0 & fld070_n <= 25, "(0-25]",
                                                         ifelse(fld070_n > 25 & fld070_n <= 100, "(25-100]", 
                                                                ifelse(fld070_n > 100, "(100-inf)", fld070_n))))
        rm(fld070_n)
        
        #fld115_n
        fld115_n <- V4DATOFIN_pure_control$fld115_n
        V4DATOFIN_pure_control$fld115_n <- ifelse(fld115_n == 0, "[0]", 
                                                  ifelse(fld115_n > 0 & fld115_n <= 4000, "limitado",
                                                         ifelse(fld115_n > 4000, "ilimitado", fld115_n)))
        rm(fld115_n)
        
        #fld116_n
        fld116_n <- V4DATOFIN_pure_control$fld116_n
        V4DATOFIN_pure_control$fld116_n <- ifelse(fld116_n == 0, "[0]", 
                                                  ifelse(fld116_n > 0 & fld116_n <= 20000000, "limitado",
                                                         ifelse(fld116_n > 20000000, "ilimitado", fld116_n)))
        rm(fld116_n)
        
        #rep1to_3_n
        rep1to_3_n <- V4DATOFIN_pure_control$rep1to_3_n
        V4DATOFIN_pure_control$rep1to_3_n <- ifelse(rep1to_3_n > 0, "si", "no")
        rm(rep1to_3_n)
        
        #rec4to_3_n
        rec4to_3_n <- V4DATOFIN_pure_control$rec4to_3_n
        V4DATOFIN_pure_control$rec4to_3_n <- ifelse(rec4to_3_n > 0, "si", "no")
        rm(rec4to_3_n)
        
        names(V4DATOFIN_pure_control) <- c("antig_n","eqanti_n","costpl_n","conren_c","cnttoc_n",
                                           "cntnrc_n","cnttmc_n","cntctd_n","cntctl_c","cntctl_2_c",
                                           "cntsol_c","fld041_c","fld045_c","fld049_c","fld054_c",
                                           "fld058_c","fld062_c","fld065_c","fld066_c","fld070_c",
                                           "fld115_c","fld116_c","fld138_n","fld139_n","fld140_n",
                                           "fld141_n","fld142_n","fld143_n","fld144_n","fld146_n",
                                           "rec4to_3_c","rep1to_3_c","target_c" )
      }
      
      
      
      
      
      
      
      
      
      print("EXPORTANDO DATA")
      
      switch(opcion, 
             B = {
               #Caso 1: Se ingresÃ³ el path destino: opcion = "batch"
               #Genera excel:
               if(substr(path_destino,nchar(path_destino),nchar(path_destino)) == "/"){
                 write.csv(V4DATOFIN_pure_prepago, paste(path_destino,"V4DATOFIN_pure_prepago_final.csv",sep = ""), row.names = FALSE)
                 write.csv(V4DATOFIN_pure_postpago, paste(path_destino,"V4DATOFIN_pure_postpago_final.csv",sep = ""), row.names = FALSE)
                 write.csv(V4DATOFIN_pure_control, paste(path_destino,"V4DATOFIN_pure_control_final.csv",sep = ""), row.names = FALSE)
               }else{
                 write.csv(V4DATOFIN_pure_prepago, paste(path_destino,"/V4DATOFIN_pure_prepago_final.csv",sep = ""), row.names = FALSE)
                 write.csv(V4DATOFIN_pure_postpago, paste(path_destino,"/V4DATOFIN_pure_postpago_final.csv",sep = ""), row.names = FALSE)
                 write.csv(V4DATOFIN_pure_control, paste(path_destino,"/V4DATOFIN_pure_control_final.csv",sep = ""), row.names = FALSE)
               }
               
               
               
             },
             
             O = {
               #Caso 2: Se ingresó la trama opcion = "batch"    
               #Muestra en consola
               #print(V4DATOFIN_pure_prepago)
               if(flag_trama_prepago){
                 trama_transformada <- do.call(paste, c(V4DATOFIN_pure_prepago, sep = token))
               }else if(flag_trama_postpago){
                 trama_transformada <- do.call(paste, c(V4DATOFIN_pure_postpago, sep = token))
               }else if(flag_trama_control){
                 trama_transformada <- do.call(paste, c(V4DATOFIN_pure_control, sep = token))
               }
               
               
               return(trama_transformada)
             },
             
             {
               stop("OPCION INVÁLIDA")
               
             })   
      
      
    },error=function(cond) {
      message <- as.character(cond)
      return(message)
    }#, warning=function(cond) {
    #  message <- as.character(cond)
    #  return(paste(message," xd"))
    #}
    
    
  ) 
  
  return(out)
  
}