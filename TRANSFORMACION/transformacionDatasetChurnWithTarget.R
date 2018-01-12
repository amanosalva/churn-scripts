  #path_origen: Lugar donde se encuentra el dataset a transformar:   .../.../.../... .csv
  #path_destino: Lugar donde se exportará el/los dataset(s) (no posee nombre de archivo): .../.../.../    
  
  transformacionDatasetChurnWithTarget <- function(path_origen, path_destino, trama, token, opcion){
    
    out <- tryCatch(
      {  
        #Constantes
        VECTOR_PRODUCTOS <- c("prepago", "postpago", "control")
        EXTENSION_ARCHIVO_DESTINO <- ".csv"
        NOMBRE_DATASET <- "BCCA_TDP_DATA_TRANS_"
        
        #Definición del OUTPUT
        lista_output <- list(NULL,NULL,NULL)
        
        
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
                 BCCA_TDP_DATA_PURE <- read_csv(path_origen, 
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
                 productos <- levels(as.factor(BCCA_TDP_DATA_PURE$produc_c))
                 if(length(levels(as.factor(BCCA_TDP_DATA_PURE$produc_c))) == 1){
                   flagVariosNiveles <- FALSE
                   
                 }else{
                   flagVariosNiveles <- TRUE
                   
                 }
                 
                 
               },
               
               O = {
                 flag_trama_prepago <- FALSE
                 flag_trama_postpago <- FALSE
                 flag_trama_control <- FALSE
                 
                 #Función que identifica que producto es
                 identificaProducto <- function(trama,tokenp){
                   registro <- as.vector(do.call(rbind,strsplit(trama,tokenp)))
                   #El primer campo del vector es el producto
                   producto_online <- registro[1]
                   
                   
                   return(producto_online)
                 }
                 
                 producto_online <- identificaProducto(trama,token)
                 productos <- producto_online
                 
                 
                 if(producto_online %in% VECTOR_PRODUCTOS[1]){
                 #Función para recibir tramas asignándole los nombres específicos del dataset puro.
                 recibeTramaPrepago <- function(trama,tokenp){
                   registro <- as.data.frame(do.call(rbind,strsplit(trama,tokenp)))
                   names(registro) <-   c("produc_c"  , "edad_n"    , "antig_n"   , "eqanti_n"   ,"rec4to_2_n", "rec4fi_2_n","rec4pr_2_n", "cntsol_n"  , "rep1to_1_n"  ,
                                          "rep1fi_1_n", "rep1to_2_n", "rep1fi_2_n", "rep2to_1_n" ,"rep2fi_1_n", "rep2to_2_n","rep2fi_2_n", "rep3to_2_n", "rep3fi_2_n"    ,
                                          "fld041_n"  , "fld042_n"  , "fld045_n"  , "fld046_n"   ,"fld049_n"  , "fld050_n"  ,"fld053_n"  , "fld054_n"  , "fld057_n"      ,
                                          "fld058_n"  , "fld061_n"  , "fld062_n"  , "fld065_n"   ,"fld066_n"  , "fld069_n"  ,"fld070_n"  , "fld073_n"  , "fld074_n"      ,
                                          "pli_sn_c"  , "cant1m_n"  , "mont1m_n"  , "cant3m_n"   ,"mont3m_n"  , "cant6m_n"  ,"mont6m_n"  , "ctotal_n"  , "mtotal_n"      ,
                                          "rec1to_3_n", "rec3to_3_n", "rec4to_3_n", "rep1to_3_n" ,"rep2to_3_n", "rep3to_3_n","target_c"  )
                   #Asignación del tipo de dato. Convierte factor a numérico
                   registro[,  c("edad_n"    , "antig_n"   , "eqanti_n"   ,"rec4to_2_n", "rec4fi_2_n","rec4pr_2_n", "cntsol_n"  , "rep1to_1_n"  ,
                                 "rep1fi_1_n", "rep1to_2_n", "rep1fi_2_n", "rep2to_1_n" ,"rep2fi_1_n", "rep2to_2_n","rep2fi_2_n", "rep3to_2_n", "rep3fi_2_n"    ,
                                 "fld041_n"  , "fld042_n"  , "fld045_n"  , "fld046_n"   ,"fld049_n"  , "fld050_n"  ,"fld053_n"  , "fld054_n"  , "fld057_n"      ,
                                 "fld058_n"  , "fld061_n"  , "fld062_n"  , "fld065_n"   ,"fld066_n"  , "fld069_n"  ,"fld070_n"  , "fld073_n"  , "fld074_n"      ,
                                 "cant1m_n"  , "mont1m_n"  , "cant3m_n"   ,"mont3m_n"  , "cant6m_n"  ,"mont6m_n"  , "ctotal_n"  , "mtotal_n"      ,
                                 "rec1to_3_n", "rec3to_3_n", "rec4to_3_n", "rep1to_3_n" ,"rep2to_3_n", "rep3to_3_n")] <- as.numeric(as.character(unlist(registro[,  c("edad_n"    , "antig_n"   , "eqanti_n"   ,"rec4to_2_n", "rec4fi_2_n","rec4pr_2_n", "cntsol_n"  , "rep1to_1_n"  ,
                                                                                                                                                                      "rep1fi_1_n", "rep1to_2_n", "rep1fi_2_n", "rep2to_1_n" ,"rep2fi_1_n", "rep2to_2_n","rep2fi_2_n", "rep3to_2_n", "rep3fi_2_n"    ,
                                                                                                                                                                      "fld041_n"  , "fld042_n"  , "fld045_n"  , "fld046_n"   ,"fld049_n"  , "fld050_n"  ,"fld053_n"  , "fld054_n"  , "fld057_n"      ,
                                                                                                                                                                      "fld058_n"  , "fld061_n"  , "fld062_n"  , "fld065_n"   ,"fld066_n"  , "fld069_n"  ,"fld070_n"  , "fld073_n"  , "fld074_n"      ,
                                                                                                                                                                      "cant1m_n"  , "mont1m_n"  , "cant3m_n"   ,"mont3m_n"  , "cant6m_n"  ,"mont6m_n"  , "ctotal_n"  , "mtotal_n"      ,
                                                                                                                                                                      "rec1to_3_n", "rec3to_3_n", "rec4to_3_n", "rep1to_3_n" ,"rep2to_3_n", "rep3to_3_n")])))
                   #Asignación del tipo de dato. Convierte factor a character
                   registro[,c("produc_c" , "pli_sn_c", "target_c"  )] <- as.character(unlist(registro[,c("produc_c", "pli_sn_c", "target_c"  )]))
                   
                   return(registro)
                 }
                 BCCA_TDP_DATA_PURE <- recibeTramaPrepago(trama,token)
                 
                 }else if(producto_online %in% VECTOR_PRODUCTOS[2]){
                 
                 recibeTramaPostpago <- function(trama,tokenp){
                   registro <- as.data.frame(do.call(rbind,strsplit(trama,tokenp)))
                   names(registro) <- c("produc_c"  , "edad_n"     ,"antig_n"   , "eqanti_n"   ,"linper_c"  , "rec1to_n"  , "rec1fi_n"  , "rec1pe_n"  , "rec1to_1_n" ,
                                        "rec1fi_1_n", "rec1pr_1_n" ,"rec1to_2_n", "rec1fi_2_n" ,"rec1pr_2_n", "costpl_n"  , "conren_c"  , "cnttoc_n"  , "cntnrc_n"   ,
                                        "cnttmc_n"  , "cntctd_n"   ,"cntctl_n"  , "cntctl_1_n" ,"cntctl_2_n", "cntsol_n"                , "rep1to_n"  , "rep1fi_n"   ,
                                        "rep1to_1_n", "rep1fi_1_n" ,"rep1to_2_n", "rep1fi_2_n" ,"rep2to_n"  , "rep2fi_n"  , "rep3to_2_n", "rep3fi_2_n", "rep4to_n"   ,
                                        "rep4fi_n"  , "rep4to_2_n" ,"rep4fi_2_n", "fld041_n"   ,"fld042_n"  , "fld045_n"  , "fld046_n"  , "fld049_n"  , "fld050_n"   ,
                                        "fld053_n"  , "fld054_n"   ,"fld057_n"  , "fld058_n"   ,"fld061_n"  , "fld062_n"  , "fld065_n"  , "fld066_n"  , "fld069_n"   ,
                                        "fld070_n"  , "fld073_n"   ,"fld074_n"  , "fld115_n"   ,"fld116_n"  , "fld117_n"  , "fld119_c"  , "fld120_c"  , "fld121_c"   ,
                                        "fld122_c"  , "fld138_n"   ,"fld139_n"  , "fld140_n"   ,"fld141_n"  , "fld142_n"  , "fld143_n"  , "fld144_n"  , "fld145_n"   ,
                                        "fld146_n"  , "rec1to_3_n" ,"rep1to_3_n", "rep2to_3_n" ,"rep3to_3_n", "rep4to_3_n", "target_c" )
                   
                   #Asignación del tipo de dato. Convierte factor a numérico
                   registro[,c("edad_n"  ,"antig_n"   , "eqanti_n" , "rec1to_n"  , "rec1fi_n"  , "rec1pe_n"  , "rec1to_1_n" ,
                               "rec1fi_1_n", "rec1pr_1_n" ,"rec1to_2_n", "rec1fi_2_n" ,"rec1pr_2_n", "costpl_n"  , "cnttoc_n"  , "cntnrc_n"   ,
                               "cnttmc_n"  , "cntctd_n"   ,"cntctl_n"  , "cntctl_1_n" ,"cntctl_2_n", "cntsol_n"                , "rep1to_n"  , "rep1fi_n"   ,
                               "rep1to_1_n", "rep1fi_1_n" ,"rep1to_2_n", "rep1fi_2_n" ,"rep2to_n"  , "rep2fi_n"  , "rep3to_2_n", "rep3fi_2_n", "rep4to_n"   ,
                               "rep4fi_n"  , "rep4to_2_n" ,"rep4fi_2_n", "fld041_n"   ,"fld042_n"  , "fld045_n"  , "fld046_n"  , "fld049_n"  , "fld050_n"   ,
                               "fld053_n"  , "fld054_n"   ,"fld057_n"  , "fld058_n"   ,"fld061_n"  , "fld062_n"  , "fld065_n"  , "fld066_n"  , "fld069_n"   ,
                               "fld070_n"  , "fld073_n"   ,"fld074_n"  , "fld115_n"   ,"fld116_n"  , "fld117_n"  , 
                               "fld138_n"   ,"fld139_n"  , "fld140_n"   ,"fld141_n"  , "fld142_n"  , "fld143_n"  , "fld144_n"  , "fld145_n"   ,
                               "fld146_n"  , "rec1to_3_n" ,"rep1to_3_n", "rep2to_3_n" ,"rep3to_3_n", "rep4to_3_n" )] <- as.numeric(as.character(unlist(registro[,c("edad_n"  ,"antig_n"   , "eqanti_n" , "rec1to_n"  , "rec1fi_n"  , "rec1pe_n"  , "rec1to_1_n" ,
                                                                                                                                                                   "rec1fi_1_n", "rec1pr_1_n" ,"rec1to_2_n", "rec1fi_2_n" ,"rec1pr_2_n", "costpl_n"  , "cnttoc_n"  , "cntnrc_n"   ,
                                                                                                                                                                   "cnttmc_n"  , "cntctd_n"   ,"cntctl_n"  , "cntctl_1_n" ,"cntctl_2_n", "cntsol_n"  , "rep1to_n"  , "rep1fi_n"   ,
                                                                                                                                                                   "rep1to_1_n", "rep1fi_1_n" ,"rep1to_2_n", "rep1fi_2_n" ,"rep2to_n"  , "rep2fi_n"  , "rep3to_2_n", "rep3fi_2_n", "rep4to_n"   ,
                                                                                                                                                                   "rep4fi_n"  , "rep4to_2_n" ,"rep4fi_2_n", "fld041_n"   ,"fld042_n"  , "fld045_n"  , "fld046_n"  , "fld049_n"  , "fld050_n"   ,
                                                                                                                                                                   "fld053_n"  , "fld054_n"   ,"fld057_n"  , "fld058_n"   ,"fld061_n"  , "fld062_n"  , "fld065_n"  , "fld066_n"  , "fld069_n"   ,
                                                                                                                                                                   "fld070_n"  , "fld073_n"   ,"fld074_n"  , "fld115_n"   ,"fld116_n"  , "fld117_n"  , 
                                                                                                                                                                   "fld138_n"   ,"fld139_n"  , "fld140_n"   ,"fld141_n"  , "fld142_n"  , "fld143_n"  , "fld144_n"  , "fld145_n"   ,
                                                                                                                                                                   "fld146_n"  , "rec1to_3_n" ,"rep1to_3_n", "rep2to_3_n" ,"rep3to_3_n", "rep4to_3_n" )])))
                   #Asignación del tipo de dato. Convierte factor a character
                   registro[,c("produc_c" , "linper_c"   ,"conren_c"  , "fld119_c"  , "fld120_c"  , "fld121_c"   ,
                               "fld122_c" , "target_c" )] <- as.character(unlist(registro[,c("produc_c" , "linper_c"   ,"conren_c"  , "fld119_c"  , "fld120_c"  , "fld121_c"   ,
                                                                                             "fld122_c" , "target_c" )]))
                   
                   return(registro)
                 }
                 BCCA_TDP_DATA_PURE <- recibeTramaPostpago(trama,token)
                 }else if(producto_online %in% VECTOR_PRODUCTOS[3]){
                 
                 
                 recibeTramaControl <- function(trama,tokenp){
                   registro <- as.data.frame(do.call(rbind,strsplit(trama,tokenp)))
                   names(registro) <- c("produc_c"   ,"edad_n"    , "antig_n"   , "eqanti_n"  , "linper_c"  , "rec1to_n"   ,"rec1fi_n"  , "rec1pr_n"  , "rec1pe_n"   ,
                                        "rec1to_2_n" ,"rec1fi_2_n", "rec1pr_2_n", "rec3to_n"  , "costpl_n"  , "conren_c"   ,"cnttoc_n"  , "cntnrc_n"  , "cnttmc_n"   ,
                                        "cntctd_n"   ,"cntctl_n"  , "cntctl_1_n", "cntctl_2_n", "cntsol_n"  ,"rep1to_n"  , "rep1fi_n"  , "rep1to_1_n" ,
                                        "rep1fi_1_n" ,"rep1to_2_n", "rep1fi_2_n", "rep2to_n"  , "rep2fi_n"  , "rep2to_1_n" ,"rep2fi_1_n", "rep2to_2_n", "rep2fi_2_n" ,
                                        "rep3to_2_n" ,"rep3fi_2_n", "rep4to_n"  , "rep4fi_n"  , "rep4to_2_n", "rep4fi_2_n" ,"fld041_n"  , "fld042_n"  , "fld045_n"   ,
                                        "fld046_n"   ,"fld049_n"  , "fld050_n"  , "fld053_n"  , "fld054_n"  , "fld057_n"   ,"fld058_n"  , "fld061_n"  , "fld062_n"   ,
                                        "fld065_n"   ,"fld066_n"  , "fld069_n"  , "fld070_n"  , "fld073_n"  , "fld074_n"   ,"fld115_n"  , "fld116_n"  , "fld117_n"   ,
                                        "fld119_c"   ,"fld120_c"  , "fld121_c"  , "fld122_c"  , "fld138_n"  , "fld139_n"   ,"fld140_n"  , "fld141_n"  , "fld142_n"   ,
                                        "fld143_n"   ,"fld144_n"  , "fld145_n"  , "fld146_n"  , "rec1to_3_n", "rec2to_3_n" ,"rep1to_3_n", "rep2to_3_n", "rep3to_3_n" ,
                                        "rep4to_3_n" ,"target_c" )
                   
                   #Asignación del tipo de dato. Convierte factor a numérico
                   registro[,c("edad_n"    , "antig_n"   , "eqanti_n"  , "rec1to_n"   ,"rec1fi_n"  , "rec1pr_n"  , "rec1pe_n"   ,
                               "rec1to_2_n" ,"rec1fi_2_n", "rec1pr_2_n", "rec3to_n"  , "costpl_n"  ,"cnttoc_n"  , "cntnrc_n"  , "cnttmc_n"   ,
                               "cntctd_n"   ,"cntctl_n"  , "cntctl_1_n", "cntctl_2_n", "cntsol_n"  ,"rep1to_n"  , "rep1fi_n"  , "rep1to_1_n" ,
                               "rep1fi_1_n" ,"rep1to_2_n", "rep1fi_2_n", "rep2to_n"  , "rep2fi_n"  , "rep2to_1_n" ,"rep2fi_1_n", "rep2to_2_n", "rep2fi_2_n" ,
                               "rep3to_2_n" ,"rep3fi_2_n", "rep4to_n"  , "rep4fi_n"  , "rep4to_2_n", "rep4fi_2_n" ,"fld041_n"  , "fld042_n"  , "fld045_n"   ,
                               "fld046_n"   ,"fld049_n"  , "fld050_n"  , "fld053_n"  , "fld054_n"  , "fld057_n"   ,"fld058_n"  , "fld061_n"  , "fld062_n"   ,
                               "fld065_n"   ,"fld066_n"  , "fld069_n"  , "fld070_n"  , "fld073_n"  , "fld074_n"   ,"fld115_n"  , "fld116_n"  , "fld117_n"   ,
                               "fld138_n"  , "fld139_n"   ,"fld140_n"  , "fld141_n"  , "fld142_n"   ,
                               "fld143_n"   ,"fld144_n"  , "fld145_n"  , "fld146_n"  , "rec1to_3_n", "rec2to_3_n" ,"rep1to_3_n", "rep2to_3_n", "rep3to_3_n" ,
                               "rep4to_3_n")] <- as.numeric(as.character(unlist(registro[,c("edad_n"    , "antig_n"   , "eqanti_n"  , "rec1to_n"   ,"rec1fi_n"  , "rec1pr_n"  , "rec1pe_n"   ,
                                                                                            "rec1to_2_n" ,"rec1fi_2_n", "rec1pr_2_n", "rec3to_n"  , "costpl_n"  ,"cnttoc_n"  , "cntnrc_n"  , "cnttmc_n"   ,
                                                                                            "cntctd_n"   ,"cntctl_n"  , "cntctl_1_n", "cntctl_2_n", "cntsol_n"  ,"rep1to_n"  , "rep1fi_n"  , "rep1to_1_n" ,
                                                                                            "rep1fi_1_n" ,"rep1to_2_n", "rep1fi_2_n", "rep2to_n"  , "rep2fi_n"  , "rep2to_1_n" ,"rep2fi_1_n", "rep2to_2_n", "rep2fi_2_n" ,
                                                                                            "rep3to_2_n" ,"rep3fi_2_n", "rep4to_n"  , "rep4fi_n"  , "rep4to_2_n", "rep4fi_2_n" ,"fld041_n"  , "fld042_n"  , "fld045_n"   ,
                                                                                            "fld046_n"   ,"fld049_n"  , "fld050_n"  , "fld053_n"  , "fld054_n"  , "fld057_n"   ,"fld058_n"  , "fld061_n"  , "fld062_n"   ,
                                                                                            "fld065_n"   ,"fld066_n"  , "fld069_n"  , "fld070_n"  , "fld073_n"  , "fld074_n"   ,"fld115_n"  , "fld116_n"  , "fld117_n"   ,
                                                                                            "fld138_n"  , "fld139_n"   ,"fld140_n"  , "fld141_n"  , "fld142_n"   ,
                                                                                            "fld143_n"   ,"fld144_n"  , "fld145_n"  , "fld146_n"  , "rec1to_3_n", "rec2to_3_n" ,"rep1to_3_n", "rep2to_3_n", "rep3to_3_n" ,
                                                                                            "rep4to_3_n")])))
                   #Asignación del tipo de dato. Convierte factor a character
                   registro[,c("produc_c" , "linper_c"  , "conren_c", "fld119_c"   ,"fld120_c"  , "fld121_c"  , "fld122_c"  ,"target_c" )] <- as.character(unlist(registro[,c("produc_c" , "linper_c"  , "conren_c", "fld119_c"   ,"fld120_c"  , "fld121_c"  , "fld122_c"  ,"target_c" )]))
                   
                   return(registro)
                 }
                 BCCA_TDP_DATA_PURE <- recibeTramaControl(trama,token)
                 }else{
                   stop("EL PRODUCTO INGRESADO EN LA TRAMA MEDIANTE LA MODALIDAD ONLINE ES INVÁLIDO")
                 }
                 
                 
                 
                 
                 },
               
               {
                 stop("OPCION INVÁLIDA")
                 
               })                                                 
        
        
        #Separando dataset PREPAGO
        if(VECTOR_PRODUCTOS[1] %in% productos){
          BCCA_TDP_DATA_PURE_PREPAGO<- subset(BCCA_TDP_DATA_PURE, BCCA_TDP_DATA_PURE$produc_c == VECTOR_PRODUCTOS[1])
          BCCA_TDP_DATA_PURE_PREPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_PREPAGO, c("produc_c"))
          producto_trama <- VECTOR_PRODUCTOS[1]
          flag_trama_prepago <- TRUE
          flag_trama_postpago <- FALSE
          flag_trama_control <- FALSE
          print("TRANSFORMANDO DATA PREPAGO")
          
          #edad_n
          edad_n <- BCCA_TDP_DATA_PURE_PREPAGO$edad_n
          BCCA_TDP_DATA_PURE_PREPAGO$edad_n <- ifelse(edad_n > 70, round(mean(edad_n)), edad_n)
          rm(edad_n)
          
          
          #cntsol_n
          cntsol_n <- BCCA_TDP_DATA_PURE_PREPAGO$cntsol_n
          BCCA_TDP_DATA_PURE_PREPAGO$cntsol_n <- ifelse(cntsol_n > 0, "si", "no")
          rm(cntsol_n)
          
          #fld041_n
          fld041_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld041_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld041_n <- ifelse(fld041_n > 0, "si", "no")
          rm(fld041_n)
          
          #fld042_n
          fld042_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld042_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld042_n <- ifelse(fld042_n > 0, "si", "no")
          rm(fld042_n)
          
          #fld045_n
          fld045_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld045_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld045_n <- ifelse(fld045_n > 0, "si", "no")
          rm(fld045_n)
          
          #fld046_n
          fld046_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld046_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld046_n <- ifelse(fld046_n > 0, "si", "no")
          rm(fld046_n)
          
          #fld049_n
          fld049_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld049_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld049_n <- ifelse(fld049_n > 0, "si", "no")
          rm(fld049_n)
          
          #fld050_n
          fld050_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld050_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld050_n <- ifelse(fld050_n > 0, "si", "no")
          rm(fld050_n)
          
          #fld053_n
          fld053_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld053_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld053_n <- ifelse(fld053_n == 0, "[0]", 
                                                    ifelse(fld053_n > 0 & fld053_n <= 50, "(0-50]", 
                                                           ifelse(fld053_n > 50 & fld053_n <= 100, "(50-100]", 
                                                                  ifelse(fld053_n > 100 & fld053_n <= 300, "(100-300]", 
                                                                         ifelse(fld053_n > 300, "(300-inf)", fld053_n)))))
          rm(fld053_n)
          
          #fld061_n
          fld061_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld061_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld061_n <- ifelse(fld061_n == 0, "[0]", 
                                                        ifelse(fld061_n > 0 & fld061_n <= 50, "(0-50]", 
                                                               ifelse(fld061_n > 50 & fld061_n <= 100, "(50-100]", 
                                                                      ifelse(fld061_n > 100 & fld061_n <= 300, "(100-300]", 
                                                                             ifelse(fld061_n > 300, "(300-inf)", fld061_n)))))
          rm(fld061_n)
          
          
          #fld054_n
          fld054_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld054_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld054_n <- ifelse(fld054_n == 0, "[0]", 
                                                    ifelse(fld054_n > 0 & fld054_n <= 5, "(0-5]", 
                                                           ifelse(fld054_n > 5 & fld054_n <= 10, "(5-10]", 
                                                                  ifelse(fld054_n > 10 & fld054_n <= 15, "(10-15]",
                                                                         ifelse(fld054_n > 15 & fld054_n <= 40, "(15-40]",
                                                                                ifelse(fld054_n > 40, "(40-inf)", fld054_n))))))
          
          rm(fld054_n)
          
          #fld062_n
          fld062_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld062_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld062_n <- ifelse(fld062_n == 0, "[0]", 
                                                        ifelse(fld062_n > 0 & fld062_n <= 5, "(0-5]", 
                                                               ifelse(fld062_n > 5 & fld062_n <= 10, "(5-10]", 
                                                                      ifelse(fld062_n > 10 & fld062_n <= 15, "(10-15]",
                                                                             ifelse(fld062_n > 15 & fld062_n <= 40, "(15-40]",
                                                                                    ifelse(fld062_n > 40, "(40-inf)", fld062_n))))))
          
          rm(fld062_n)
          
          
          #fld057_n
          fld057_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld057_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld057_n <- ifelse(fld057_n == 0, "[0]", 
                                                    ifelse(fld057_n > 0 & fld057_n <= 50, "(0-50]", 
                                                           ifelse(fld057_n > 50 & fld057_n <= 100, "(50-100]", 
                                                                  ifelse(fld057_n > 100 & fld057_n <= 300, "(100-300]", 
                                                                         ifelse(fld057_n > 300, "(300-inf)", fld057_n)))))
          rm(fld057_n)
          
          #fld058_n
          fld058_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld058_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld058_n <- ifelse(fld058_n == 0, "[0]", 
                                                         ifelse(fld058_n > 0 & fld058_n <= 5, "(0-5]", 
                                                                ifelse(fld058_n > 5 & fld058_n <= 10, "(5-10]", 
                                                                       ifelse(fld058_n > 10 & fld058_n <= 15, "(10-15]",
                                                                              ifelse(fld058_n > 15 & fld058_n <= 40, "(15-40]",
                                                                                     ifelse(fld058_n > 40, "(40-inf)", fld058_n))))))
          
          rm(fld058_n)
          
          
          #fld065_n
          fld065_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld065_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld065_n <- ifelse(fld065_n > 0, "si", "no")
          rm(fld065_n)
          
          
          #fld066_n
          fld066_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld066_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld066_n <- ifelse(fld066_n > 0, "si", "no")
          rm(fld066_n)
          
          #fld069_n
          fld069_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld069_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld069_n <- ifelse(fld069_n == 0, "[0]", 
                                                    ifelse(fld069_n > 0 & fld069_n <= 50, "(0-50]", 
                                                           ifelse(fld069_n > 50 & fld069_n <= 150, "(50-150]",
                                                                  ifelse(fld069_n > 150 & fld069_n <= 300, "(150-300]", 
                                                                         ifelse(fld069_n > 300 & fld069_n <= 700, "(300-700]", 
                                                                                ifelse(fld069_n > 700, "(700-inf)", fld069_n))))))
          rm(fld069_n)
          
          #fld070_n
          fld070_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld070_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld070_n <- ifelse(fld070_n == 0, "[0]", 
                                                    ifelse(fld070_n > 0 & fld070_n <= 25, "(0-25]",
                                                           ifelse(fld070_n > 25 & fld070_n <= 100, "(25-100]", 
                                                                  ifelse(fld070_n > 100, "(100-inf)", fld070_n))))
          rm(fld070_n)
          
          #fld073_n
          fld073_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld073_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld073_n <- ifelse(fld073_n > 0, "si", "no")
          rm(fld073_n)
          
          #fld074_n
          fld074_n <- BCCA_TDP_DATA_PURE_PREPAGO$fld074_n
          BCCA_TDP_DATA_PURE_PREPAGO$fld074_n <- ifelse(fld074_n > 0, "si", "no")
          rm(fld074_n)
          
          #cant1m_n
          cant1m_n <- BCCA_TDP_DATA_PURE_PREPAGO$cant1m_n
          BCCA_TDP_DATA_PURE_PREPAGO$cant1m_n <- ifelse(cant1m_n == 0, "[0]", 
                                                    ifelse(cant1m_n > 0 & cant1m_n <= 2, "(0-2]", 
                                                           ifelse(cant1m_n > 2 & cant1m_n <= 5, "(2-5]", 
                                                                  ifelse(cant1m_n > 5, "(5-inf)", cant1m_n))))
          rm(cant1m_n)
          
          #mont1m_n 
          mont1m_n <- BCCA_TDP_DATA_PURE_PREPAGO$mont1m_n
          BCCA_TDP_DATA_PURE_PREPAGO$mont1m_n <- ifelse(mont1m_n == 0, "[0]",
                                                    ifelse(mont1m_n > 0 & mont1m_n <= 3, "(0-3]", 
                                                           ifelse(mont1m_n > 3 & mont1m_n <= 5, "(3-5]", 
                                                                  ifelse(mont1m_n > 5 & mont1m_n <= 10, "(5-10]", 
                                                                         ifelse(mont1m_n > 10 & mont1m_n <= 20, "(10-20]", 
                                                                                ifelse(mont1m_n > 20, "(20-inf)", mont1m_n))))))
          rm(mont1m_n)
          
          
          #cant3m_n
          cant3m_n <- BCCA_TDP_DATA_PURE_PREPAGO$cant3m_n
          BCCA_TDP_DATA_PURE_PREPAGO$cant3m_n <- ifelse(cant3m_n == 0, "[0]", 
                                                        ifelse(cant3m_n > 0 & cant3m_n <= 2, "(0-2]", 
                                                               ifelse(cant3m_n > 2 & cant3m_n <= 5, "(2-5]", 
                                                                      ifelse(cant3m_n > 5, "(5-inf)", cant3m_n))))
          rm(cant3m_n)
          
          #mont3m_n 
          mont3m_n <- BCCA_TDP_DATA_PURE_PREPAGO$mont3m_n
          BCCA_TDP_DATA_PURE_PREPAGO$mont3m_n <- ifelse(mont3m_n == 0, "[0]",
                                                        ifelse(mont3m_n > 0 & mont3m_n <= 3, "(0-3]", 
                                                               ifelse(mont3m_n > 3 & mont3m_n <= 5, "(3-5]", 
                                                                      ifelse(mont3m_n > 5 & mont3m_n <= 10, "(5-10]", 
                                                                             ifelse(mont3m_n > 10 & mont3m_n <= 20, "(10-20]", 
                                                                                    ifelse(mont3m_n > 20, "(20-inf)", mont3m_n))))))
          rm(mont3m_n)
          
          #cant6m_n
          cant6m_n <- BCCA_TDP_DATA_PURE_PREPAGO$cant6m_n
          BCCA_TDP_DATA_PURE_PREPAGO$cant6m_n <- ifelse(cant6m_n == 0, "[0]", 
                                                        ifelse(cant6m_n > 0 & cant6m_n <= 2, "(0-2]", 
                                                               ifelse(cant6m_n > 2 & cant6m_n <= 5, "(2-5]", 
                                                                      ifelse(cant6m_n > 5, "(5-inf)", cant6m_n))))
          rm(cant6m_n)
          
          #mont6m_n 
          mont6m_n <- BCCA_TDP_DATA_PURE_PREPAGO$mont6m_n
          BCCA_TDP_DATA_PURE_PREPAGO$mont6m_n <- ifelse(mont6m_n == 0, "[0]",
                                                        ifelse(mont6m_n > 0 & mont6m_n <= 3, "(0-3]", 
                                                               ifelse(mont6m_n > 3 & mont6m_n <= 5, "(3-5]", 
                                                                      ifelse(mont6m_n > 5 & mont6m_n <= 10, "(5-10]", 
                                                                             ifelse(mont6m_n > 10 & mont6m_n <= 20, "(10-20]", 
                                                                                    ifelse(mont6m_n > 20, "(20-inf)", mont6m_n))))))
          rm(mont6m_n)
          
          
          
          
          
          
          
          #ctotal_n
          ctotal_n <- BCCA_TDP_DATA_PURE_PREPAGO$ctotal_n
          BCCA_TDP_DATA_PURE_PREPAGO$ctotal_n <- ifelse(ctotal_n == 0, "[0]", 
                                                    ifelse(ctotal_n > 0 & ctotal_n <= 10, "(0-10]", 
                                                           ifelse(ctotal_n > 10 & ctotal_n <= 20, "(10-20]", 
                                                                  ifelse(ctotal_n > 20 & ctotal_n <= 30, "(20-30]", 
                                                                         ifelse(ctotal_n > 30 & ctotal_n <= 40, "(30-40]", 
                                                                                ifelse(ctotal_n > 40, "(40-inf)", ctotal_n))))))
          
          
          rm(ctotal_n)
          
          #mtotal_n
          mtotal_n <- BCCA_TDP_DATA_PURE_PREPAGO$mtotal_n
          BCCA_TDP_DATA_PURE_PREPAGO$mtotal_n <- ifelse(mtotal_n == 0, "[0]",
                                                    ifelse(mtotal_n > 0 & mtotal_n <= 18, "(0-18]", 
                                                           ifelse(mtotal_n > 18 & mtotal_n <= 30, "(18-30]",
                                                                  ifelse(mtotal_n > 30 & mtotal_n <= 60, "(30-60]",
                                                                         ifelse(mtotal_n > 60 & mtotal_n <= 120, "(60-120]",
                                                                                ifelse(mtotal_n > 120, "(120-inf)", mtotal_n))))))
          
          
          rm(mtotal_n)
          
  
          #Asignando nuevos nombres
          names(BCCA_TDP_DATA_PURE_PREPAGO) <-  c("edad_n"    , "antig_n"   , "eqanti_n"   ,"rec4to_2_n", "rec4fi_2_n","rec4pr_2_n", "cntsol_c"  , "rep1to_1_n"    ,
                                                  "rep1fi_1_n", "rep1to_2_n", "rep1fi_2_n", "rep2to_1_n" ,"rep2fi_1_n", "rep2to_2_n","rep2fi_2_n", "rep3to_2_n", "rep3fi_2_n"    ,
                                                  "fld041_c"  , "fld042_c"  , "fld045_c"  , "fld046_c"   ,"fld049_c"  , "fld050_c"  ,"fld053_c"  , "fld054_c"  , "fld057_c"      ,
                                                  "fld058_c"  , "fld061_c"  , "fld062_c"  , "fld065_c"   ,"fld066_c"  , "fld069_c"  ,"fld070_c"  , "fld073_c"  , "fld074_c"      ,
                                                  "pli_sn_c"  , "cant1m_c"  , "mont1m_c"  , "cant3m_c"   ,"mont3m_c"  , "cant6m_c"  ,"mont6m_c"  , "ctotal_c"  , "mtotal_c"      ,
                                                  "rec1to_3_n", "rec3to_3_n", "rec4to_3_n", "rep1to_3_n" ,"rep2to_3_n", "rep3to_3_n","target_c"  )
          
        }
        
        #Separando dataset POSTPAGO
        if(VECTOR_PRODUCTOS[2] %in% productos){
          producto_trama <- VECTOR_PRODUCTOS[2]
          BCCA_TDP_DATA_PURE_POSTPAGO<- subset(BCCA_TDP_DATA_PURE, BCCA_TDP_DATA_PURE$produc_c == "postpago")
          BCCA_TDP_DATA_PURE_POSTPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_POSTPAGO, c("produc_c"))
          flag_trama_prepago <- FALSE
          flag_trama_postpago <- TRUE
          flag_trama_control <- FALSE
          print("TRANSFORMANDO DATA POSTPAGO")
          
          #edad_n
          edad_n <- BCCA_TDP_DATA_PURE_POSTPAGO$edad_n
          BCCA_TDP_DATA_PURE_POSTPAGO$edad_n <- ifelse(edad_n > 70, round(mean(edad_n)), edad_n)
          rm(edad_n)
  
          #cntsol_n
          cntsol_n <- BCCA_TDP_DATA_PURE_POSTPAGO$cntsol_n
          BCCA_TDP_DATA_PURE_POSTPAGO$cntsol_n <- ifelse(cntsol_n > 0, "si", "no")
          rm(cntsol_n)
          
          
          #fld041_n
          fld041_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld041_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld041_n <- ifelse(fld041_n > 0, "si", "no")
          rm(fld041_n)
          
          #fld042_n
          fld042_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld042_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld042_n <- ifelse(fld042_n > 0, "si", "no")
          rm(fld042_n)
          
          #fld045_n
          fld045_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld045_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld045_n <- ifelse(fld045_n > 0, "si", "no")
          rm(fld045_n)
          
          #fld046_n
          fld046_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld046_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld046_n <- ifelse(fld046_n > 0, "si", "no")
          rm(fld046_n)
          
          #fld049_n
          fld049_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld049_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld049_n <- ifelse(fld049_n > 0, "si", "no")
          rm(fld049_n)
  
          #fld050_n
          fld050_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld050_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld050_n <- ifelse(fld050_n > 0, "si", "no")
          rm(fld050_n)
          
          #fld054_n
          fld054_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld054_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld054_n <- ifelse(fld054_n == 0, "[0]", 
                                                     ifelse(fld054_n > 0 & fld054_n <= 5, "(0-5]", 
                                                            ifelse(fld054_n > 5 & fld054_n <= 10, "(5-10]", 
                                                                   ifelse(fld054_n > 10 & fld054_n <= 15, "(10-15]",
                                                                          ifelse(fld054_n > 15 & fld054_n <= 40, "(15-40]",
                                                                                 ifelse(fld054_n > 40, "(40-inf)", fld054_n))))))
          
          rm(fld054_n)
          
          #fld057_n
          fld057_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld057_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld057_n <- ifelse(fld057_n == 0, "[0]", 
                                                        ifelse(fld057_n > 0 & fld057_n <= 50, "(0-50]", 
                                                               ifelse(fld057_n > 50 & fld057_n <= 100, "(50-100]", 
                                                                      ifelse(fld057_n > 100 & fld057_n <= 300, "(100-300]", 
                                                                             ifelse(fld057_n > 300, "(300-inf)", fld057_n)))))
          rm(fld057_n)
          
          #fld058_n
          fld058_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld058_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld058_n <- ifelse(fld058_n == 0, "[0]", 
                                                     ifelse(fld058_n > 0 & fld058_n <= 5, "(0-5]", 
                                                            ifelse(fld058_n > 5 & fld058_n <= 10, "(5-10]", 
                                                                   ifelse(fld058_n > 10 & fld058_n <= 15, "(10-15]",
                                                                          ifelse(fld058_n > 15 & fld058_n <= 40, "(15-40]",
                                                                                 ifelse(fld058_n > 40, "(40-inf)", fld058_n))))))
          
          rm(fld058_n)
          
          #fld061_n
          fld061_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld061_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld061_n <- ifelse(fld061_n == 0, "[0]", 
                                                        ifelse(fld061_n > 0 & fld061_n <= 50, "(0-50]", 
                                                               ifelse(fld061_n > 50 & fld061_n <= 100, "(50-100]", 
                                                                      ifelse(fld061_n > 100 & fld061_n <= 300, "(100-300]", 
                                                                             ifelse(fld061_n > 300, "(300-inf)", fld061_n)))))
          rm(fld061_n)
          
          #fld062_n
          fld062_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld062_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld062_n <- ifelse(fld062_n == 0, "[0]", 
                                                     ifelse(fld062_n > 0 & fld062_n <= 5, "(0-5]", 
                                                            ifelse(fld062_n > 5 & fld062_n <= 10, "(5-10]", 
                                                                   ifelse(fld062_n > 10 & fld062_n <= 15, "(10-15]",
                                                                          ifelse(fld062_n > 15 & fld062_n <= 40, "(15-40]",
                                                                                 ifelse(fld062_n > 40, "(40-inf)", fld062_n))))))
          rm(fld062_n)
          
          #fld065_n
          fld065_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld065_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld065_n <- ifelse(fld065_n > 0, "si", "no")
          rm(fld065_n)
          
          #fld066_n
          fld066_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld066_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld066_n <- ifelse(fld066_n > 0, "si", "no")
          rm(fld066_n)
          
          #fld069_n
          fld069_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld069_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld069_n <- ifelse(fld069_n == 0, "[0]", 
                                                        ifelse(fld069_n > 0 & fld069_n <= 50, "(0-50]", 
                                                               ifelse(fld069_n > 50 & fld069_n <= 150, "(50-150]",
                                                                      ifelse(fld069_n > 150 & fld069_n <= 300, "(150-300]", 
                                                                             ifelse(fld069_n > 300 & fld069_n <= 700, "(300-700]", 
                                                                                    ifelse(fld069_n > 700, "(700-inf)", fld069_n))))))
          rm(fld069_n)
          
          #fld070_n
          fld070_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld070_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld070_n <- ifelse(fld070_n == 0, "[0]", 
                                                     ifelse(fld070_n > 0 & fld070_n <= 25, "(0-25]",
                                                            ifelse(fld070_n > 25 & fld070_n <= 100, "(25-100]", 
                                                                   ifelse(fld070_n > 100, "(100-inf)", fld070_n))))
          rm(fld070_n)
          
          #fld073_n
          fld073_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld073_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld073_n <- ifelse(fld073_n > 0, "si", "no")
          rm(fld073_n)
          
          #fld074_n
          fld074_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld074_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld074_n <- ifelse(fld074_n > 0, "si", "no")
          rm(fld074_n)
          
          
          #fld115_n
          fld115_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld115_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld115_n <- ifelse(fld115_n == 0, "[0]", 
                                                     ifelse(fld115_n > 0 & fld115_n <= 4000, "limitado",
                                                            ifelse(fld115_n > 4000, "ilimitado", fld115_n)))
          rm(fld115_n)
          
          #fld116_n
          fld116_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld116_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld116_n <- ifelse(fld116_n == 0, "[0]", 
                                                     ifelse(fld116_n > 0 & fld116_n <= 20000000, "limitado",
                                                            ifelse(fld116_n > 20000000, "ilimitado", fld116_n)))
          rm(fld116_n)
          
          fld117_n <- BCCA_TDP_DATA_PURE_POSTPAGO$fld117_n
          BCCA_TDP_DATA_PURE_POSTPAGO$fld117_n <- ifelse(fld117_n == 0, "[0]", 
                                                        ifelse(fld117_n > 0 & fld117_n <= 50, "(0-50]",
                                                               ifelse(fld117_n > 50 & fld117_n <= 200, "(50-200]",
                                                                      ifelse(fld117_n > 200 & fld117_n <= 900, "(200-900]",
                                                                             ifelse(fld117_n > 900, ">900", fld117_n)))))
          rm(fld117_n)
  
          names(BCCA_TDP_DATA_PURE_POSTPAGO) <- c("edad_n"    ,"antig_n"   , "eqanti_n"   ,"linper_c"  , "rec1to_n"  , "rec1fi_n"  , "rec1pe_n"  , "rec1to_1_n" ,
                                                  "rec1fi_1_n", "rec1pr_1_n" ,"rec1to_2_n", "rec1fi_2_n" ,"rec1pr_2_n", "costpl_n"  , "conren_c"  , "cnttoc_n"  , "cntnrc_n"   ,
                                                  "cnttmc_n"  , "cntctd_n"   ,"cntctl_n"  , "cntctl_1_n" ,"cntctl_2_n", "cntsol_c"  , "rep1to_n"  , "rep1fi_n"   ,
                                                  "rep1to_1_n", "rep1fi_1_n" ,"rep1to_2_n", "rep1fi_2_n" ,"rep2to_n"  , "rep2fi_n"  , "rep3to_2_n", "rep3fi_2_n", "rep4to_n"   ,
                                                  "rep4fi_n"  , "rep4to_2_n" ,"rep4fi_2_n", "fld041_c"   ,"fld042_c"  , "fld045_c"  , "fld046_c"  , "fld049_c"  , "fld050_c"   ,
                                                  "fld053_c"  , "fld054_c"   ,"fld057_c"  , "fld058_c"   ,"fld061_c"  , "fld062_c"  , "fld065_c"  , "fld066_c"  , "fld069_c"   ,
                                                  "fld070_c"  , "fld073_c"   ,"fld074_c"  , "fld115_c"   ,"fld116_c"  , "fld117_c"  , "fld119_c"  , "fld120_c"  , "fld121_c"   ,
                                                  "fld122_c"  , "fld138_n"   ,"fld139_n"  , "fld140_n"   ,"fld141_n"  , "fld142_n"  , "fld143_n"  , "fld144_n"  , "fld145_n"   ,
                                                  "fld146_n"  , "rec1to_3_n" ,"rep1to_3_n", "rep2to_3_n" ,"rep3to_3_n", "rep4to_3_n", "target_c" )
          
          
        }
        
        if(VECTOR_PRODUCTOS[3] %in% productos){
          producto_trama <- VECTOR_PRODUCTOS[3]
          BCCA_TDP_DATA_PURE_CONTROL<- subset(BCCA_TDP_DATA_PURE, BCCA_TDP_DATA_PURE$produc_c == "control")
          BCCA_TDP_DATA_PURE_CONTROL <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_CONTROL, c("produc_c"))
          flag_trama_prepago <- FALSE
          flag_trama_postpago <- FALSE
          flag_trama_control <- TRUE
          print("TRANSFORMANDO DATA CONTROL")
          
          #edad_n
          edad_n <- BCCA_TDP_DATA_PURE_CONTROL$edad_n
          BCCA_TDP_DATA_PURE_CONTROL$edad_n <- ifelse(edad_n > 70, round(mean(edad_n)), edad_n)
          rm(edad_n)
          
          #cntctl_n
          cntctl_n <- BCCA_TDP_DATA_PURE_CONTROL$cntctl_n
          BCCA_TDP_DATA_PURE_CONTROL$cntctl_n <- ifelse(cntctl_n > 0, "si", "no")
          rm(cntctl_n)
          
          #cntctl_1_n
          cntctl_1_n <- BCCA_TDP_DATA_PURE_CONTROL$cntctl_1_n
          BCCA_TDP_DATA_PURE_CONTROL$cntctl_1_n <- ifelse(cntctl_1_n > 0, "si", "no")
          rm(cntctl_1_n)
          
          #cntctl_2_n
          cntctl_2_n <- BCCA_TDP_DATA_PURE_CONTROL$cntctl_2_n
          BCCA_TDP_DATA_PURE_CONTROL$cntctl_2_n <- ifelse(cntctl_2_n > 0, "si", "no")
          rm(cntctl_2_n)
          
          #cntsol_n
          cntsol_n <- BCCA_TDP_DATA_PURE_CONTROL$cntsol_n
          BCCA_TDP_DATA_PURE_CONTROL$cntsol_n <- ifelse(cntsol_n > 0, "si", "no")
          rm(cntsol_n)
          
          #fld041_n
          fld041_n <- BCCA_TDP_DATA_PURE_CONTROL$fld041_n
          BCCA_TDP_DATA_PURE_CONTROL$fld041_n <- ifelse(fld041_n > 0, "si", "no")
          rm(fld041_n)
          
          #fld042_n
          fld042_n <- BCCA_TDP_DATA_PURE_CONTROL$fld042_n
          BCCA_TDP_DATA_PURE_CONTROL$fld042_n <- ifelse(fld042_n > 0, "si", "no")
          rm(fld042_n)
          
          #fld045_n
          fld045_n <- BCCA_TDP_DATA_PURE_CONTROL$fld045_n
          BCCA_TDP_DATA_PURE_CONTROL$fld045_n <- ifelse(fld045_n > 0, "si", "no")
          rm(fld045_n)
          
          #fld046_n
          fld046_n <- BCCA_TDP_DATA_PURE_CONTROL$fld046_n
          BCCA_TDP_DATA_PURE_CONTROL$fld046_n <- ifelse(fld046_n > 0, "si", "no")
          rm(fld046_n)
          
          #fld049_n
          fld049_n <- BCCA_TDP_DATA_PURE_CONTROL$fld049_n
          BCCA_TDP_DATA_PURE_CONTROL$fld049_n <- ifelse(fld049_n > 0, "si", "no")
          rm(fld049_n)
          
          #fld050_n
          fld050_n <- BCCA_TDP_DATA_PURE_CONTROL$fld050_n
          BCCA_TDP_DATA_PURE_CONTROL$fld050_n <- ifelse(fld050_n > 0, "si", "no")
          rm(fld050_n)
          
          
          #fld053_n
          fld053_n <- BCCA_TDP_DATA_PURE_CONTROL$fld053_n
          BCCA_TDP_DATA_PURE_CONTROL$fld053_n <- ifelse(fld053_n == 0, "[0]", 
                                                        ifelse(fld053_n > 0 & fld053_n <= 50, "(0-50]", 
                                                               ifelse(fld053_n > 50 & fld053_n <= 100, "(50-100]", 
                                                                      ifelse(fld053_n > 100 & fld053_n <= 300, "(100-300]", 
                                                                             ifelse(fld053_n > 300, "(300-inf)", fld053_n)))))
          rm(fld053_n)
          
          
          #fld054_n
          fld054_n <- BCCA_TDP_DATA_PURE_CONTROL$fld054_n
          BCCA_TDP_DATA_PURE_CONTROL$fld054_n <- ifelse(fld054_n == 0, "[0]", 
                                                    ifelse(fld054_n > 0 & fld054_n <= 5, "(0-5]", 
                                                           ifelse(fld054_n > 5 & fld054_n <= 10, "(5-10]", 
                                                                  ifelse(fld054_n > 10 & fld054_n <= 15, "(10-15]",
                                                                         ifelse(fld054_n > 15 & fld054_n <= 40, "(15-40]", 
                                                                                ifelse(fld054_n > 40, "(40-inf)",fld054_n))))))
          rm(fld054_n)
          #fld057_n
          fld057_n <- BCCA_TDP_DATA_PURE_CONTROL$fld057_n
          BCCA_TDP_DATA_PURE_CONTROL$fld057_n <- ifelse(fld057_n == 0, "[0]", 
                                                        ifelse(fld057_n > 0 & fld057_n <= 50, "(0-50]", 
                                                               ifelse(fld057_n > 50 & fld057_n <= 100, "(50-100]", 
                                                                      ifelse(fld057_n > 100 & fld057_n <= 300, "(100-300]", 
                                                                             ifelse(fld057_n > 300, "(300-inf)", fld057_n)))))
          rm(fld057_n)
          
  
          #fld061_n
          fld061_n <- BCCA_TDP_DATA_PURE_CONTROL$fld061_n
          BCCA_TDP_DATA_PURE_CONTROL$fld061_n <- ifelse(fld061_n == 0, "[0]", 
                                                        ifelse(fld061_n > 0 & fld061_n <= 50, "(0-50]", 
                                                               ifelse(fld061_n > 50 & fld061_n <= 100, "(50-100]", 
                                                                      ifelse(fld061_n > 100 & fld061_n <= 300, "(100-300]", 
                                                                             ifelse(fld061_n > 300, "(300-inf)", fld061_n)))))
          rm(fld061_n)
          
          #fld062_n
          fld062_n <- BCCA_TDP_DATA_PURE_CONTROL$fld062_n
          BCCA_TDP_DATA_PURE_CONTROL$fld062_n <- ifelse(fld062_n == 0, "[0]", 
                                                         ifelse(fld062_n > 0 & fld062_n <= 5, "(0-5]", 
                                                                ifelse(fld062_n > 5 & fld062_n <= 10, "(5-10]", 
                                                                       ifelse(fld062_n > 10 & fld062_n <= 15, "(10-15]",
                                                                              ifelse(fld062_n > 15 & fld062_n <= 40, "(15-40]",
                                                                                     ifelse(fld062_n > 40, "(40-inf)", fld062_n))))))
          rm(fld062_n)
          
          #fld058_n
          fld058_n <- BCCA_TDP_DATA_PURE_CONTROL$fld058_n
          BCCA_TDP_DATA_PURE_CONTROL$fld058_n <- ifelse(fld058_n == 0, "[0]", 
                                                    ifelse(fld058_n > 0 & fld058_n <= 5, "(0-5]", 
                                                           ifelse(fld058_n > 5 & fld058_n <= 10, "(5-10]", 
                                                                  ifelse(fld058_n > 10 & fld058_n <= 15, "(10-15]",
                                                                         ifelse(fld058_n > 15 & fld058_n <= 40, "(15-40]", 
                                                                                ifelse(fld058_n > 40, "(40-inf)",fld058_n))))))
          rm(fld058_n) 
          
          #fld062_n
          fld062_n <- BCCA_TDP_DATA_PURE_CONTROL$fld062_n
          BCCA_TDP_DATA_PURE_CONTROL$fld062_n <- ifelse(fld062_n == 0, "[0]", 
                                                    ifelse(fld062_n > 0 & fld062_n <= 5, "(0-5]", 
                                                           ifelse(fld062_n > 5 & fld062_n <= 10, "(5-10]", 
                                                                  ifelse(fld062_n > 10 & fld062_n <= 15, "(10-15]",
                                                                         ifelse(fld062_n > 15 & fld062_n <= 40, "(15-40]", 
                                                                                ifelse(fld062_n > 40, "(40-inf)",fld062_n))))))
          rm(fld062_n) 
          
          
          #fld065_n
          fld065_n <- BCCA_TDP_DATA_PURE_CONTROL$fld065_n
          BCCA_TDP_DATA_PURE_CONTROL$fld065_n <- ifelse(fld065_n > 0, "si", "no")
          rm(fld065_n)
          
          #fld066_n
          fld066_n <- BCCA_TDP_DATA_PURE_CONTROL$fld066_n
          BCCA_TDP_DATA_PURE_CONTROL$fld066_n <- ifelse(fld066_n > 0, "si", "no")
          rm(fld066_n)
          
          #fld069_n
          fld069_n <- BCCA_TDP_DATA_PURE_CONTROL$fld069_n
          BCCA_TDP_DATA_PURE_CONTROL$fld069_n <- ifelse(fld069_n == 0, "[0]", 
                                                        ifelse(fld069_n > 0 & fld069_n <= 50, "(0-50]", 
                                                               ifelse(fld069_n > 50 & fld069_n <= 150, "(50-150]",
                                                                      ifelse(fld069_n > 150 & fld069_n <= 300, "(150-300]", 
                                                                             ifelse(fld069_n > 300 & fld069_n <= 700, "(300-700]", 
                                                                                    ifelse(fld069_n > 700, "(700-inf)", fld069_n))))))
          
          #fld070_n
          fld070_n <- BCCA_TDP_DATA_PURE_CONTROL$fld070_n
          BCCA_TDP_DATA_PURE_CONTROL$fld070_n <- ifelse(fld070_n == 0, "[0]", 
                                                    ifelse(fld070_n > 0 & fld070_n <= 25, "(0-25]",
                                                           ifelse(fld070_n > 25 & fld070_n <= 100, "(25-100]", 
                                                                  ifelse(fld070_n > 100, "(100-inf)", fld070_n))))
          rm(fld070_n)
          
          #fld073_n
          fld073_n <- BCCA_TDP_DATA_PURE_CONTROL$fld073_n
          BCCA_TDP_DATA_PURE_CONTROL$fld073_n <- ifelse(fld073_n > 0, "si", "no")
          rm(fld073_n)
          
          #fld074_n
          fld074_n <- BCCA_TDP_DATA_PURE_CONTROL$fld074_n
          BCCA_TDP_DATA_PURE_CONTROL$fld074_n <- ifelse(fld074_n > 0, "si", "no")
          rm(fld074_n)
          
          #fld115_n
          fld115_n <- BCCA_TDP_DATA_PURE_CONTROL$fld115_n
          BCCA_TDP_DATA_PURE_CONTROL$fld115_n <- ifelse(fld115_n == 0, "[0]", 
                                                    ifelse(fld115_n > 0 & fld115_n <= 4000, "limitado",
                                                           ifelse(fld115_n > 4000, "ilimitado", fld115_n)))
          rm(fld115_n)
          
          #fld116_n
          fld116_n <- BCCA_TDP_DATA_PURE_CONTROL$fld116_n
          BCCA_TDP_DATA_PURE_CONTROL$fld116_n <- ifelse(fld116_n == 0, "[0]", 
                                                    ifelse(fld116_n > 0 & fld116_n <= 20000000, "limitado",
                                                           ifelse(fld116_n > 20000000, "ilimitado", fld116_n)))
          rm(fld116_n)
          
          #fld117_n
          fld117_n <- BCCA_TDP_DATA_PURE_CONTROL$fld117_n
          BCCA_TDP_DATA_PURE_CONTROL$fld117_n <- ifelse(fld117_n == 0, "[0]", 
                                                        ifelse(fld117_n > 0 & fld117_n <= 50, "(0-50]",
                                                               ifelse(fld117_n > 50 & fld117_n <= 200, "(50-200]",
                                                                      ifelse(fld117_n > 200 & fld117_n <= 900, "(200-900]",
                                                                             ifelse(fld117_n > 900, ">900", fld117_n)))))
          rm(fld117_n)
          
          names(BCCA_TDP_DATA_PURE_CONTROL) <-  c("edad_n"     , "antig_n"   , "eqanti_n"  , "linper_c"  , "rec1to_n"   ,"rec1fi_n"  , "rec1pr_n"  , "rec1pe_n"   ,
                                                  "rec1to_2_n" ,"rec1fi_2_n", "rec1pr_2_n", "rec3to_n"  , "costpl_n"  , "conren_c"   ,"cnttoc_n"  , "cntnrc_n"  , "cnttmc_n"   ,
                                                  "cntctd_n"   ,"cntctl_c"  , "cntctl_1_c", "cntctl_2_c", "cntsol_c"                 ,"rep1to_n"  , "rep1fi_n"  , "rep1to_1_n" ,
                                                  "rep1fi_1_n" ,"rep1to_2_n", "rep1fi_2_n", "rep2to_n"  , "rep2fi_n"  , "rep2to_1_n" ,"rep2fi_1_n", "rep2to_2_n", "rep2fi_2_n" ,
                                                  "rep3to_2_n" ,"rep3fi_2_n", "rep4to_n"  , "rep4fi_n"  , "rep4to_2_n", "rep4fi_2_n" ,"fld041_c"  , "fld042_c"  , "fld045_c"   ,
                                                  "fld046_c"   ,"fld049_c"  , "fld050_c"  , "fld053_c"  , "fld054_c"  , "fld057_n"   ,"fld058_c"  , "fld061_c"  , "fld062_c"   ,
                                                  "fld065_c"   ,"fld066_c"  , "fld069_c"  , "fld070_c"  , "fld073_c"  , "fld074_c"   ,"fld115_c"  , "fld116_c"  , "fld117_c"   ,
                                                  "fld119_c"   ,"fld120_c"  , "fld121_c"  , "fld122_c"  , "fld138_n"  , "fld139_n"   ,"fld140_n"  , "fld141_n"  , "fld142_n"   ,
                                                  "fld143_n"   ,"fld144_n"  , "fld145_n"  , "fld146_n"  , "rec1to_3_n", "rec2to_3_n" ,"rep1to_3_n", "rep2to_3_n", "rep3to_3_n" ,
                                                  "rep4to_3_n" ,"target_c" )
        }
        
        
        
        
        
        
        
        
        
        print("EXPORTANDO DATA")
        
        switch(opcion, 
               B = {
                 #Caso 1: Se ingresó el path destino: opcion = "batch"
                 #Genera excel:
                 vector_resultante <- c("","","","","","")
                 
  
                 if(substr(path_destino,nchar(path_destino),nchar(path_destino)) == "/"){
                   if(flag_trama_prepago){
                     path_destino_prepago <- paste(path_destino,NOMBRE_DATASET,toupper(VECTOR_PRODUCTOS[1]),EXTENSION_ARCHIVO_DESTINO, sep = "") 
                     vector_resultante[1] <- VECTOR_PRODUCTOS[1]
                     vector_resultante[4] <- path_destino_prepago
                     write.csv(BCCA_TDP_DATA_PURE_PREPAGO, path_destino_prepago, row.names = FALSE)
                   }
                   if(flag_trama_postpago){
                     path_destino_postpago <- paste(path_destino,NOMBRE_DATASET,toupper(VECTOR_PRODUCTOS[2]),EXTENSION_ARCHIVO_DESTINO, sep = "") 
                     vector_resultante[2] <- VECTOR_PRODUCTOS[2]
                     vector_resultante[5] <- path_destino_postpago
                     write.csv(BCCA_TDP_DATA_PURE_POSTPAGO, path_destino_postpago, row.names = FALSE)
                   }
                   if(flag_trama_control){
                     path_destino_control <- paste(path_destino,NOMBRE_DATASET,toupper(VECTOR_PRODUCTOS[3]),EXTENSION_ARCHIVO_DESTINO, sep = "") 
                     vector_resultante[3] <- VECTOR_PRODUCTOS[3]
                     vector_resultante[6] <- path_destino_control
                     write.csv(BCCA_TDP_DATA_PURE_CONTROL, path_destino_control, row.names = FALSE)
                   }
                 }else{
                   if(flag_trama_prepago){
                     path_destino_prepago <- paste(path_destino,"/",NOMBRE_DATASET,toupper(VECTOR_PRODUCTOS[1]),EXTENSION_ARCHIVO_DESTINO, sep = "") 
                     vector_resultante[1] <- VECTOR_PRODUCTOS[1]
                     vector_resultante[4] <- path_destino_prepago
                     write.csv(BCCA_TDP_DATA_PURE_PREPAGO, path_destino_prepago, row.names = FALSE)
                   }
                   if(flag_trama_postpago){
                     path_destino_postpago <- paste(path_destino,"/",NOMBRE_DATASET,toupper(VECTOR_PRODUCTOS[2]),EXTENSION_ARCHIVO_DESTINO, sep = "") 
                     vector_resultante[2] <- VECTOR_PRODUCTOS[2]
                     vector_resultante[5] <- path_destino_postpago
                     write.csv(BCCA_TDP_DATA_PURE_POSTPAGO, path_destino_postpago, row.names = FALSE)
                   }
                   if(flag_trama_control){
                     path_destino_control <- paste(path_destino,"/",NOMBRE_DATASET,toupper(VECTOR_PRODUCTOS[3]),EXTENSION_ARCHIVO_DESTINO, sep = "")
                     vector_resultante[3] <- unlist(VECTOR_PRODUCTOS[3])
                     vector_resultante[6] <- unlist(path_destino_control)
                     write.csv(BCCA_TDP_DATA_PURE_CONTROL, path_destino_control, row.names = FALSE)
                   }
                   
                 }
                 
                 matriz_resultante <- matrix(vector_resultante, nrow = 3)
                 print(matriz_resultante)
                 
                 #Llenando output
                 lista_output[[1]] <- "OK"
                 lista_output[[3]] <- matriz_resultante
  
                 return(lista_output)
                 
               },
               
               O = {
                 #Caso 2: Se ingresó la trama opcion = "batch"    
                 #Muestra en consola
                 #print(BCCA_TDP_DATA_PURE_PREPAGO)
                 if(flag_trama_prepago){
                   trama_transformada <- do.call(paste, c(BCCA_TDP_DATA_PURE_PREPAGO, sep = token))
                 }else if(flag_trama_postpago){
                   trama_transformada <- do.call(paste, c(BCCA_TDP_DATA_PURE_POSTPAGO, sep = token))
                 }else if(flag_trama_control){
                   trama_transformada <- do.call(paste, c(BCCA_TDP_DATA_PURE_CONTROL, sep = token))
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
    lista_output[1] <- out
    
    return(lista_output)
    
  }