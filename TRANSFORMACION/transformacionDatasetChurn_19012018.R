#path_origen: Lugar donde se encuentra el dataset a transformar:   .../.../.../... .csv
#path_destino: Lugar donde se exportará el/los dataset(s) (no posee nombre de archivo): .../.../.../    

transformacionDatasetChurn <- function(path_origen, path_destino, trama, token, opcion){
  
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
               print(paste("IMPORTANDO DATASET DESDE ", path_origen), sep="")
               BCCA_TDP_DATA_PURE <- read_csv(path_origen, 
                                          col_types = list("vcplan_n" = col_double(), "vctcpl_n"= col_double(), 
                                                           "vccpno_n"= col_double(), "vccpte_n"= col_double(), 
                                                           "fmtexr_n"= col_double(), "fmexr1_n"= col_double(), 
                                                           "fmexr3_n"= col_double(), "fmexr6_n"= col_double(), 
                                                           "figtdp_n"= col_double(), "cant1m_n"= col_double(), 
                                                           "mont1m_n"= col_double(), "cant3m_n"= col_double(), 
                                                           "mont3m_n"= col_double(), "cant6m_n"= col_double(), 
                                                           "mont6m_n"= col_double(), "ctotal_n"= col_double(), 
                                                           "mtotal_n"= col_double()))
               
               #Verifica niveles por producto
               productos <- levels(as.factor(BCCA_TDP_DATA_PURE$vprodu_c))
               if(length(levels(as.factor(BCCA_TDP_DATA_PURE$vprodu_c))) == 1){
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
                 names(registro) <-   c("xtelef_n"  ,"vprodu_c"  , "vedadc_n"    , "vantig_n"   , "veanti_n"   ,"rec4to_2_n", "rec4fi_2_n","rec4pr_2_n", "vcsbaj_n"  , "vtrp13_n"  ,
                                        "vrp13f_n", "vtrp16_n", "vrp16f_n", "rep2to_1_n" ,"rep2fi_1_n", "rep2to_2_n","rep2fi_2_n", "vtrp36_n", "vrp36f_n"    ,
                                        "ttmrcl_n"  , "tmrcl1_n"  , "ttmren_n"  , "tmren1_n"   ,"ttmrvi_n"  , "tmrvi1_n"  ,"ttmhcl_n"  , "tmhcl1_n"  , "ttmhen_n"      ,
                                        "tmhen1_n"  , "ttmhvi_n"  , "tmhvi1_n"  , "ttkbco_n"   ,"tkbco1_n"  , "tsmsen_n"  ,"tsmse1_n"  , "ttcall_n"  , "tcall1_n"      ,
                                        "pli_sn_c"  , "cant1m_n"  , "mont1m_n"  , "cant3m_n"   ,"mont3m_n"  , "cant6m_n"  ,"mont6m_n"  , "ctotal_n"  , "mtotal_n"      ,
                                        "vtrec1_n", "rec3to_3_n", "rec4to_3_n", "vtrep1_n" ,"vtrep2_n", "vtrep3_n")
                 #Asignación del tipo de dato. Convierte factor a numérico
                 registro[,  c("xtelef_n"  ,"vedadc_n"    , "vantig_n"   , "veanti_n"   ,"rec4to_2_n", "rec4fi_2_n","rec4pr_2_n", "vcsbaj_n"  , "vtrp13_n"  ,
                               "vrp13f_n", "vtrp16_n", "vrp16f_n", "rep2to_1_n" ,"rep2fi_1_n", "rep2to_2_n","rep2fi_2_n", "vtrp36_n", "vrp36f_n"    ,
                               "ttmrcl_n"  , "tmrcl1_n"  , "ttmren_n"  , "tmren1_n"   ,"ttmrvi_n"  , "tmrvi1_n"  ,"ttmhcl_n"  , "tmhcl1_n"  , "ttmhen_n"      ,
                               "tmhen1_n"  , "ttmhvi_n"  , "tmhvi1_n"  , "ttkbco_n"   ,"tkbco1_n"  , "tsmsen_n"  ,"tsmse1_n"  , "ttcall_n"  , "tcall1_n"      ,
                               "cant1m_n"  , "mont1m_n"  , "cant3m_n"   ,"mont3m_n"  , "cant6m_n"  ,"mont6m_n"  , "ctotal_n"  , "mtotal_n"      ,
                               "vtrec1_n", "rec3to_3_n", "rec4to_3_n", "vtrep1_n" ,"vtrep2_n", "vtrep3_n")] <- as.numeric(as.character(unlist(registro[,  c("vedadc_n"    , "vantig_n"   , "veanti_n"   ,"rec4to_2_n", "rec4fi_2_n","rec4pr_2_n", "vcsbaj_n"  , "vtrp13_n"  ,
                                                                                                                                                                    "vrp13f_n", "vtrp16_n", "vrp16f_n", "rep2to_1_n" ,"rep2fi_1_n", "rep2to_2_n","rep2fi_2_n", "vtrp36_n", "vrp36f_n"    ,
                                                                                                                                                                    "ttmrcl_n"  , "tmrcl1_n"  , "ttmren_n"  , "tmren1_n"   ,"ttmrvi_n"  , "tmrvi1_n"  ,"ttmhcl_n"  , "tmhcl1_n"  , "ttmhen_n"      ,
                                                                                                                                                                    "tmhen1_n"  , "ttmhvi_n"  , "tmhvi1_n"  , "ttkbco_n"   ,"tkbco1_n"  , "tsmsen_n"  ,"tsmse1_n"  , "ttcall_n"  , "tcall1_n"      ,
                                                                                                                                                                    "cant1m_n"  , "mont1m_n"  , "cant3m_n"   ,"mont3m_n"  , "cant6m_n"  ,"mont6m_n"  , "ctotal_n"  , "mtotal_n"      ,
                                                                                                                                                                    "vtrec1_n", "rec3to_3_n", "rec4to_3_n", "vtrep1_n" ,"vtrep2_n", "vtrep3_n")])))
                 #Asignación del tipo de dato. Convierte factor a character
                 registro[,c("vprodu_c" , "pli_sn_c")] <- as.character(unlist(registro[,c("vprodu_c", "pli_sn_c")]))
                 
                 return(registro)
               }
               BCCA_TDP_DATA_PURE <- recibeTramaPrepago(trama,token)
               
               }else if(producto_online %in% VECTOR_PRODUCTOS[2]){
               
               recibeTramaPostpago <- function(trama,tokenp){
                 registro <- as.data.frame(do.call(rbind,strsplit(trama,tokenp)))
                 names(registro) <- c("xtelef_n"  ,"vprodu_c"  , "vedadc_n"     ,"vantig_n"   , "veanti_n"   ,"vtperm_c"  , "vtrc11_n"  , "vrc11f_n"  , "vrc11p_n"  , "vtrc13_n" ,
                                      "vrc13f_n", "vrc13c_n" ,"vtrc16_n", "vrc16f_n" ,"vrc16c_n", "vcplan_n"  , "vcrequ_c"  , "vctcpl_n"  , "vccpno_n"   ,
                                      "vccpte_n"  , "vccdeu_n"   ,"vcclcv_n"  , "vcclcs_n" ,"vcclcd_n", "vcsbaj_n"  , "vtrp11_n"  , "vrp11f_n"   ,
                                      "vtrp13_n", "vrp13f_n" ,"vtrp16_n", "vrp16f_n" ,"vtrp21_n"  , "vrp21f_n"  , "vtrp36_n", "vrp36f_n", "vtrp41_n"   ,
                                      "vrp41f_n"  , "vtrp46_n" ,"vrp46f_n", "ttmrcl_n"   ,"tmrcl1_n"  , "ttmren_n"  , "tmren1_n"  , "ttmrvi_n"  , "tmrvi1_n"   ,
                                      "ttmhcl_n"  , "tmhcl1_n"   ,"ttmhen_n"  , "tmhen1_n"   ,"ttmhvi_n"  , "tmhvi1_n"  , "ttkbco_n"  , "tkbco1_n"  , "tsmsen_n"   ,
                                      "tsmse1_n"  , "ttcall_n"   ,"tcall1_n"  , "tcmlpl_n"   ,"tckbpl_n"  , "tcsmsp_n"  , "tmilmm_c"  , "tmilmf_c"  , "tmilmo_c"   ,
                                      "tsmsil_c"  , "ftexre_n"   ,"fmtexr_n"  , "fcexr1_n"   ,"fmexr1_n"  , "fcexr3_n"  , "fmexr3_n"  , "fcexr6_n"  , "fmexr6_n"   ,
                                      "figtdp_n"  , "vtrec1_n" ,"vtrep1_n", "vtrep2_n" ,"vtrep3_n", "vtrep4_n")
                 
                 #Asignación del tipo de dato. Convierte factor a numérico
                 registro[,c("xtelef_n"  ,"vedadc_n"  ,"vantig_n"   , "veanti_n" , "vtrc11_n"  , "vrc11f_n"  , "vrc11p_n"  , "vtrc13_n" ,
                             "vrc13f_n", "vrc13c_n" ,"vtrc16_n", "vrc16f_n" ,"vrc16c_n", "vcplan_n"  , "vctcpl_n"  , "vccpno_n"   ,
                             "vccpte_n"  , "vccdeu_n"   ,"vcclcv_n"  , "vcclcs_n" ,"vcclcd_n", "vcsbaj_n"  , "vtrp11_n"  , "vrp11f_n"   ,
                             "vtrp13_n", "vrp13f_n" ,"vtrp16_n", "vrp16f_n" ,"vtrp21_n"  , "vrp21f_n"  , "vtrp36_n", "vrp36f_n", "vtrp41_n"   ,
                             "vrp41f_n"  , "vtrp46_n" ,"vrp46f_n", "ttmrcl_n"   ,"tmrcl1_n"  , "ttmren_n"  , "tmren1_n"  , "ttmrvi_n"  , "tmrvi1_n"   ,
                             "ttmhcl_n"  , "tmhcl1_n"   ,"ttmhen_n"  , "tmhen1_n"   ,"ttmhvi_n"  , "tmhvi1_n"  , "ttkbco_n"  , "tkbco1_n"  , "tsmsen_n"   ,
                             "tsmse1_n"  , "ttcall_n"   ,"tcall1_n"  , "tcmlpl_n"   ,"tckbpl_n"  , "tcsmsp_n"  , 
                             "ftexre_n"   ,"fmtexr_n"  , "fcexr1_n"   ,"fmexr1_n"  , "fcexr3_n"  , "fmexr3_n"  , "fcexr6_n"  , "fmexr6_n"   ,
                             "figtdp_n"  , "vtrec1_n" ,"vtrep1_n", "vtrep2_n" ,"vtrep3_n", "vtrep4_n" )] <- as.numeric(as.character(unlist(registro[,c("vedadc_n"  ,"vantig_n"   , "veanti_n" , "vtrc11_n"  , "vrc11f_n"  , "vrc11p_n"  , "vtrc13_n" ,
                                                                                                                                                                 "vrc13f_n", "vrc13c_n" ,"vtrc16_n", "vrc16f_n" ,"vrc16c_n", "vcplan_n"  , "vctcpl_n"  , "vccpno_n"   ,
                                                                                                                                                                 "vccpte_n"  , "vccdeu_n"   ,"vcclcv_n"  , "vcclcs_n" ,"vcclcd_n", "vcsbaj_n"  , "vtrp11_n"  , "vrp11f_n"   ,
                                                                                                                                                                 "vtrp13_n", "vrp13f_n" ,"vtrp16_n", "vrp16f_n" ,"vtrp21_n"  , "vrp21f_n"  , "vtrp36_n", "vrp36f_n", "vtrp41_n"   ,
                                                                                                                                                                 "vrp41f_n"  , "vtrp46_n" ,"vrp46f_n", "ttmrcl_n"   ,"tmrcl1_n"  , "ttmren_n"  , "tmren1_n"  , "ttmrvi_n"  , "tmrvi1_n"   ,
                                                                                                                                                                 "ttmhcl_n"  , "tmhcl1_n"   ,"ttmhen_n"  , "tmhen1_n"   ,"ttmhvi_n"  , "tmhvi1_n"  , "ttkbco_n"  , "tkbco1_n"  , "tsmsen_n"   ,
                                                                                                                                                                 "tsmse1_n"  , "ttcall_n"   ,"tcall1_n"  , "tcmlpl_n"   ,"tckbpl_n"  , "tcsmsp_n"  , 
                                                                                                                                                                 "ftexre_n"   ,"fmtexr_n"  , "fcexr1_n"   ,"fmexr1_n"  , "fcexr3_n"  , "fmexr3_n"  , "fcexr6_n"  , "fmexr6_n"   ,
                                                                                                                                                                 "figtdp_n"  , "vtrec1_n" ,"vtrep1_n", "vtrep2_n" ,"vtrep3_n", "vtrep4_n" )])))
                 #Asignación del tipo de dato. Convierte factor a character
                 registro[,c("vprodu_c" , "vtperm_c"   ,"vcrequ_c"  , "tmilmm_c"  , "tmilmf_c"  , "tmilmo_c"   ,
                             "tsmsil_c")] <- as.character(unlist(registro[,c("vprodu_c" , "vtperm_c"   ,"vcrequ_c"  , "tmilmm_c"  , "tmilmf_c"  , "tmilmo_c"   ,
                                                                                           "tsmsil_c")]))
                 
                 return(registro)
               }
               BCCA_TDP_DATA_PURE <- recibeTramaPostpago(trama,token)
               }else if(producto_online %in% VECTOR_PRODUCTOS[3]){
               
               
               recibeTramaControl <- function(trama,tokenp){
                 registro <- as.data.frame(do.call(rbind,strsplit(trama,tokenp)))
                 names(registro) <- c("xtelef_n"  ,"vprodu_c"   ,"vedadc_n"    , "vantig_n"   , "veanti_n"  , "vtperm_c"  , "vtrc11_n"   ,"vrc11f_n"  , "rec1pr_n"  , "vrc11p_n"   ,
                                      "vtrc16_n" ,"vrc16f_n", "vrc16c_n", "rec3to_n"  , "vcplan_n"  , "vcrequ_c"   ,"vctcpl_n"  , "vccpno_n"  , "vccpte_n"   ,
                                      "vccdeu_n"   ,"vcclcv_n"  , "vcclcs_n", "vcclcd_n", "vcsbaj_n"  , "vtrp11_n"  , "vrp11f_n"  , "vtrp13_n" ,
                                      "vrp13f_n" ,"vtrp16_n", "vrp16f_n", "vtrp21_n"  , "vrp21f_n"  , "rep2to_1_n" ,"rep2fi_1_n", "rep2to_2_n", "rep2fi_2_n" ,
                                      "vtrp36_n" ,"vrp36f_n", "vtrp41_n"  , "vrp41f_n"  , "vtrp46_n", "vrp46f_n" ,"ttmrcl_n"  , "tmrcl1_n"  , "ttmren_n"   ,
                                      "tmren1_n"   ,"ttmrvi_n"  , "tmrvi1_n"  , "ttmhcl_n"  , "tmhcl1_n"  , "ttmhen_n"   ,"tmhen1_n"  , "ttmhvi_n"  , "tmhvi1_n"   ,
                                      "ttkbco_n"   ,"tkbco1_n"  , "tsmsen_n"  , "tsmse1_n"  , "ttcall_n"  , "tcall1_n"   ,"tcmlpl_n"  , "tckbpl_n"  , "tcsmsp_n"   ,
                                      "tmilmm_c"   ,"tmilmf_c"  , "tmilmo_c"  , "tsmsil_c"  , "ftexre_n"  , "fmtexr_n"   ,"fcexr1_n"  , "fmexr1_n"  , "fcexr3_n"   ,
                                      "fmexr3_n"   ,"fcexr6_n"  , "fmexr6_n"  , "figtdp_n"  , "vtrec1_n", "rec2to_3_n" ,"vtrep1_n", "vtrep2_n", "vtrep3_n" ,
                                      "vtrep4_n")
                 
                 #Asignación del tipo de dato. Convierte factor a numérico
                 registro[,c("xtelef_n"  ,"vedadc_n"    , "vantig_n"   , "veanti_n"  , "vtrc11_n"   ,"vrc11f_n"  , "rec1pr_n"  , "vrc11p_n"   ,
                             "vtrc16_n" ,"vrc16f_n", "vrc16c_n", "rec3to_n"  , "vcplan_n"  ,"vctcpl_n"  , "vccpno_n"  , "vccpte_n"   ,
                             "vccdeu_n"   ,"vcclcv_n"  , "vcclcs_n", "vcclcd_n", "vcsbaj_n"  ,"vtrp11_n"  , "vrp11f_n"  , "vtrp13_n" ,
                             "vrp13f_n" ,"vtrp16_n", "vrp16f_n", "vtrp21_n"  , "vrp21f_n"  , "rep2to_1_n" ,"rep2fi_1_n", "rep2to_2_n", "rep2fi_2_n" ,
                             "vtrp36_n" ,"vrp36f_n", "vtrp41_n"  , "vrp41f_n"  , "vtrp46_n", "vrp46f_n" ,"ttmrcl_n"  , "tmrcl1_n"  , "ttmren_n"   ,
                             "tmren1_n"   ,"ttmrvi_n"  , "tmrvi1_n"  , "ttmhcl_n"  , "tmhcl1_n"  , "ttmhen_n"   ,"tmhen1_n"  , "ttmhvi_n"  , "tmhvi1_n"   ,
                             "ttkbco_n"   ,"tkbco1_n"  , "tsmsen_n"  , "tsmse1_n"  , "ttcall_n"  , "tcall1_n"   ,"tcmlpl_n"  , "tckbpl_n"  , "tcsmsp_n"   ,
                             "ftexre_n"  , "fmtexr_n"   ,"fcexr1_n"  , "fmexr1_n"  , "fcexr3_n"   ,
                             "fmexr3_n"   ,"fcexr6_n"  , "fmexr6_n"  , "figtdp_n"  , "vtrec1_n", "rec2to_3_n" ,"vtrep1_n", "vtrep2_n", "vtrep3_n" ,
                             "vtrep4_n")] <- as.numeric(as.character(unlist(registro[,c("vedadc_n"    , "vantig_n"   , "veanti_n"  , "vtrc11_n"   ,"vrc11f_n"  , "rec1pr_n"  , "vrc11p_n"   ,
                                                                                          "vtrc16_n" ,"vrc16f_n", "vrc16c_n", "rec3to_n"  , "vcplan_n"  ,"vctcpl_n"  , "vccpno_n"  , "vccpte_n"   ,
                                                                                          "vccdeu_n"   ,"vcclcv_n"  , "vcclcs_n", "vcclcd_n", "vcsbaj_n"  ,"vtrp11_n"  , "vrp11f_n"  , "vtrp13_n" ,
                                                                                          "vrp13f_n" ,"vtrp16_n", "vrp16f_n", "vtrp21_n"  , "vrp21f_n"  , "rep2to_1_n" ,"rep2fi_1_n", "rep2to_2_n", "rep2fi_2_n" ,
                                                                                          "vtrp36_n" ,"vrp36f_n", "vtrp41_n"  , "vrp41f_n"  , "vtrp46_n", "vrp46f_n" ,"ttmrcl_n"  , "tmrcl1_n"  , "ttmren_n"   ,
                                                                                          "tmren1_n"   ,"ttmrvi_n"  , "tmrvi1_n"  , "ttmhcl_n"  , "tmhcl1_n"  , "ttmhen_n"   ,"tmhen1_n"  , "ttmhvi_n"  , "tmhvi1_n"   ,
                                                                                          "ttkbco_n"   ,"tkbco1_n"  , "tsmsen_n"  , "tsmse1_n"  , "ttcall_n"  , "tcall1_n"   ,"tcmlpl_n"  , "tckbpl_n"  , "tcsmsp_n"   ,
                                                                                          "ftexre_n"  , "fmtexr_n"   ,"fcexr1_n"  , "fmexr1_n"  , "fcexr3_n"   ,
                                                                                          "fmexr3_n"   ,"fcexr6_n"  , "fmexr6_n"  , "figtdp_n"  , "vtrec1_n", "rec2to_3_n" ,"vtrep1_n", "vtrep2_n", "vtrep3_n" ,
                                                                                          "vtrep4_n")])))
                 #Asignación del tipo de dato. Convierte factor a character
                 registro[,c("vprodu_c" , "vtperm_c"  , "vcrequ_c", "tmilmm_c"   ,"tmilmf_c"  , "tmilmo_c"  , "tsmsil_c")] <- as.character(unlist(registro[,c("vprodu_c" , "vtperm_c"  , "vcrequ_c", "tmilmm_c"   ,"tmilmf_c"  , "tmilmo_c"  , "tsmsil_c")]))
                 
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
      
      #Eliminando el campo teléfono.
      BCCA_TDP_DATA_PURE <- eliminaCaracteristica(BCCA_TDP_DATA_PURE, c("xtelef_n"))
      #Separando dataset PREPAGO
      if(VECTOR_PRODUCTOS[1] %in% productos){
        BCCA_TDP_DATA_PURE_PREPAGO<- subset(BCCA_TDP_DATA_PURE, BCCA_TDP_DATA_PURE$vprodu_c == VECTOR_PRODUCTOS[1])
        BCCA_TDP_DATA_PURE_PREPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_PREPAGO, c("vprodu_c"))
        producto_trama <- VECTOR_PRODUCTOS[1]
        flag_trama_prepago <- TRUE
        flag_trama_postpago <- FALSE
        flag_trama_control <- FALSE
        print("TRANSFORMANDO DATA PREPAGO")
        
        #vedadc_n
        vedadc_n <- BCCA_TDP_DATA_PURE_PREPAGO$vedadc_n
        BCCA_TDP_DATA_PURE_PREPAGO$vedadc_n <- ifelse(vedadc_n > 70, round(mean(vedadc_n)), vedadc_n)
        rm(vedadc_n)

        #vcsbaj_n
        vcsbaj_n <- BCCA_TDP_DATA_PURE_PREPAGO$vcsbaj_n
        BCCA_TDP_DATA_PURE_PREPAGO$vcsbaj_n <- ifelse(vcsbaj_n > 0, "si", "no")
        rm(vcsbaj_n)
        
        #ttmrcl_n
        ttmrcl_n <- BCCA_TDP_DATA_PURE_PREPAGO$ttmrcl_n
        BCCA_TDP_DATA_PURE_PREPAGO$ttmrcl_n <- ifelse(ttmrcl_n > 0, "si", "no")
        rm(ttmrcl_n)
        
        #tmrcl1_n
        tmrcl1_n <- BCCA_TDP_DATA_PURE_PREPAGO$tmrcl1_n
        BCCA_TDP_DATA_PURE_PREPAGO$tmrcl1_n <- ifelse(tmrcl1_n > 0, "si", "no")
        rm(tmrcl1_n)
        
        #ttmren_n
        ttmren_n <- BCCA_TDP_DATA_PURE_PREPAGO$ttmren_n
        BCCA_TDP_DATA_PURE_PREPAGO$ttmren_n <- ifelse(ttmren_n > 0, "si", "no")
        rm(ttmren_n)
        
        #tmren1_n
        tmren1_n <- BCCA_TDP_DATA_PURE_PREPAGO$tmren1_n
        BCCA_TDP_DATA_PURE_PREPAGO$tmren1_n <- ifelse(tmren1_n > 0, "si", "no")
        rm(tmren1_n)
        
        #ttmrvi_n
        ttmrvi_n <- BCCA_TDP_DATA_PURE_PREPAGO$ttmrvi_n
        BCCA_TDP_DATA_PURE_PREPAGO$ttmrvi_n <- ifelse(ttmrvi_n > 0, "si", "no")
        rm(ttmrvi_n)
        
        #tmrvi1_n
        tmrvi1_n <- BCCA_TDP_DATA_PURE_PREPAGO$tmrvi1_n
        BCCA_TDP_DATA_PURE_PREPAGO$tmrvi1_n <- ifelse(tmrvi1_n > 0, "si", "no")
        rm(tmrvi1_n)
        
        #ttmhcl_n
        ttmhcl_n <- BCCA_TDP_DATA_PURE_PREPAGO$ttmhcl_n
        BCCA_TDP_DATA_PURE_PREPAGO$ttmhcl_n <- ifelse(ttmhcl_n == 0, "[0]", 
                                                  ifelse(ttmhcl_n > 0 & ttmhcl_n <= 50, "(0-50]", 
                                                         ifelse(ttmhcl_n > 50 & ttmhcl_n <= 100, "(50-100]", 
                                                                ifelse(ttmhcl_n > 100 & ttmhcl_n <= 300, "(100-300]", 
                                                                       ifelse(ttmhcl_n > 300, "(300-inf)", ttmhcl_n)))))
        rm(ttmhcl_n)
        
        #ttmhvi_n
        ttmhvi_n <- BCCA_TDP_DATA_PURE_PREPAGO$ttmhvi_n
        BCCA_TDP_DATA_PURE_PREPAGO$ttmhvi_n <- ifelse(ttmhvi_n == 0, "[0]", 
                                                      ifelse(ttmhvi_n > 0 & ttmhvi_n <= 50, "(0-50]", 
                                                             ifelse(ttmhvi_n > 50 & ttmhvi_n <= 100, "(50-100]", 
                                                                    ifelse(ttmhvi_n > 100 & ttmhvi_n <= 300, "(100-300]", 
                                                                           ifelse(ttmhvi_n > 300, "(300-inf)", ttmhvi_n)))))
        rm(ttmhvi_n)
        
        
        #tmhcl1_n
        tmhcl1_n <- BCCA_TDP_DATA_PURE_PREPAGO$tmhcl1_n
        BCCA_TDP_DATA_PURE_PREPAGO$tmhcl1_n <- ifelse(tmhcl1_n == 0, "[0]", 
                                                  ifelse(tmhcl1_n > 0 & tmhcl1_n <= 5, "(0-5]", 
                                                         ifelse(tmhcl1_n > 5 & tmhcl1_n <= 10, "(5-10]", 
                                                                ifelse(tmhcl1_n > 10 & tmhcl1_n <= 15, "(10-15]",
                                                                       ifelse(tmhcl1_n > 15 & tmhcl1_n <= 40, "(15-40]",
                                                                              ifelse(tmhcl1_n > 40, "(40-inf)", tmhcl1_n))))))
        
        rm(tmhcl1_n)
        
        #tmhvi1_n
        tmhvi1_n <- BCCA_TDP_DATA_PURE_PREPAGO$tmhvi1_n
        BCCA_TDP_DATA_PURE_PREPAGO$tmhvi1_n <- ifelse(tmhvi1_n == 0, "[0]", 
                                                      ifelse(tmhvi1_n > 0 & tmhvi1_n <= 5, "(0-5]", 
                                                             ifelse(tmhvi1_n > 5 & tmhvi1_n <= 10, "(5-10]", 
                                                                    ifelse(tmhvi1_n > 10 & tmhvi1_n <= 15, "(10-15]",
                                                                           ifelse(tmhvi1_n > 15 & tmhvi1_n <= 40, "(15-40]",
                                                                                  ifelse(tmhvi1_n > 40, "(40-inf)", tmhvi1_n))))))
        
        rm(tmhvi1_n)
        
        
        #ttmhen_n
        ttmhen_n <- BCCA_TDP_DATA_PURE_PREPAGO$ttmhen_n
        BCCA_TDP_DATA_PURE_PREPAGO$ttmhen_n <- ifelse(ttmhen_n == 0, "[0]", 
                                                  ifelse(ttmhen_n > 0 & ttmhen_n <= 50, "(0-50]", 
                                                         ifelse(ttmhen_n > 50 & ttmhen_n <= 100, "(50-100]", 
                                                                ifelse(ttmhen_n > 100 & ttmhen_n <= 300, "(100-300]", 
                                                                       ifelse(ttmhen_n > 300, "(300-inf)", ttmhen_n)))))
        rm(ttmhen_n)
        
        #tmhen1_n
        tmhen1_n <- BCCA_TDP_DATA_PURE_PREPAGO$tmhen1_n
        BCCA_TDP_DATA_PURE_PREPAGO$tmhen1_n <- ifelse(tmhen1_n == 0, "[0]", 
                                                       ifelse(tmhen1_n > 0 & tmhen1_n <= 5, "(0-5]", 
                                                              ifelse(tmhen1_n > 5 & tmhen1_n <= 10, "(5-10]", 
                                                                     ifelse(tmhen1_n > 10 & tmhen1_n <= 15, "(10-15]",
                                                                            ifelse(tmhen1_n > 15 & tmhen1_n <= 40, "(15-40]",
                                                                                   ifelse(tmhen1_n > 40, "(40-inf)", tmhen1_n))))))
        
        rm(tmhen1_n)
        
        
        #ttkbco_n
        ttkbco_n <- BCCA_TDP_DATA_PURE_PREPAGO$ttkbco_n
        BCCA_TDP_DATA_PURE_PREPAGO$ttkbco_n <- ifelse(ttkbco_n > 0, "si", "no")
        rm(ttkbco_n)
        
        
        #tkbco1_n
        tkbco1_n <- BCCA_TDP_DATA_PURE_PREPAGO$tkbco1_n
        BCCA_TDP_DATA_PURE_PREPAGO$tkbco1_n <- ifelse(tkbco1_n > 0, "si", "no")
        rm(tkbco1_n)
        
        #tsmsen_n
        tsmsen_n <- BCCA_TDP_DATA_PURE_PREPAGO$tsmsen_n
        BCCA_TDP_DATA_PURE_PREPAGO$tsmsen_n <- ifelse(tsmsen_n == 0, "[0]", 
                                                  ifelse(tsmsen_n > 0 & tsmsen_n <= 50, "(0-50]", 
                                                         ifelse(tsmsen_n > 50 & tsmsen_n <= 150, "(50-150]",
                                                                ifelse(tsmsen_n > 150 & tsmsen_n <= 300, "(150-300]", 
                                                                       ifelse(tsmsen_n > 300 & tsmsen_n <= 700, "(300-700]", 
                                                                              ifelse(tsmsen_n > 700, "(700-inf)", tsmsen_n))))))
        rm(tsmsen_n)
        
        #tsmse1_n
        tsmse1_n <- BCCA_TDP_DATA_PURE_PREPAGO$tsmse1_n
        BCCA_TDP_DATA_PURE_PREPAGO$tsmse1_n <- ifelse(tsmse1_n == 0, "[0]", 
                                                  ifelse(tsmse1_n > 0 & tsmse1_n <= 25, "(0-25]",
                                                         ifelse(tsmse1_n > 25 & tsmse1_n <= 100, "(25-100]", 
                                                                ifelse(tsmse1_n > 100, "(100-inf)", tsmse1_n))))
        rm(tsmse1_n)
        
        #ttcall_n
        ttcall_n <- BCCA_TDP_DATA_PURE_PREPAGO$ttcall_n
        BCCA_TDP_DATA_PURE_PREPAGO$ttcall_n <- ifelse(ttcall_n > 0, "si", "no")
        rm(ttcall_n)
        
        #tcall1_n
        tcall1_n <- BCCA_TDP_DATA_PURE_PREPAGO$tcall1_n
        BCCA_TDP_DATA_PURE_PREPAGO$tcall1_n <- ifelse(tcall1_n > 0, "si", "no")
        rm(tcall1_n)
        
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
        names(BCCA_TDP_DATA_PURE_PREPAGO) <-  c("vedadc_n"    , "vantig_n"   , "veanti_n"   ,"rec4to_2_n", "rec4fi_2_n","rec4pr_2_n", "cntsol_c"  , "vtrp13_n"    ,
                                                "vrp13f_n", "vtrp16_n", "vrp16f_n", "rep2to_1_n" ,"rep2fi_1_n", "rep2to_2_n","rep2fi_2_n", "vtrp36_n", "vrp36f_n"    ,
                                                "fld041_c"  , "fld042_c"  , "fld045_c"  , "fld046_c"   ,"fld049_c"  , "fld050_c"  ,"fld053_c"  , "fld054_c"  , "fld057_c"      ,
                                                "fld058_c"  , "fld061_c"  , "fld062_c"  , "fld065_c"   ,"fld066_c"  , "fld069_c"  ,"fld070_c"  , "fld073_c"  , "fld074_c"      ,
                                                "pli_sn_c"  , "cant1m_c"  , "mont1m_c"  , "cant3m_c"   ,"mont3m_c"  , "cant6m_c"  ,"mont6m_c"  , "ctotal_c"  , "mtotal_c"      ,
                                                "vtrec1_n", "rec3to_3_n", "rec4to_3_n", "vtrep1_n" ,"vtrep2_n", "vtrep3_n")
        
      }
      
      #Separando dataset POSTPAGO
      if(VECTOR_PRODUCTOS[2] %in% productos){
        producto_trama <- VECTOR_PRODUCTOS[2]
        BCCA_TDP_DATA_PURE_POSTPAGO<- subset(BCCA_TDP_DATA_PURE, BCCA_TDP_DATA_PURE$vprodu_c == "postpago")
        BCCA_TDP_DATA_PURE_POSTPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_POSTPAGO, c("vprodu_c"))
        flag_trama_prepago <- FALSE
        flag_trama_postpago <- TRUE
        flag_trama_control <- FALSE
        print("TRANSFORMANDO DATA POSTPAGO")
        
        #vedadc_n
        vedadc_n <- BCCA_TDP_DATA_PURE_POSTPAGO$vedadc_n
        BCCA_TDP_DATA_PURE_POSTPAGO$vedadc_n <- ifelse(vedadc_n > 70, round(mean(vedadc_n)), vedadc_n)
        rm(vedadc_n)
        

        #vcsbaj_n
        vcsbaj_n <- BCCA_TDP_DATA_PURE_POSTPAGO$vcsbaj_n
        BCCA_TDP_DATA_PURE_POSTPAGO$vcsbaj_n <- ifelse(vcsbaj_n > 0, "si", "no")
        rm(vcsbaj_n)
        
        
        #ttmrcl_n
        ttmrcl_n <- BCCA_TDP_DATA_PURE_POSTPAGO$ttmrcl_n
        BCCA_TDP_DATA_PURE_POSTPAGO$ttmrcl_n <- ifelse(ttmrcl_n > 0, "si", "no")
        rm(ttmrcl_n)
        
        #tmrcl1_n
        tmrcl1_n <- BCCA_TDP_DATA_PURE_POSTPAGO$tmrcl1_n
        BCCA_TDP_DATA_PURE_POSTPAGO$tmrcl1_n <- ifelse(tmrcl1_n > 0, "si", "no")
        rm(tmrcl1_n)
        
        #ttmren_n
        ttmren_n <- BCCA_TDP_DATA_PURE_POSTPAGO$ttmren_n
        BCCA_TDP_DATA_PURE_POSTPAGO$ttmren_n <- ifelse(ttmren_n > 0, "si", "no")
        rm(ttmren_n)
        
        #tmren1_n
        tmren1_n <- BCCA_TDP_DATA_PURE_POSTPAGO$tmren1_n
        BCCA_TDP_DATA_PURE_POSTPAGO$tmren1_n <- ifelse(tmren1_n > 0, "si", "no")
        rm(tmren1_n)
        
        #ttmrvi_n
        ttmrvi_n <- BCCA_TDP_DATA_PURE_POSTPAGO$ttmrvi_n
        BCCA_TDP_DATA_PURE_POSTPAGO$ttmrvi_n <- ifelse(ttmrvi_n > 0, "si", "no")
        rm(ttmrvi_n)

        #tmrvi1_n
        tmrvi1_n <- BCCA_TDP_DATA_PURE_POSTPAGO$tmrvi1_n
        BCCA_TDP_DATA_PURE_POSTPAGO$tmrvi1_n <- ifelse(tmrvi1_n > 0, "si", "no")
        rm(tmrvi1_n)
        
        #tmhcl1_n
        tmhcl1_n <- BCCA_TDP_DATA_PURE_POSTPAGO$tmhcl1_n
        BCCA_TDP_DATA_PURE_POSTPAGO$tmhcl1_n <- ifelse(tmhcl1_n == 0, "[0]", 
                                                   ifelse(tmhcl1_n > 0 & tmhcl1_n <= 5, "(0-5]", 
                                                          ifelse(tmhcl1_n > 5 & tmhcl1_n <= 10, "(5-10]", 
                                                                 ifelse(tmhcl1_n > 10 & tmhcl1_n <= 15, "(10-15]",
                                                                        ifelse(tmhcl1_n > 15 & tmhcl1_n <= 40, "(15-40]",
                                                                               ifelse(tmhcl1_n > 40, "(40-inf)", tmhcl1_n))))))
        
        rm(tmhcl1_n)
        
        #ttmhen_n
        ttmhen_n <- BCCA_TDP_DATA_PURE_POSTPAGO$ttmhen_n
        BCCA_TDP_DATA_PURE_POSTPAGO$ttmhen_n <- ifelse(ttmhen_n == 0, "[0]", 
                                                      ifelse(ttmhen_n > 0 & ttmhen_n <= 50, "(0-50]", 
                                                             ifelse(ttmhen_n > 50 & ttmhen_n <= 100, "(50-100]", 
                                                                    ifelse(ttmhen_n > 100 & ttmhen_n <= 300, "(100-300]", 
                                                                           ifelse(ttmhen_n > 300, "(300-inf)", ttmhen_n)))))
        rm(ttmhen_n)
        
        #tmhen1_n
        tmhen1_n <- BCCA_TDP_DATA_PURE_POSTPAGO$tmhen1_n
        BCCA_TDP_DATA_PURE_POSTPAGO$tmhen1_n <- ifelse(tmhen1_n == 0, "[0]", 
                                                   ifelse(tmhen1_n > 0 & tmhen1_n <= 5, "(0-5]", 
                                                          ifelse(tmhen1_n > 5 & tmhen1_n <= 10, "(5-10]", 
                                                                 ifelse(tmhen1_n > 10 & tmhen1_n <= 15, "(10-15]",
                                                                        ifelse(tmhen1_n > 15 & tmhen1_n <= 40, "(15-40]",
                                                                               ifelse(tmhen1_n > 40, "(40-inf)", tmhen1_n))))))
        
        rm(tmhen1_n)
        
        #ttmhvi_n
        ttmhvi_n <- BCCA_TDP_DATA_PURE_POSTPAGO$ttmhvi_n
        BCCA_TDP_DATA_PURE_POSTPAGO$ttmhvi_n <- ifelse(ttmhvi_n == 0, "[0]", 
                                                      ifelse(ttmhvi_n > 0 & ttmhvi_n <= 50, "(0-50]", 
                                                             ifelse(ttmhvi_n > 50 & ttmhvi_n <= 100, "(50-100]", 
                                                                    ifelse(ttmhvi_n > 100 & ttmhvi_n <= 300, "(100-300]", 
                                                                           ifelse(ttmhvi_n > 300, "(300-inf)", ttmhvi_n)))))
        rm(ttmhvi_n)
        
        #tmhvi1_n
        tmhvi1_n <- BCCA_TDP_DATA_PURE_POSTPAGO$tmhvi1_n
        BCCA_TDP_DATA_PURE_POSTPAGO$tmhvi1_n <- ifelse(tmhvi1_n == 0, "[0]", 
                                                   ifelse(tmhvi1_n > 0 & tmhvi1_n <= 5, "(0-5]", 
                                                          ifelse(tmhvi1_n > 5 & tmhvi1_n <= 10, "(5-10]", 
                                                                 ifelse(tmhvi1_n > 10 & tmhvi1_n <= 15, "(10-15]",
                                                                        ifelse(tmhvi1_n > 15 & tmhvi1_n <= 40, "(15-40]",
                                                                               ifelse(tmhvi1_n > 40, "(40-inf)", tmhvi1_n))))))
        rm(tmhvi1_n)
        
        #ttkbco_n
        ttkbco_n <- BCCA_TDP_DATA_PURE_POSTPAGO$ttkbco_n
        BCCA_TDP_DATA_PURE_POSTPAGO$ttkbco_n <- ifelse(ttkbco_n > 0, "si", "no")
        rm(ttkbco_n)
        
        #tkbco1_n
        tkbco1_n <- BCCA_TDP_DATA_PURE_POSTPAGO$tkbco1_n
        BCCA_TDP_DATA_PURE_POSTPAGO$tkbco1_n <- ifelse(tkbco1_n > 0, "si", "no")
        rm(tkbco1_n)
        
        #tsmsen_n
        tsmsen_n <- BCCA_TDP_DATA_PURE_POSTPAGO$tsmsen_n
        BCCA_TDP_DATA_PURE_POSTPAGO$tsmsen_n <- ifelse(tsmsen_n == 0, "[0]", 
                                                      ifelse(tsmsen_n > 0 & tsmsen_n <= 50, "(0-50]", 
                                                             ifelse(tsmsen_n > 50 & tsmsen_n <= 150, "(50-150]",
                                                                    ifelse(tsmsen_n > 150 & tsmsen_n <= 300, "(150-300]", 
                                                                           ifelse(tsmsen_n > 300 & tsmsen_n <= 700, "(300-700]", 
                                                                                  ifelse(tsmsen_n > 700, "(700-inf)", tsmsen_n))))))
        rm(tsmsen_n)
        
        #tsmse1_n
        tsmse1_n <- BCCA_TDP_DATA_PURE_POSTPAGO$tsmse1_n
        BCCA_TDP_DATA_PURE_POSTPAGO$tsmse1_n <- ifelse(tsmse1_n == 0, "[0]", 
                                                   ifelse(tsmse1_n > 0 & tsmse1_n <= 25, "(0-25]",
                                                          ifelse(tsmse1_n > 25 & tsmse1_n <= 100, "(25-100]", 
                                                                 ifelse(tsmse1_n > 100, "(100-inf)", tsmse1_n))))
        rm(tsmse1_n)
        
        #ttcall_n
        ttcall_n <- BCCA_TDP_DATA_PURE_POSTPAGO$ttcall_n
        BCCA_TDP_DATA_PURE_POSTPAGO$ttcall_n <- ifelse(ttcall_n > 0, "si", "no")
        rm(ttcall_n)
        
        #tcall1_n
        tcall1_n <- BCCA_TDP_DATA_PURE_POSTPAGO$tcall1_n
        BCCA_TDP_DATA_PURE_POSTPAGO$tcall1_n <- ifelse(tcall1_n > 0, "si", "no")
        rm(tcall1_n)
        
        
        #tcmlpl_n
        tcmlpl_n <- BCCA_TDP_DATA_PURE_POSTPAGO$tcmlpl_n
        BCCA_TDP_DATA_PURE_POSTPAGO$tcmlpl_n <- ifelse(tcmlpl_n == 0, "[0]", 
                                                   ifelse(tcmlpl_n > 0 & tcmlpl_n <= 4000, "limitado",
                                                          ifelse(tcmlpl_n > 4000, "ilimitado", tcmlpl_n)))
        rm(tcmlpl_n)
        
        #tckbpl_n
        tckbpl_n <- BCCA_TDP_DATA_PURE_POSTPAGO$tckbpl_n
        BCCA_TDP_DATA_PURE_POSTPAGO$tckbpl_n <- ifelse(tckbpl_n == 0, "[0]", 
                                                   ifelse(tckbpl_n > 0 & tckbpl_n <= 20000000, "limitado",
                                                          ifelse(tckbpl_n > 20000000, "ilimitado", tckbpl_n)))
        rm(tckbpl_n)
        
        #tcsmsp_n
        tcsmsp_n <- BCCA_TDP_DATA_PURE_POSTPAGO$tcsmsp_n
        BCCA_TDP_DATA_PURE_POSTPAGO$tcsmsp_n <- ifelse(tcsmsp_n == 0, "[0]", 
                                                       ifelse(tcsmsp_n > 0 & tcsmsp_n <= 50, "(0-50]",
                                                              ifelse(tcsmsp_n > 50 & tcsmsp_n <= 200, "(50-200]",
                                                                     ifelse(tcsmsp_n > 200 & tcsmsp_n <= 900, "(200-900]",
                                                                        ifelse(tcsmsp_n > 900, ">900", tcsmsp_n)))))
        rm(tcsmsp_n)
        

        names(BCCA_TDP_DATA_PURE_POSTPAGO) <- c("vedadc_n","vantig_n","veanti_n","vtperm_c","vtrc11_n","vrc11f_n",
                                                "vrc11p_n","vtrc13_n","vrc13f_n","vrc13c_n","vtrc16_n","vrc16f_n",
                                                "vrc16c_n","vcplan_n","vcrequ_c","vctcpl_n","vccpno_n","vccpte_n",
                                                "vccdeu_n","vcclcv_n","vcclcs_n","vcclcd_n","vcsbaj_c","vtrp11_n",
                                                "vrp11f_n","vtrp13_n","vrp13f_n","vtrp16_n","vrp16f_n","vtrp21_n",
                                                "vrp21f_n","vtrp36_n","vrp36f_n","vtrp41_n","vrp41f_n","vtrp46_n",
                                                "vrp46f_n","ttmrcl_c","tmrcl1_c","ttmren_c","tmren1_c","ttmrvi_c",
                                                "tmrvi1_c","ttmhcl_c","tmhcl1_c","ttmhen_c","tmhen1_c","ttmhvi_c",
                                                "tmhvi1_c","ttkbco_c","tkbco1_c","tsmsen_c","tsmse1_c","ttcall_c",
                                                "tcall1_c","tcmlpl_c","tckbpl_c","tcsmsp_c","tmilmm_c","tmilmf_c",
                                                "tmilmo_c","tsmsil_c","ftexre_n","fmtexr_n","fcexr1_n","fmexr1_n",
                                                "fcexr3_n","fmexr3_n","fcexr6_n","fmexr6_n","figtdp_n","vtrec1_n",
                                                "vtrep1_n","vtrep2_n","vtrep3_n","vtrep4_n")
        
      }
      
      if(VECTOR_PRODUCTOS[3] %in% productos){
        producto_trama <- VECTOR_PRODUCTOS[3]
        BCCA_TDP_DATA_PURE_CONTROL<- subset(BCCA_TDP_DATA_PURE, BCCA_TDP_DATA_PURE$vprodu_c == "control")
        BCCA_TDP_DATA_PURE_CONTROL <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_CONTROL, c("vprodu_c"))
        flag_trama_prepago <- FALSE
        flag_trama_postpago <- FALSE
        flag_trama_control <- TRUE
        print("TRANSFORMANDO DATA CONTROL")
        
        #vedadc_n
        vedadc_n <- BCCA_TDP_DATA_PURE_CONTROL$vedadc_n
        BCCA_TDP_DATA_PURE_CONTROL$vedadc_n <- ifelse(vedadc_n > 70, round(mean(vedadc_n)), vedadc_n)
        rm(vedadc_n)
        
        #vcclcv_n
        vcclcv_n <- BCCA_TDP_DATA_PURE_CONTROL$vcclcv_n
        BCCA_TDP_DATA_PURE_CONTROL$vcclcv_n <- ifelse(vcclcv_n > 0, "si", "no")
        rm(vcclcv_n)
        
        #vcclcs_n
        vcclcs_n <- BCCA_TDP_DATA_PURE_CONTROL$vcclcs_n
        BCCA_TDP_DATA_PURE_CONTROL$vcclcs_n <- ifelse(vcclcs_n > 0, "si", "no")
        rm(vcclcs_n)
        
        #vcclcd_n
        vcclcd_n <- BCCA_TDP_DATA_PURE_CONTROL$vcclcd_n
        BCCA_TDP_DATA_PURE_CONTROL$vcclcd_n <- ifelse(vcclcd_n > 0, "si", "no")
        rm(vcclcd_n)
        
        #vcsbaj_n
        vcsbaj_n <- BCCA_TDP_DATA_PURE_CONTROL$vcsbaj_n
        BCCA_TDP_DATA_PURE_CONTROL$vcsbaj_n <- ifelse(vcsbaj_n > 0, "si", "no")
        rm(vcsbaj_n)
        
        #ttmrcl_n
        ttmrcl_n <- BCCA_TDP_DATA_PURE_CONTROL$ttmrcl_n
        BCCA_TDP_DATA_PURE_CONTROL$ttmrcl_n <- ifelse(ttmrcl_n > 0, "si", "no")
        rm(ttmrcl_n)
        
        #tmrcl1_n
        tmrcl1_n <- BCCA_TDP_DATA_PURE_CONTROL$tmrcl1_n
        BCCA_TDP_DATA_PURE_CONTROL$tmrcl1_n <- ifelse(tmrcl1_n > 0, "si", "no")
        rm(tmrcl1_n)
        
        #ttmren_n
        ttmren_n <- BCCA_TDP_DATA_PURE_CONTROL$ttmren_n
        BCCA_TDP_DATA_PURE_CONTROL$ttmren_n <- ifelse(ttmren_n > 0, "si", "no")
        rm(ttmren_n)
        
        #tmren1_n
        tmren1_n <- BCCA_TDP_DATA_PURE_CONTROL$tmren1_n
        BCCA_TDP_DATA_PURE_CONTROL$tmren1_n <- ifelse(tmren1_n > 0, "si", "no")
        rm(tmren1_n)
        
        #ttmrvi_n
        ttmrvi_n <- BCCA_TDP_DATA_PURE_CONTROL$ttmrvi_n
        BCCA_TDP_DATA_PURE_CONTROL$ttmrvi_n <- ifelse(ttmrvi_n > 0, "si", "no")
        rm(ttmrvi_n)
        
        #tmrvi1_n
        tmrvi1_n <- BCCA_TDP_DATA_PURE_CONTROL$tmrvi1_n
        BCCA_TDP_DATA_PURE_CONTROL$tmrvi1_n <- ifelse(tmrvi1_n > 0, "si", "no")
        rm(tmrvi1_n)
        
        
        #ttmhcl_n
        ttmhcl_n <- BCCA_TDP_DATA_PURE_CONTROL$ttmhcl_n
        BCCA_TDP_DATA_PURE_CONTROL$ttmhcl_n <- ifelse(ttmhcl_n == 0, "[0]", 
                                                      ifelse(ttmhcl_n > 0 & ttmhcl_n <= 50, "(0-50]", 
                                                             ifelse(ttmhcl_n > 50 & ttmhcl_n <= 100, "(50-100]", 
                                                                    ifelse(ttmhcl_n > 100 & ttmhcl_n <= 300, "(100-300]", 
                                                                           ifelse(ttmhcl_n > 300, "(300-inf)", ttmhcl_n)))))
        rm(ttmhcl_n)
        
        
        #tmhcl1_n
        tmhcl1_n <- BCCA_TDP_DATA_PURE_CONTROL$tmhcl1_n
        BCCA_TDP_DATA_PURE_CONTROL$tmhcl1_n <- ifelse(tmhcl1_n == 0, "[0]", 
                                                  ifelse(tmhcl1_n > 0 & tmhcl1_n <= 5, "(0-5]", 
                                                         ifelse(tmhcl1_n > 5 & tmhcl1_n <= 10, "(5-10]", 
                                                                ifelse(tmhcl1_n > 10 & tmhcl1_n <= 15, "(10-15]",
                                                                       ifelse(tmhcl1_n > 15 & tmhcl1_n <= 40, "(15-40]", 
                                                                              ifelse(tmhcl1_n > 40, "(40-inf)",tmhcl1_n))))))
        rm(tmhcl1_n)
        #ttmhen_n
        ttmhen_n <- BCCA_TDP_DATA_PURE_CONTROL$ttmhen_n
        BCCA_TDP_DATA_PURE_CONTROL$ttmhen_n <- ifelse(ttmhen_n == 0, "[0]", 
                                                      ifelse(ttmhen_n > 0 & ttmhen_n <= 50, "(0-50]", 
                                                             ifelse(ttmhen_n > 50 & ttmhen_n <= 100, "(50-100]", 
                                                                    ifelse(ttmhen_n > 100 & ttmhen_n <= 300, "(100-300]", 
                                                                           ifelse(ttmhen_n > 300, "(300-inf)", ttmhen_n)))))
        rm(ttmhen_n)
        

        #ttmhvi_n
        ttmhvi_n <- BCCA_TDP_DATA_PURE_CONTROL$ttmhvi_n
        BCCA_TDP_DATA_PURE_CONTROL$ttmhvi_n <- ifelse(ttmhvi_n == 0, "[0]", 
                                                      ifelse(ttmhvi_n > 0 & ttmhvi_n <= 50, "(0-50]", 
                                                             ifelse(ttmhvi_n > 50 & ttmhvi_n <= 100, "(50-100]", 
                                                                    ifelse(ttmhvi_n > 100 & ttmhvi_n <= 300, "(100-300]", 
                                                                           ifelse(ttmhvi_n > 300, "(300-inf)", ttmhvi_n)))))
        rm(ttmhvi_n)
        
        #tmhvi1_n
        tmhvi1_n <- BCCA_TDP_DATA_PURE_CONTROL$tmhvi1_n
        BCCA_TDP_DATA_PURE_CONTROL$tmhvi1_n <- ifelse(tmhvi1_n == 0, "[0]", 
                                                       ifelse(tmhvi1_n > 0 & tmhvi1_n <= 5, "(0-5]", 
                                                              ifelse(tmhvi1_n > 5 & tmhvi1_n <= 10, "(5-10]", 
                                                                     ifelse(tmhvi1_n > 10 & tmhvi1_n <= 15, "(10-15]",
                                                                            ifelse(tmhvi1_n > 15 & tmhvi1_n <= 40, "(15-40]",
                                                                                   ifelse(tmhvi1_n > 40, "(40-inf)", tmhvi1_n))))))
        rm(tmhvi1_n)
        
        #tmhen1_n
        tmhen1_n <- BCCA_TDP_DATA_PURE_CONTROL$tmhen1_n
        BCCA_TDP_DATA_PURE_CONTROL$tmhen1_n <- ifelse(tmhen1_n == 0, "[0]", 
                                                  ifelse(tmhen1_n > 0 & tmhen1_n <= 5, "(0-5]", 
                                                         ifelse(tmhen1_n > 5 & tmhen1_n <= 10, "(5-10]", 
                                                                ifelse(tmhen1_n > 10 & tmhen1_n <= 15, "(10-15]",
                                                                       ifelse(tmhen1_n > 15 & tmhen1_n <= 40, "(15-40]", 
                                                                              ifelse(tmhen1_n > 40, "(40-inf)",tmhen1_n))))))
        rm(tmhen1_n) 
        
        #tmhvi1_n
        tmhvi1_n <- BCCA_TDP_DATA_PURE_CONTROL$tmhvi1_n
        BCCA_TDP_DATA_PURE_CONTROL$tmhvi1_n <- ifelse(tmhvi1_n == 0, "[0]", 
                                                  ifelse(tmhvi1_n > 0 & tmhvi1_n <= 5, "(0-5]", 
                                                         ifelse(tmhvi1_n > 5 & tmhvi1_n <= 10, "(5-10]", 
                                                                ifelse(tmhvi1_n > 10 & tmhvi1_n <= 15, "(10-15]",
                                                                       ifelse(tmhvi1_n > 15 & tmhvi1_n <= 40, "(15-40]", 
                                                                              ifelse(tmhvi1_n > 40, "(40-inf)",tmhvi1_n))))))
        rm(tmhvi1_n) 
        
        
        #ttkbco_n
        ttkbco_n <- BCCA_TDP_DATA_PURE_CONTROL$ttkbco_n
        BCCA_TDP_DATA_PURE_CONTROL$ttkbco_n <- ifelse(ttkbco_n > 0, "si", "no")
        rm(ttkbco_n)
        
        #tkbco1_n
        tkbco1_n <- BCCA_TDP_DATA_PURE_CONTROL$tkbco1_n
        BCCA_TDP_DATA_PURE_CONTROL$tkbco1_n <- ifelse(tkbco1_n > 0, "si", "no")
        rm(tkbco1_n)
        
        #tsmsen_n
        tsmsen_n <- BCCA_TDP_DATA_PURE_CONTROL$tsmsen_n
        BCCA_TDP_DATA_PURE_CONTROL$tsmsen_n <- ifelse(tsmsen_n == 0, "[0]", 
                                                      ifelse(tsmsen_n > 0 & tsmsen_n <= 50, "(0-50]", 
                                                             ifelse(tsmsen_n > 50 & tsmsen_n <= 150, "(50-150]",
                                                                    ifelse(tsmsen_n > 150 & tsmsen_n <= 300, "(150-300]", 
                                                                           ifelse(tsmsen_n > 300 & tsmsen_n <= 700, "(300-700]", 
                                                                                  ifelse(tsmsen_n > 700, "(700-inf)", tsmsen_n))))))
        
        #tsmse1_n
        tsmse1_n <- BCCA_TDP_DATA_PURE_CONTROL$tsmse1_n
        BCCA_TDP_DATA_PURE_CONTROL$tsmse1_n <- ifelse(tsmse1_n == 0, "[0]", 
                                                  ifelse(tsmse1_n > 0 & tsmse1_n <= 25, "(0-25]",
                                                         ifelse(tsmse1_n > 25 & tsmse1_n <= 100, "(25-100]", 
                                                                ifelse(tsmse1_n > 100, "(100-inf)", tsmse1_n))))
        rm(tsmse1_n)
        
        #ttcall_n
        ttcall_n <- BCCA_TDP_DATA_PURE_CONTROL$ttcall_n
        BCCA_TDP_DATA_PURE_CONTROL$ttcall_n <- ifelse(ttcall_n > 0, "si", "no")
        rm(ttcall_n)
        
        #tcall1_n
        tcall1_n <- BCCA_TDP_DATA_PURE_CONTROL$tcall1_n
        BCCA_TDP_DATA_PURE_CONTROL$tcall1_n <- ifelse(tcall1_n > 0, "si", "no")
        rm(tcall1_n)
        
        #tcmlpl_n
        tcmlpl_n <- BCCA_TDP_DATA_PURE_CONTROL$tcmlpl_n
        BCCA_TDP_DATA_PURE_CONTROL$tcmlpl_n <- ifelse(tcmlpl_n == 0, "[0]", 
                                                  ifelse(tcmlpl_n > 0 & tcmlpl_n <= 4000, "limitado",
                                                         ifelse(tcmlpl_n > 4000, "ilimitado", tcmlpl_n)))
        rm(tcmlpl_n)
        
        #tckbpl_n
        tckbpl_n <- BCCA_TDP_DATA_PURE_CONTROL$tckbpl_n
        BCCA_TDP_DATA_PURE_CONTROL$tckbpl_n <- ifelse(tckbpl_n == 0, "[0]", 
                                                  ifelse(tckbpl_n > 0 & tckbpl_n <= 20000000, "limitado",
                                                         ifelse(tckbpl_n > 20000000, "ilimitado", tckbpl_n)))
        rm(tckbpl_n)
        
        #tcsmsp_n
        tcsmsp_n <- BCCA_TDP_DATA_PURE_CONTROL$tcsmsp_n
        BCCA_TDP_DATA_PURE_CONTROL$tcsmsp_n <- ifelse(tcsmsp_n == 0, "[0]", 
                                                       ifelse(tcsmsp_n > 0 & tcsmsp_n <= 50, "(0-50]",
                                                              ifelse(tcsmsp_n > 50 & tcsmsp_n <= 200, "(50-200]",
                                                                     ifelse(tcsmsp_n > 200 & tcsmsp_n <= 900, "(200-900]",
                                                                            ifelse(tcsmsp_n > 900, ">900", tcsmsp_n)))))
        
        names(BCCA_TDP_DATA_PURE_CONTROL) <-  c("vedadc_n"     , "vantig_n"   , "veanti_n"  , "vtperm_c"  , "vtrc11_n"   ,"vrc11f_n"  , "rec1pr_n"  , "vrc11p_n"   ,
                                                "vtrc16_n" ,"vrc16f_n", "vrc16c_n", "rec3to_n"  , "vcplan_n"  , "vcrequ_c"   ,"vctcpl_n"  , "vccpno_n"  , "vccpte_n"   ,
                                                "vccdeu_n"   ,"cntctl_c"  , "cntctl_1_c", "cntctl_2_c", "cntsol_c"  , "vtrp11_n"  , "vrp11f_n"  , "vtrp13_n" ,
                                                "vrp13f_n" ,"vtrp16_n", "vrp16f_n", "vtrp21_n"  , "vrp21f_n"  , "rep2to_1_n" ,"rep2fi_1_n", "rep2to_2_n", "rep2fi_2_n" ,
                                                "vtrp36_n" ,"vrp36f_n", "vtrp41_n"  , "vrp41f_n"  , "vtrp46_n", "vrp46f_n" ,"fld041_c"  , "fld042_c"  , "fld045_c"   ,
                                                "fld046_c"   ,"fld049_c"  , "fld050_c"  , "fld053_c"  , "fld054_c"  , "ttmhen_c"   ,"fld058_c"  , "fld061_c"  , "fld062_c"   ,
                                                "fld065_c"   ,"fld066_c"  , "fld069_c"  , "fld070_c"  , "fld073_c"  , "fld074_c"   ,"fld115_c"  , "fld116_c"  , "fld117_c"   ,
                                                "tmilmm_c"   ,"tmilmf_c"  , "tmilmo_c"  , "tsmsil_c"  , "ftexre_n"  , "fmtexr_n"   ,"fcexr1_n"  , "fmexr1_n"  , "fcexr3_n"   ,
                                                "fmexr3_n"   ,"fcexr6_n"  , "fmexr6_n"  , "figtdp_n"  , "vtrec1_n", "rec2to_3_n" ,"vtrep1_n", "vtrep2_n", "vtrep3_n" ,
                                                "vtrep4_n")
      }
      
      
      
      
      
      
      
      
      
      print("EXPORTANDO DATA")
      
      switch(opcion, 
             B = {
               #Caso 1: Se ingresó el path destino: opcion = "batch"
               #Genera excel:
               vector_resultante <- c("","")
               

               if(substr(path_destino,nchar(path_destino),nchar(path_destino)) == "/"){
                 if(flag_trama_prepago){
                   #BCCA_TDP_DATA_PURE_PREPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_PREPAGO, c("target_c"))
                   path_destino_prepago <- paste(path_destino,NOMBRE_DATASET,toupper(VECTOR_PRODUCTOS[1]),EXTENSION_ARCHIVO_DESTINO, sep = "") 
                   vector_resultante[1] <- VECTOR_PRODUCTOS[1]
                   vector_resultante[2] <- path_destino_prepago
                   write.csv(BCCA_TDP_DATA_PURE_PREPAGO, path_destino_prepago, row.names = FALSE)
                 }
                 if(flag_trama_postpago){
                   #BCCA_TDP_DATA_PURE_POSTPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_POSTPAGO, c("target_c"))
                   path_destino_postpago <- paste(path_destino,NOMBRE_DATASET,toupper(VECTOR_PRODUCTOS[2]),EXTENSION_ARCHIVO_DESTINO, sep = "") 
                   vector_resultante[1] <- VECTOR_PRODUCTOS[2]
                   vector_resultante[2] <- path_destino_postpago
                   write.csv(BCCA_TDP_DATA_PURE_POSTPAGO, path_destino_postpago, row.names = FALSE)
                 }
                 if(flag_trama_control){
                   #BCCA_TDP_DATA_PURE_CONTROL <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_CONTROL, c("target_c"))
                   path_destino_control <- paste(path_destino,NOMBRE_DATASET,toupper(VECTOR_PRODUCTOS[3]),EXTENSION_ARCHIVO_DESTINO, sep = "") 
                   vector_resultante[1] <- VECTOR_PRODUCTOS[3]
                   vector_resultante[2] <- path_destino_control
                   write.csv(BCCA_TDP_DATA_PURE_CONTROL, path_destino_control, row.names = FALSE)
                 }
               }else{
                 if(flag_trama_prepago){
                   #BCCA_TDP_DATA_PURE_PREPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_PREPAGO, c("target_c"))
                   path_destino_prepago <- paste(path_destino,"/",NOMBRE_DATASET,toupper(VECTOR_PRODUCTOS[1]),EXTENSION_ARCHIVO_DESTINO, sep = "") 
                   vector_resultante[1] <- VECTOR_PRODUCTOS[1]
                   vector_resultante[2] <- path_destino_prepago
                   write.csv(BCCA_TDP_DATA_PURE_PREPAGO, path_destino_prepago, row.names = FALSE)
                 }
                 if(flag_trama_postpago){
                   #BCCA_TDP_DATA_PURE_POSTPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_POSTPAGO, c("target_c"))
                   path_destino_postpago <- paste(path_destino,"/",NOMBRE_DATASET,toupper(VECTOR_PRODUCTOS[2]),EXTENSION_ARCHIVO_DESTINO, sep = "") 
                   vector_resultante[1] <- VECTOR_PRODUCTOS[2]
                   vector_resultante[2] <- path_destino_postpago
                   write.csv(BCCA_TDP_DATA_PURE_POSTPAGO, path_destino_postpago, row.names = FALSE)
                 }
                 if(flag_trama_control){
                   #BCCA_TDP_DATA_PURE_CONTROL <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_CONTROL, c("target_c"))
                   path_destino_control <- paste(path_destino,"/",NOMBRE_DATASET,toupper(VECTOR_PRODUCTOS[3]),EXTENSION_ARCHIVO_DESTINO, sep = "")
                   vector_resultante[1] <- unlist(VECTOR_PRODUCTOS[3])
                   vector_resultante[2] <- unlist(path_destino_control)
                   write.csv(BCCA_TDP_DATA_PURE_CONTROL, path_destino_control, row.names = FALSE)
                 }
                 
               }
               
               
               #Llenando output
               lista_output[[1]] <- "OK"
               lista_output[[3]] <- vector_resultante

               return(lista_output)
               
             },
             
             O = {
               #Caso 2: Se ingresó la trama opcion = "batch"    
               #Muestra en consola
               #print(BCCA_TDP_DATA_PURE_PREPAGO)
               if(flag_trama_prepago){
                 #BCCA_TDP_DATA_PURE_PREPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_PREPAGO, c("target_c"))
                 trama_transformada <- do.call(paste, c(BCCA_TDP_DATA_PURE_PREPAGO, sep = token))
               }else if(flag_trama_postpago){
                 #BCCA_TDP_DATA_PURE_POSTPAGO <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_POSTPAGO, c("target_c"))
                 trama_transformada <- do.call(paste, c(BCCA_TDP_DATA_PURE_POSTPAGO, sep = token))
               }else if(flag_trama_control){
                 #BCCA_TDP_DATA_PURE_CONTROL <- eliminaCaracteristica(BCCA_TDP_DATA_PURE_CONTROL, c("target_c"))
                 trama_transformada <- do.call(paste, c(BCCA_TDP_DATA_PURE_CONTROL, sep = token))
               }
               
               
               lista_output[[1]] <- "OK"
               lista_output[[2]] <- trama_transformada
               
               
               return(lista_output)
             },
             
             {
               stop("OPCION INVÁLIDA")
               
             })   
      
      
    },error=function(cond) {
      message <- as.character(cond)
      return(message)
    }#, warning=function(cond) {
    #  message <- as.character(cond)
    #  return(paste(message," -"))
    #}
    
    
  ) 
  lista_output[1] <- out
  
  return(lista_output)
  
}