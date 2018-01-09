path_origen <- "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA_DATA_PURE/FILTRADA LISTA PARA TRANSFORMAR/BCCA_TDP_DATA_CONTROL.csv"
path_destino <- "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/BCCA_DATA_PURE/FILTRADA LISTA PARA TRANSFORMAR/destino"

#Batch

lista_output <- transformacionDatasetChurn(path_origen, path_destino, "","","B")

lista_output


#Online

trama_prepago <- "prepago,65,106,22,0,0,0,5,0,0,0,0,0,0,0,0,0,0,17,17,0,0,0,0,171,56,19,4,6,0,49572,49572,35,1,0,0,N,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0"
trama_postpago <- "postpago,62,153,22,n,0,0,0,0,0,0,0,0,0,199.99,n,28,12,16,7,0,0,0,0,4276,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,396,0,1856,0,14,0,0,0,129,0,0,0,999999,999999999999999,999999,S,S,S,S,5,34.57,0,0,1,4.86,1,15.07,12371.75,0,0,0,0,0,1"
trama_control <- "control,31,118,30,n,0,0,0,0,0,0,0,0,144.9,n,26,13,13,1,0,0,0,5,4181,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,122,0,51,0,0,0,0,0,1,0,0,0,0,999999999999999,20,N,N,N,N,0,0,0,0,0,0,1,0,5258.62,0,0,0,0,0,0,1"

token <- ","

lista_output <- transformacionDatasetChurn("", "", trama_prepago, token,"O")
lista_output

lista_output <- transformacionDatasetChurn("", "", trama_postpago, token,"O")
lista_output

lista_output <- transformacionDatasetChurn("", "", trama_control, token,"O")
lista_output
