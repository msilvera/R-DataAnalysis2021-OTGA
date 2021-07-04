library(dplyr)

#Agrego options para que no transforme a notacion científica los datos del csv
options(scipen = 999)

#EJERCICIO:  ALGORITMOS DE DETECCION DE DATOS ATIPICOS

filename = file.choose()
#[1] "C:\\Users\\msilvera\\Documents\\ONG\\OceanTeacher\\Modulo 2\\Ejercicio Imputacion\\datos_ejercicio-AtipicosImputacion.csv
#uso read.csv porque los datos están separados por comas
datos <- read.csv(filename, stringsAsFactors = FALSE)

#Me quedo con las columnas que me interesan
datosf <- subset(datos, select = c("ID_MUESTREO","COD_ESTACION","REGION_DES","SUS_ESTACION_DES",
                                   "CODIGO_VARIABLE_EV_DES","CONCENTRACION_EV"))
#A su vez, elijo los datos de Agua Estuarina de la region Caribe para analizarlos ya que son los que hay mas cantidad
datosf <- datosf[datosf$REGION_DES == "Caribe" & datosf$SUS_ESTACION_DES == "Agua Estuarina",]

#Me quedo solo con los registros de la variable Temperatura
datosf <- datosf[datosf$CODIGO_VARIABLE_EV_DES == "Temperatura",]

#Sobre los datos de temperaturas, veo cuales estaciones son las que tienen mas datos y me quedo con esos registros
#las estaciones seleccionadas son las que tienen màs de 300 registros cada una.
freq <- datosf %>% group_by(COD_ESTACION) %>% dplyr::summarise(Freq = n())
datosf <- datosf[datosf$COD_ESTACION %in% c("C47005032","C47005034","C47005033","C47005025","C47005038","C47005036","C47005040","C47005028","C47005054",
                                            "C47005045","C47005044","C47005043","C47005049","C47005039","C47005022","C47005026","C47005027"),]

#Valido la normalidad de los datos, para ver si es factible usar los metodos parametricos.
#Ajusto manualmente los valores por defecto de la funcion HIST porque sino los breaks no quedan bien y se ve mal el historgrama
h <- hist(datosf$CONCENTRACION_EV,breaks=seq(0,350,by=1), main='Histograma Ejercicio: Temperatura',xlim=c (20,45), ylim=c (0,1500))
#veo losparametros usados para la generacion del historgrama
h

#IMPLEMENTACION DEL METODO IQR

iqrMethod <- function(x){
  
  estaciones <<- unique(x$COD_ESTACION)
  #En este caso tengo 17 estaciones -> 17 filas
  iqrDf <<- data.frame(matrix(NA, nrow = 17, ncol = 6))  
  colnames(iqrDf) <<- c("CODEST", "Q1", "Q3", "IQR", "limitInf", "limitSup")
  iqrDf$CODEST <<- estaciones
  
  dfValids <- x[0,]
  dfOutliers <- x[0,]
  
  for(codEst in estaciones){
    
    iqrDatos <- datosf[datosf$COD_ESTACION == codEst,]
    Q <<- quantile(iqrDatos$CONCENTRACION_EV, probs=c(.25, .75), na.rm = FALSE)
    iqrV <<- IQR(iqrDatos$CONCENTRACION_EV)
    
    iqrDf$Q1[iqrDf$CODEST == codEst] <<- Q[1]
    iqrDf$Q3[iqrDf$CODEST == codEst] <<- Q[2]
    iqrDf$IQR[iqrDf$CODEST == codEst] <<- iqrV
    iqrDf$limitInf[iqrDf$CODEST == codEst] <<- Q[2] - 1.5*iqrV
    iqrDf$limitSup[iqrDf$CODEST == codEst] <<- Q[1] + 1.5*iqrV
    
    limitInf <- iqrDf$limitInf[iqrDf$CODEST == codEst]
    limitSup <- iqrDf$limitSup[iqrDf$CODEST == codEst]
    
    searchValids <- iqrDatos[iqrDatos$CONCENTRACION_EV >= limitInf & iqrDatos$CONCENTRACION_EV <= limitSup,]
    searchOutliers <- iqrDatos[iqrDatos$CONCENTRACION_EV < limitInf | iqrDatos$CONCENTRACION_EV > limitSup,]
    
    dfValids <- rbind(dfValids, searchValids)
    dfOutliers <- rbind(dfOutliers, searchOutliers)
  }
  
  return(list(Valids = dfValids, Outliers = dfOutliers))
}

IQR.result <- iqrMethod(datosf)
