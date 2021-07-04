
library(dplyr)

options(scipen = 999)

# ALGORITMOS DE DETECCION DE DATOS ATIPICOS

datos <- read.csv2("C:\\Users\\camil\\Desktop\\CURSO\\DATOS\\data.csv", stringsAsFactors = FALSE)
datosf <- subset(datos, select = c("ID_MUESTRA","COD_ESTACION","REGION_DES","TEMPORADA_MS_DES","SUSTRATO_MS_DES","SUS_ESTACION_DES",
                 "CODIGO_VARIABLE_EV_DES","UNIDADES_EV_DES","CONCENTRACION_EV"))
datosf <- datosf[datosf$TEMPORADA_MS_DES == "Seca" & datosf$REGION_DES == "Caribe" & datosf$SUS_ESTACION_DES == "Agua Marina",]
datosf <- datosf[datosf$CODIGO_VARIABLE_EV_DES == "Temperatura",]

freq <- datosf %>% group_by(COD_ESTACION) %>% dplyr::summarise(Freq = n())
datosf <- datosf[datosf$COD_ESTACION %in% c("C47003006","C47003015","C47003016","C47003022","C47003020","C47003008"),]

# METODOS PARAMETRICOS

hist(datosf$CONCENTRACION_EV)

"METODO IQR"

iqrMethod <- function(x){
  
  estaciones <<- unique(x$COD_ESTACION)
  iqrDf <<- data.frame(matrix(NA, nrow = 6, ncol = 6))  
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

#  DETECCION POR LA MEDIA

detect_outliers_mean <- function(x){          
  
  processed_data <<- x %>% group_by(COD_ESTACION) %>% 
    summarise(FREQ = n(), STD = sd(CONCENTRACION_EV), MEAN = mean(CONCENTRACION_EV), .groups = 'drop')
  
  processed_data <<- mutate(processed_data, LIM_INF = MEAN - 3*STD, LIM_SUP = MEAN + 3*STD) 
  processed_data$LIM_INF[processed_data$LIM_INF < 0] <- 0 
  
  datos_redcam <<- merge(x, y = subset(processed_data, select = -c(FREQ,MEAN,STD)), 
                         by = "COD_ESTACION", all.datos_redcam = TRUE)
  
  valids <<- datos_redcam[(datos_redcam$LIM_INF <= datos_redcam$CONCENTRACION_EV & datos_redcam$CONCENTRACION_EV <= datos_redcam$LIM_SUP),]
  outliers <<- datos_redcam[(datos_redcam$CONCENTRACION_EV < datos_redcam$LIM_INF | datos_redcam$CONCENTRACION_EV > datos_redcam$LIM_SUP),]
  return(list(Valids = valids, Outliers = outliers))
}

MEAN.result <- detect_outliers_mean(datosf)

# DETECCION POR LA MEDIANA

detect_outliers_median <- function(x){          
  
  processed_data <<- x %>% group_by(COD_ESTACION) %>% 
    summarise(FREQ = n(), MAD = mad(CONCENTRACION_EV), MEDIAN = median(CONCENTRACION_EV), .groups = 'drop')
  
  processed_data <<- mutate(processed_data, LIM_INF = MEDIAN - 3*MAD, LIM_SUP = MEDIAN + 3*MAD) 
  processed_data$LIM_INF[processed_data$LIM_INF < 0] <- 0 
  
  datos_redcam <<- merge(x, y = subset(processed_data, select = -c(FREQ,MEDIAN,MAD)), 
                         by = "COD_ESTACION", all.datos_redcam = TRUE)
  
  valids <<- datos_redcam[(datos_redcam$LIM_INF <= datos_redcam$CONCENTRACION_EV & datos_redcam$CONCENTRACION_EV <= datos_redcam$LIM_SUP),]
  outliers <<- datos_redcam[(datos_redcam$CONCENTRACION_EV < datos_redcam$LIM_INF | datos_redcam$CONCENTRACION_EV > datos_redcam$LIM_SUP),]
  return(list(Valids = valids, Outliers = outliers))
}

MEDIAN.result <- detect_outliers_median(datosf)

# METODOS NO PARAMETRICOS

# NO SUPERVISADOS

# ISOLATION FOREST

new_data <- subset(datos, select = c("ID_MUESTREO","COD_ESTACION","REGION_DES","TEMPORADA_MS_DES","SUSTRATO_MS_DES","SUS_ESTACION_DES",
                                       "CODIGO_VARIABLE_EV_DES","CONCENTRACION_EV"))
new_data <- new_data[new_data$TEMPORADA_MS_DES == "Seca" & new_data$REGION_DES == "Caribe" & new_data$SUS_ESTACION_DES == "Agua Marina",]
new_data <- new_data[new_data$CODIGO_VARIABLE_EV_DES %in% c("Temperatura","Salinidad","pH"),]
new_data <- reshape(new_data, idvar = c("ID_MUESTREO","COD_ESTACION","REGION_DES","TEMPORADA_MS_DES",
                                            "SUSTRATO_MS_DES","SUS_ESTACION_DES"), timevar = "CODIGO_VARIABLE_EV_DES", direction = "wide")
new_data <- new_data[complete.cases(new_data),]

library(solitude) 

detect_outliers_isoforest <- function(x){   #   DETECCION A TRAVES DEL ALGORITMO DE ISOLATION FOREST

  iso <<- isolationForest$new(sample_size = length(x), respect_unordered_factors = "ignore")
  iso$fit(x)
  x$predict <- iso$predict(x)
  x$etiqueta <- ifelse(x$predict$anomaly_score >= 0.60, "outlier", "normal")
  x <- subset(x, select = -c(predict))
  valids <- x[x$etiqueta == "normal",]
  outliers <- x[x$etiqueta == "outlier",]
  
  return(list(Valids = valids, Outliers = outliers))
}

ISO.result <- detect_outliers_isoforest(new_data)

# DBSCAN

library(dbscan)

dbScanData <- new_data
pivotData <- as.matrix(dbScanData[7:9])
kNNdistplot(pivotData, k = 10)  
dbscan.result <- dbscan(pivotData, eps= 2.5, minPts=10)
hullplot(pivotData, dbscan.result$cluster)
dbScanData$cluster <- dbscan.result$cluster 
dbScanData[dbScanData$cluster == 0,]

# LOF

"DBSCAN"
lof.result <- dbscan::lof(pivotData, minPts = 5)
lof.outliers <- lof.result[lof.result > 1.2]
lof.outliers

"Rlof"
library(Rlof)
outlier.scores <- Rlof::lof(pivotData, k=c(5:10))
outlier.scores


