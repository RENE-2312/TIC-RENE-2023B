# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modelo Regresion Logistica %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este script se utiliza una regresión logística para abordar el problema de credit scoring, evaluando el rendimiento del modelo en
# dos conjuntos de datos distintos: uno para entrenar el modelo y otro para validar sus resultados. Para entrenar el modelo se realizó un 
# rebalanceo a la base de modelamiento para tener una proporción de 80%-20% de buenos y malos, a esta base se la denomina rmod.

# Las tablas performance se almacenan en un archivo de Excel, Tablas_Performance_RGL.xlsx, en la carpeta de resultados, e incluyen 
# métricas como KS, ROC y GINI  para cada modelo-base.

# Los resultados de la regresión logística se guardan en un archivo RData, InfoRGL.RData, ubicado en la subcarpeta RData de la carpeta BDD.

# El modelo ajusta la probabilidad de ser un "Mal Pagador", mientras que el Score en las tablas performance representa la probabilidad de
# ser un "Buen Pagador". Por lo tanto, una probabilidad alta en el modelo indica un posible mal pagador, mientras que un alto SCORE en las 
# tablas performance sugiere un buen pagador.

# Para ajustar el modelo, se utilizó la técnica de stepwise para seleccionar las variables más representativas en la identificación 
# de malos pagadores, en base a las variables creadas en el script 201_tratamiento_identificacion_final.R, Una vez identificadas las 
# variables mediante Stepwise, se seleccionaron aquellas menos correlacionadas, así como aquellas que proporcionaban información sobre
# deuda, operaciones, entidades, acreedores, días de vencimiento, características idiosincráticas y estados financieros para los diferentes
# sistemas de crédito del país. También se tuvieron en cuenta las variables por su signo y su contribución para el modelo final.

# ¡¡¡¡¡¡Aviso importante!!!!!

# No es necesario ejecutar este script de forma independiente. Se puede ejecutar desde el script 001_Ejecutar_Proyecto.R 
# para asegurar una ejecución ordenada y completa del proyecto.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\t\t\t\t\tModelo Regresion Logistica' )
message( paste( rep( '-', 100 ), collapse = '' ) )

#--------------------------------------------------------- Direcciones y Funciones --------------------------------------------------------

# Saca los deciles del Score y categoriza cada intervalo
rango_score <- function(vector){
  index <- aux <- seq(1:length(vector))
  res <- data.frame(id=index, val=vector)
  res <- res[order(res$val, decreasing = TRUE),]
  res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 11),0), labels = seq(1,10))
  res <- res[order(res$id),]
  return(as.numeric(res$aux))
}

#---------------------------------------------------------- Lectura Resultados RGL ------------------------------------------------------------

options(scipen = 999)

# Direccion de la data Consolidada (Buró + Institución)
dir.b <- paste0(dir.p, "/BDD/RData")
setwd(dir.b)
list.files()
#~~~~~~~~~~~~~~~~~~~ Lectura información:
load("InfoRGL.RData")

# Base de modelizacion
mod <- datos[ModVal == 0 ]
mod$Rango_RGL <- rango_score(mod$SCORE_RGL)

setwd(dir.r)
r_malo_int<-read_excel("Tablas_Performance_RGL.xlsx",sheet = "Modelo-Base-Modelizacion", range = "I20:I29",col_names = FALSE)
r_malo_int<-r_malo_int$...1
mod$r_malo_int_RGL<-r_malo_int[mod$Rango_RGL]


r_malo_cum<-read_excel("Tablas_Performance_RGL.xlsx",sheet = "Modelo-Base-Modelizacion",  range = "J20:J29",col_names = FALSE)
r_malo_cum<-r_malo_cum$...1
mod$r_malo_cum_RGL<-r_malo_cum[mod$Rango_RGL]

# Base de validacion
val <- datos[ModVal == 1 ]
val$Rango_RGL <- rango_score(val$SCORE_RGL)

setwd(dir.r)
r_malo_int<-read_excel("Tablas_Performance_RGL.xlsx",sheet = "Modelo-Base-Validacion",  range = "I20:I29",col_names = FALSE)
r_malo_int<-r_malo_int$...1
val$r_malo_int_RGL<-r_malo_int[val$Rango_RGL]

r_malo_cum<-read_excel("Tablas_Performance_RGL.xlsx",sheet = "Modelo-Base-Validacion",  range = "J20:J29",col_names = FALSE)
r_malo_cum<-r_malo_cum$...1
val$r_malo_cum_RGL<-r_malo_cum[val$Rango_RGL]

info<-rbind(mod,val)

resultados<-info|>select(ModVal,SCORE_RGL,Rango_RGL,r_malo_int_RGL,r_malo_cum_RGL)


#---------------------------------------------------------- Lectura Resultados RF ------------------------------------------------------------

options(scipen = 999)

# Direccion de la data Consolidada (Buró + Institución)
dir.b <- paste0(dir.p, "/BDD/RData")
setwd(dir.b)
list.files()
#~~~~~~~~~~~~~~~~~~~ Lectura información:
load("InfoRF_VC.RData")

# Base de modelizacion
mod <- datos[ModVal == 0 ]
mod$Rango_RF <- rango_score(mod$SCORE_RF)

setwd(dir.r)
r_malo_int<-read_excel("Tablas_Performance_RF_Vc.xlsx",sheet = "Modelo-Base-Modelizacion",  range = "I20:I29",col_names = FALSE)
r_malo_int<-r_malo_int$...1
mod$r_malo_int_RF<-r_malo_int[mod$Rango_RF]

r_malo_cum<-read_excel("Tablas_Performance_RF_VC.xlsx",sheet = "Modelo-Base-Modelizacion",  range = "J20:J29",col_names = FALSE)
r_malo_cum<-r_malo_cum$...1
mod$r_malo_cum_RF<-r_malo_cum[mod$Rango_RF]

# Base de validacion
val <- datos[ModVal == 1 ]
val$Rango_RF <- rango_score(val$SCORE_RF)

setwd(dir.r)
r_malo_int<-read_excel("Tablas_Performance_RF_VC.xlsx",sheet = "Modelo-Base-Validacion",  range = "I20:I29",col_names = FALSE)
r_malo_int<-r_malo_int$...1
val$r_malo_int_RF<-r_malo_int[val$Rango_RF]

r_malo_cum<-read_excel("Tablas_Performance_RF_VC.xlsx",sheet = "Modelo-Base-Validacion",  range = "J20:J29",col_names = FALSE)
r_malo_cum<-r_malo_cum$...1
val$r_malo_cum_RF<-r_malo_cum[val$Rango_RF]

info<-rbind(mod,val)

resultados<-cbind(resultados,info|>select(SCORE_RF,Rango_RF,r_malo_int_RF,r_malo_cum_RF))

#---------------------------------------------------------- Lectura Resultados XGB  ------------------------------------------------------------

options(scipen = 999)

# Direccion de la data Consolidada (Buró + Institución)
dir.b <- paste0(dir.p, "/BDD/RData")
setwd(dir.b)
list.files()
#~~~~~~~~~~~~~~~~~~~ Lectura información:
load("InfoXGB.RData")

# Base de modelizacion
mod <- datos[ModVal == 0 ]
mod$Rango_XGB <- rango_score(mod$SCORE_XGB)

setwd(dir.r)
r_malo_int<-read_excel("Tablas_Performance_XGB.xlsx",sheet = "Modelo-Base-Modelizacion",  range = "I20:I29",col_names = FALSE)
r_malo_int<-r_malo_int$...1
mod$r_malo_int_XGB<-r_malo_int[mod$Rango_XGB]

r_malo_cum<-read_excel("Tablas_Performance_XGB.xlsx",sheet = "Modelo-Base-Modelizacion",  range = "J20:J29",col_names = FALSE)
r_malo_cum<-r_malo_cum$...1
mod$r_malo_cum_XGB<-r_malo_cum[mod$Rango_XGB]

# Base de validacion
val <- datos[ModVal == 1 ]
val$Rango_XGB <- rango_score(val$SCORE_XGB)

setwd(dir.r)
r_malo_int<-read_excel("Tablas_Performance_XGB.xlsx",sheet = "Modelo-Base-Validacion",  range = "I20:I29",col_names = FALSE)
r_malo_int<-r_malo_int$...1
val$r_malo_int_XGB<-r_malo_int[val$Rango_XGB]

r_malo_cum<-read_excel("Tablas_Performance_XGB.xlsx",sheet = "Modelo-Base-Validacion",  range = "J20:J29",col_names = FALSE)
r_malo_cum<-r_malo_cum$...1
val$r_malo_cum_XGB<-r_malo_cum[val$Rango_XGB]

info<-rbind(mod,val)

resultados<-cbind(resultados,info|>select(SCORE_XGB,Rango_XGB,r_malo_int_XGB,r_malo_cum_XGB))

resultados$EAD<-datos$VALOR_CREDITO
resultados$VarDep<-datos$VarDep
#----------------------------------------------------- LGD -------------------------------------------------------------
resultados[,LGD:=0.45]
#--------------------------------------------- Calculo Perdida Esperada  -------------------------------------------------------------------------

resultados[,PE_RGL:=EAD*LGD*r_malo_int_RGL]
resultados[,PE_RF:=EAD*LGD*r_malo_int_RF]
resultados[,PE_XGB:=EAD*LGD*r_malo_int_XGB]

resultados[,r_PE_RGL:=PE_RGL/EAD]
resultados[,r_PE_RF:=PE_RF/EAD]
resultados[,r_PE_XGB:=PE_XGB/EAD]

#-------------------------------------------------- Comparación --------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Modelización: RGL
mod <- resultados[ModVal == 0 & Rango_RGL==7]
r_PE<-sum(mod$PE_RGL)/sum(mod$EAD)
mod <- resultados[ModVal == 0 & Rango_RF==7]
r_PE<-sum(mod$PE_RF)/sum(mod$EAD)
mod <- resultados[ModVal == 0 & Rango_XGB==7]
r_PE<-sum(mod$PE_XGB)/sum(mod$EAD)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Validación: RGL
val <- resultados[ModVal == 1  & Rango_RGL==6]
r_PE<-sum(val$PE_RGL)/sum(mod$EAD)
val <- resultados[ModVal == 1  & Rango_RF==6]
r_PE<-sum(val$PE_RF)/sum(mod$EAD)
val <- resultados[ModVal == 1  & Rango_XGB==6]
r_PE<-sum(val$PE_XGB)/sum(mod$EAD)

#----------------------------------------------------- Almacenamiento de Resultados -----------------------------------------------------------------

# Se guarda la data generada paa este modelo
setwd(paste(dir.p,"BDD","RData",sep="/"))
save(list = c("resultados"), file = "Resultados_Malos.RData")


rm(list=setdiff(ls(),c("dir.p","dir.r","dir.s")))
gc()

# Se restaura la dirección de trabajo a la carpeta principal del proyecto
setwd(dir.p)

