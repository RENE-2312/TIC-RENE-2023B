# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modelo XGBoost %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este script se utiliza un modelo de XGBoost para abordar el problema de credit scoring, evaluando el rendimiento del modelo en
# dos conjuntos de datos distintos: uno para entrenar el modelo y otro para validar sus resultados. Para entrenar el modelo se realizó un 
# rebalanceo a la base de modelamiento para tener una proporción de 80%-20% de buenos y malos, a esta base se la denomina rmod.

# Las tablas performance se almacenan en un archivo de Excel, Tablas_Performance_XGB.xlsx, en la carpeta de resultados, e incluyen 
# métricas como KS, ROC y GINI  para cada modelo-base.

# Los resultados del random forest se guardan en un archivo RData, InfoXGB.RData, ubicado en la subcarpeta RData de la carpeta BDD.

# Para ajustar el modelo, primero se seleccionaron las variables con mayor valor en las pruebas KS o VI. Se eligieron variables de diferente 
# indole (Idiosincráticas, Deuda, Operaciones, Días de Vencimiento, Entidades), en donde se seleccionaron variables sin una alta correlación.

# Los hiperparámetros de este modelo se seleccionaron tomando como referencia los hiperparámetros del modelo de random forest. 
# Se realizaron diversas pruebas para la selección de la tasa de aprenidzaje del modelo (eta) y se tomo aquel valor que redujo el sobreajuste
# del modelo.

# ¡¡¡¡¡¡Aviso importante!!!!!

# No es necesario ejecutar este script de forma independiente. Se puede ejecutar desde el script 001_Ejecutar_Proyecto.R 
# para asegurar una ejecución ordenada y completa del proyecto.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\t\t\t\t\tModelo XGBoost' )
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

# Funcion de rebalanceo 
rebalanceo <- function(data,inc_data,porc_malos,semilla){
  r_b <- which(data$VarDep==0)
  r_m <- which(data$VarDep==1)
  set.seed(semilla)
  N<-floor((1+inc_data)*nrow(data))
  N_m<-floor(porc_malos*N)
  r_b <- sample(r_b, size =N-N_m, replace = TRUE)
  r_m <- sample(r_m, size =N_m, replace = TRUE)
  r_r <- c(r_b,r_m)
  return(data[r_r,])
}

#------------------------------------------------------ Lectura de la base Modelamiento ------------------------------------------------------------

patron <- "\\InfoModelamiento\\b"
archivos_filtrados <- list.files(dir.p, pattern = patron, full.names = TRUE, recursive = TRUE)
load(archivos_filtrados)

#---------------------------------------------------------- Base de Modelamiento  ------------------------------------------------------------

# Filtramos la base de Modelamiento
mod <- datos[ModVal == 0 & VarDep %in% c(0,1)]
dim(mod)
mod[,table(VarDep)]
mod[,prop.table(table(VarDep))]

#----------------------------------------------------- Base Modelamiento Balanceada  ------------------------------------------------------------

rmod<-rebalanceo(mod,0.2,0.2,12345)
dim(rmod)
rmod[,table(VarDep)]
rmod[,prop.table(table(VarDep))]

#----------------------------------------------------- Lectura Plantilla Tablas Performance -------------------------------------------------------

#~~~~~~~~~~~~ Lectura de las tablas performance:

patron <- "\\Plantilla_Tablas_Performance\\b"
# Direccion del excel con la plantilla de las tablas performance:
dir.tpf <- list.files(dir.p, pattern = patron, full.names = TRUE, recursive = TRUE)

# Se carga la plantilla
plantilla<- loadWorkbook(dir.tpf)

#----------------------------------------------------------- Variables  -------------------------------------------------------

#~~~~~~~~~~~~~~~ Variables Iniciales:

#~~~~~~~~~~~~~~~~~~~~~ Mejor modelo 
variables_modelo<-c("prbm_PROVINCIA_ACTIVIDAD_DES","prbm_VIVIENDA",
                    "prbm_TOTAL_INGRESOS",
                    "MARCA_DVEN_SF_3M",
                    "MARCA_NENT_VENC_SB_24M",
                    "r_NUM_OPE_VIG_SF_12s36M","prbm_NOPE_VENC_SB_36M",
                    "prbb_DEUDA_VENCIDA_SCE_36M","prbm_DEUDA_TOTAL_OP_12M","r_DEUDA_TOTAL_SF_3a6M",
                    "prbm_NUM_ACREED_SB_3M")

#----------------------------------------------------------  Modelo XGB ---------------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Reestructuración de la data:
# Se pone en el formato adecuado a la información para la implementación del modelo

# Identificación Variable Dependiente
# (1) Malos, (0) Buenos
label<-rmod$VarDep

# Información de modelación
RMOD<-rmod[,..variables_modelo,drop = FALSE]
RMOD<-as.matrix(RMOD)

# Información de todos los individuos
DATOS<-datos[,..variables_modelo,drop = FALSE]
DATOS<-as.matrix(DATOS)

#--------------------------------------------------------- Validación Cruzada Grilla 1 ---------------------------------------------------------------------------

# vector representitividad  1, numero de individuos en la base balanceada
n1 <- dim(rmod)[1]

round(quantile(0:n1, probs = seq(0,0.1,by=0.01)),0)
#Vector con el porcentaje de presentatividad del 1% - 10%
representatividad_1 <- round(quantile(0:n1, probs = seq(0,0.1,by=0.01)),0)
representatividad_1<- as.data.frame(representatividad_1)
representatividad_1 <- representatividad_1[,1]
representatividad_1<- representatividad_1[5:11]
representatividad_1


#~~~~~~~~~~~~~~~ Grilla de Hiperparametros

grilla_xgb_1 <- expand_grid(
  'colsample_bytree'      = c(3/11),
  'min_node' = representatividad_1,
  'eta'=c(0.1,0.2,0.3)
)

# Aqui se almacenara el error para cada modelo
oob_min_score_xgb_1 <- rep(NA, nrow(grilla_xgb_1))
oob_best_iter_xgb_1<-rep(NA, nrow(grilla_xgb_1))
X <- as.matrix(RMOD)
y <- label
#~~~~~~~~~~~~~~~ Ajuste de los modelos 

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

for(i in 1:nrow(grilla_xgb_1)){

  set.seed(12345)
  # Configuración de parámetros
  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = grilla_xgb_1$eta[i],
    min_child_weight = grilla_xgb_1$min_node[i],
    colsample_bytree = grilla_xgb_1$colsample_bytree[i]
  )
  
  # Realizar validación cruzada
  cv_result <- xgb.cv(
    data = X,
    label = y,
    params = params,
    nrounds = 165,  # Número total de iteraciones
    nfold = 5,      # Número de pliegues en la validación cruzada (ajusta según tus necesidades)
    early_stopping_rounds = 50,
    maximize = FALSE
  )
  
  oob_best_iter_xgb_1[i]<-cv_result$best_iteration
  oob_min_score_xgb_1[i] <- as.numeric(cv_result$evaluation_log[cv_result$best_iteration,4])
}

stopCluster(cl)

#~~~~~~~~~~ Resultados:
resultados_xgb_1 <- data.table(grilla_xgb_1)
resultados_xgb_1[, iter := oob_best_iter_xgb_1]
resultados_xgb_1[, loglossmean := oob_min_score_xgb_1]
resultados_xgb_1[order(loglossmean)]


#------------------------------------------------ Mejor Modelo XGB ----------------------------------------------------

set.seed(12345)
bstDense <- xgboost(
  data = RMOD,
  label = label,
  eta = 0.2,#0.2
  nthread = 4,
  nrounds = 72,#72
  objective = "binary:logistic",
  min_child_weight = 346,#346
  early_stopping_rounds = 50,
  maximize = FALSE,  # Para minimizar la métrica de evaluación
  eval_metric = "logloss",
  colsample_bytree=3/11,
  lambda=1,
  tree_method="exact"
)


#---------------------------------------------------- Analisis de Correlación -------------------------------------------------------

#~~~~~~~~~~~~ Matriz de Correlacion:
res <- cor(setDT(rmod)[,variables_modelo, with=FALSE])
res

#~~~~~~~~~~~~ Graficos de la matriz de correlación
ggcorrplot(res, hc.order = TRUE, type = "upper",method = "circle",lab = TRUE)

correlacion<-ggcorrplot(res, hc.order = TRUE, type = "lower",outline.col = "white",
           lab = TRUE)

print(correlacion)
ggsave(paste0(dir.r,"/","grafico_correlacion_xgb.png"), plot = correlacion, width = 10, height = 8, units = "in", dpi = 300)

#~~~~~~~~~~~~ Análisis espectral de la matriz de autocorrelación
# Análisis de condicionamiento
sqrt(max(eigen(res)$values)/min(eigen(res)$values))
min(eigen(res)$values)
max(eigen(res)$values)
# como es menor a 5, esto nos indica que no hay autocorrelación

# ----------------------------------------------------- Importancia de las variables  --------------------------------------------------------------------
importancia_xgb <- xgb.importance(feature_names = colnames(RMOD), model = bstDense)
importancia_xgb
xgb.plot.importance(importance_matrix = importancia_xgb)

# Feature: Nombre de la variable.
# Gain: Importancia de la variable basada en la ganancia. Indica la contribución de la variable al modelo.
# Cover: Importancia de la variable basada en la cobertura. Mide la relativa cantidad de observaciones afectadas por la variable.
# Frequency: Importancia basada en la frecuencia. Indica la frecuencia relativa con la que se utiliza la variable en las divisiones de árboles.



importancia_var<-data.table(Predictor=importancia_xgb$Feature,Importancia=importancia_xgb$Gain)
variables_top<-head(importancia_var[order(Importancia,decreasing = TRUE)],15)

# Gráfico de Importancia de las Variables
graf_importancia<-ggplot(
  data = importancia_var,
  aes(x    = reorder(Predictor, Importancia),
      y    = Importancia,
      fill = Importancia)
) +
  labs(x = "Predictor", title = "Importancia de los predictores") +
  geom_col() +
  geom_text(aes(label = round(Importancia, 2)), hjust = 0, size = 4) +  # Agrega etiquetas con los números
  scale_fill_viridis_c() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 15))

print(graf_importancia)
ggsave(paste0(dir.r,"/","grafico_importancia_xgb.png"), plot = graf_importancia, width = 10, height = 8, units = "in", dpi = 300)

# ---------------------------------------------------------------- Score Modelo  --------------------------------------------------------------------

# Obtenemos el SCORE (Probabilidad de ser Bueno)
pred <- predict(bstDense, DATOS)

datos[,SCORE_XGB:=ceiling(1000*(1-pred))]

#----------------------------------------------------- Tablas Performance: Modelo-Base-Modelizacion -----------------------------------------------

#~~~~~~~~~~~~ Obtenemos la Información:
# Base de modelizacion
mod <- datos[ModVal == 0 ]
res_mod <- data.table(Var=mod$VarDep, Score=mod$SCORE_XGB)
res_mod$Rango <- rango_score(res_mod$Score)
# Informacion a poner en el excel
rangos<-res_mod[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]# Intervalos Score
rangos
frecuencias<-res_mod[,table(Rango, Var)]# Frecuencias de buenos, malos, indeterminados, otros por Rango de Score
frecuencias
100*res_mod[,prop.table(table(Rango, Var),1)]# Tasa de buenos y malos por Rango de Score


#~~~~~~~~~~~~ Cargamos la información a la plantilla:
writeData(plantilla, sheet = "Modelo-Base-Modelizacion",x=as.numeric(rangos$Min),startCol = 1,startRow = 3)# Cargamos los minimos de cada rango
writeData(plantilla, sheet = "Modelo-Base-Modelizacion",x=as.numeric(rangos$Max),startCol = 2,startRow = 3)# Cargamos los maximos de cada rango
writeData(plantilla, sheet = "Modelo-Base-Modelizacion",x=as.numeric(frecuencias[1:10]),startCol =4,startRow = 3)# Cargamos los buenos por rango
writeData(plantilla, sheet = "Modelo-Base-Modelizacion",x=as.numeric(frecuencias[11:20]),startCol =5,startRow = 3)# Cargamos los malos por rango
writeData(plantilla, sheet = "Modelo-Base-Modelizacion",x=as.numeric(frecuencias[21:30]),startCol =6,startRow = 3)# Cargamos los indeterminados por rango
writeData(plantilla, sheet = "Modelo-Base-Modelizacion",x=as.numeric(frecuencias[31:40]),startCol =7,startRow = 3)# Cargamos los otros por rango

#--------------------------------------------- Tablas Performance: Modelo-Base-Validacion -----------------------------------------------

#~~~~~~~~~~~~ Obtenemos la Información:
# Base de validacion
val <- datos[ModVal == 1 ]
res_val <- data.table(Var=val$VarDep, Score=val$SCORE_XGB)
res_val$Rango <- rango_score(res_val$Score)
# Informacion a poner en el excel
rangos<-res_val[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]# Intervalos Score
rangos
frecuencias<-res_val[,table(Rango, Var)]# Frecuencias de buenos y malos por Rango de Score
frecuencias
100*res_val[,prop.table(table(Rango, Var),1)]# Tasa de buenos y malos por Rango de Score


#~~~~~~~~~~~~ Cargamos la información a la plantilla:
writeData(plantilla, sheet = "Modelo-Base-Validacion",x=as.numeric(rangos$Min),startCol = 1,startRow = 3)# Cargamos los minimos de cada rango
writeData(plantilla, sheet = "Modelo-Base-Validacion",x=as.numeric(rangos$Max),startCol = 2,startRow = 3)# Cargamos los maximos de cada rango
writeData(plantilla, sheet = "Modelo-Base-Validacion",x=as.numeric(frecuencias[1:10]),startCol =4,startRow = 3)# Cargamos los buenos por rango
writeData(plantilla, sheet = "Modelo-Base-Validacion",x=as.numeric(frecuencias[11:20]),startCol =5,startRow = 3)# Cargamos los malos por rango
writeData(plantilla, sheet = "Modelo-Base-Validacion",x=as.numeric(frecuencias[21:30]),startCol =6,startRow = 3)# Cargamos los indeterminados por rango
writeData(plantilla, sheet = "Modelo-Base-Validacion",x=as.numeric(frecuencias[31:40]),startCol =7,startRow = 3)# Cargamos los otros por rango

#----------------------------------------------------- Almacenamiento de Resultados -----------------------------------------------------------------


# Guardamos las tablas performances
saveWorkbook(plantilla,paste(dir.r,"Tablas_Performance_XGB.xlsx",sep="/"),overwrite = TRUE)
setwd(dir.r)
write.xlsx(list("GRILLA" = resultados_xgb_1), file = "Grilla_XGB.xlsx")

# Se guarda la data generada paa este modelo
setwd(paste(dir.p,"BDD","RData",sep="/"))
save(list = c("datos"), file = "InfoXGB.RData")

#----------------------------------------------------------  Graficas Modelo ---------------------------------------------------------------

#~~~~~~~~~~~~ Grafica Curva ROC del modelo

objroc1 <- roc(res_mod$Var, res_mod$Score, auc=T, ci=T)
objroc2 <- roc(res_val$Var, res_val$Score, auc=T, ci=T)
plot(objroc1, col="blue", xlab="1 - Especificidad", ylab="Sensibilidad", main="Comparación curvas ROC", legacy.axes = TRUE)
plot(objroc2, col="red", add=TRUE)
legend("bottomright", legend=c("Modelamiento", "Validación"), col=c("blue", "red"), lwd=1.4)


rm(list=setdiff(ls(),c("dir.p","dir.r","dir.s")))
gc()

setwd(dir.p)

