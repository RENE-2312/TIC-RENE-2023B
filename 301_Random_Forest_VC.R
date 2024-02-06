# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modelo Random Forest %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este script se utiliza un modelo de random forest para abordar el problema de credit scoring, evaluando el rendimiento del modelo en
# dos conjuntos de datos distintos: uno para entrenar el modelo y otro para validar sus resultados. Para entrenar el modelo se realizó un 
# rebalanceo a la base de modelamiento para tener una proporción de 80%-20% de buenos y malos, a esta base se la denomina rmod.

# Las tablas performance se almacenan en un archivo de Excel, Tablas_Performance_RF.xlsx, en la carpeta de resultados, e incluyen 
# métricas como KS, ROC y GINI  para cada modelo-base.

# Los resultados del random forest se guardan en un archivo RData, Info_RF_VC.RData, ubicado en la subcarpeta RData de la carpeta BDD.

# Para ajustar el modelo, primero se seleccionaron las variables con mayor valor en las pruebas KS o VI. Se eligieron variables de diferente 
# indole (Idiosincráticas, Deuda, Operaciones, Días de Vencimiento, Entidades), en donde se seleccionaron variables sin una alta correlación.

# Una vez seleccionadas las variables, para la optimización de hiperparámetros, se procedió a crear una grilla de hiperparámetros 
# (mtry, ntrees, min_node) para escojer el mejor modelo para nuestros datos. La cantidad de mtry se determinó como (# predictores)^0.5, 
# según la recomendación para problemas de clasificación. En donde para la selección del mejor modelo se realizo validación cruzada en 
# la base rmod (método de k-folders).

# ¡¡¡¡¡¡Aviso importante!!!!!

# No es necesario ejecutar este script de forma independiente. Se puede ejecutar desde el script 001_Ejecutar_Proyecto.R 
# para asegurar una ejecución ordenada y completa del proyecto.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\t\t\t\t\tModelo Random Forest VC' )
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

#-------------------------------------------------------- Variables  -------------------------------------------------------

#-------------------------------------------------------- Random Forest  Formula -------------------------------------------------------
#~~~~~~~~~~~~~~~ Variables Iniciales:

variables_modelo<-c("prbm_PROVINCIA_ACTIVIDAD_DES",
                    "prbb_TOTAL_INGRESOS",
                    "prbm_DEUDA_VENCIDA_SCE_12M","prbm_DEUDA_TOTAL_OP_12M","r_DEUDA_TOTAL_SF_3a6M",
                    "prbm_NOPE_VENC_SB_36M","r_NUM_OPE_VIG_SB_12s24M",
                    "MARCA_NENT_VEN_SB_36M",
                    "prbb_NOPE_APERT_OP_24M",
                    "prbm_NUM_ACREED_SB_3M",
                    "r_MAX_DVEN_SB_6a12M")

#~~~~~~~~~~~~ Se crea la fórmula del modelo
formula<-"VarDep~"

numvariable<-1
for(variable in variables_modelo){
  if(numvariable==1){
    formula<-paste0(formula," ",variable," ")
  }else{
    formula<-paste0(formula,"+"," ",variable," ")
  }
  numvariable<-numvariable+1
}

# Formula del modelo
message( paste( rep( '~', 100 ), collapse = '' ) )
message( '\t\t\t\t\tFormula del Modelo RF - VC' )
message(formula)
message( paste( rep( '~', 100 ), collapse = '' ) )

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

#~~~~~~~~~~~~~~~~~~~~~~ Inicialización del modelo
rmod$VarDep<-as.factor(rmod$VarDep)
modelo <- rand_forest(
  mode  = "classification",
  mtry  = tune(),
  trees = tune(),
  min_n = tune(),
) %>%
  set_engine(
    engine     = "ranger",
    importance = "impurity",
    seed       = 12345
  )

#~~~~~~~~~~~~~~~~~~~~~~ Fórmula del modelo
transformer <- recipe(
  formula = as.formula(formula),
  data    =  rmod
)

#~~~~~~~~~~~~~~~~~~~~~~ Método de validación cruzada (k-folders, k=5)
set.seed(12345)
cv_folds <- vfold_cv(
  data    = rmod,
  v       = 5,
  strata  = VarDep
)

#~~~~~~~~~~~~~~~~~~~~~~ Workflow
workflow_modelado <- workflow() %>%
  add_recipe(transformer) %>%
  add_model(modelo)

#~~~~~~~~~~~~~~~~~~~~~~ Grilla de Hiperparamentros
hiperpar_grid <- expand_grid(
  'trees'     = c(66,83,99,116,132),
  'mtry'      = c(3,4),
  'min_n' = representatividad_1
)


cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

#~~~~~~~~~~~~~~~ Ajuste de los modelos 
grid_fit <- tune_grid(
  object    = workflow_modelado,
  resamples = cv_folds,
  metrics   = metric_set(accuracy),
  grid      = hiperpar_grid
)

stopCluster(cl)

#~~~~~~~~~~~~~~~ Resultados de los modelos
mejores_1<-show_best(grid_fit, metric = "accuracy", n =200 )
mejores_hiperpar_1 <- select_best(grid_fit, metric = "accuracy")


# --------------------------------------------------------- Random Forest Final  ----------------------------------------------------------------------------------

# Se ajusta el modelo de random forest que tiene menor error y reduce el sobreajuste en nuestros datos ntree=250, mtry= 4, min.node.size=346


# Filtramos la base de Modelamiento
mod <- datos[ModVal == 0 & VarDep %in% c(0,1)]
dim(mod)
mod[,table(VarDep)]
mod[,prop.table(table(VarDep))]

# Obtenemos la base rebalanceada
rmod<-rebalanceo(mod,0.2,0.2,12345)
dim(rmod)
rmod[,table(VarDep)]
rmod[,prop.table(table(VarDep))]

#~~~~~~~~~~~~~~~~~~~~ Optimización del modelo
modelo <- ranger(
  formula   = as.formula(formula),
  data      = rmod, 
  classification = TRUE,
  probability = TRUE,
  importance = "impurity",
  replace = TRUE,
  num.trees =132,#132
  mtry      =3 ,#3
  min.node.size =485,#485
  seed      = 12345
)

#~~~~~~~~~~~~~~~~~~~~ Informacion del modelo
modelo|>summary()
modelo$treetype


modelo$dependent.variable.name
modelo$prediction.error

modelo$importance.mode
modelo$splitrule

modelo$num.trees
modelo$num.independent.variables
modelo$mtry
modelo$min.node.size

modelo$call
modelo$num.samples
modelo$replace

modelo$forest$num.trees
modelo$forest$is.ordered
modelo$forest$treetype
modelo$forest$split.values


#---------------------------------------------------- Analisis de Correlación -------------------------------------------------------

#~~~~~~~~~~~~ Matriz de Correlacion:
res <- cor(setDT(rmod)[,variables_modelo, with=FALSE])
res

#~~~~~~~~~~~~ Graficos de la matriz de correlación
ggcorrplot(res, hc.order = TRUE, type = "upper",method = "circle",lab = TRUE)

correlacion<-ggcorrplot(res, hc.order = TRUE, type = "lower",outline.col = "white",
           lab = TRUE)

print(correlacion)
ggsave(paste0(dir.r,"/","grafico_correlacion_rf.png"), plot = correlacion, width = 10, height = 8, units = "in", dpi = 300)

#~~~~~~~~~~~~ Análisis espectral de la matriz de autocorrelación
# Condición de número de la matriz de correlación
sqrt(max(eigen(res)$values)/min(eigen(res)$values))
min(eigen(res)$values)
max(eigen(res)$values)

#------------------------------------------------------ Importancia de Variables  -------------------------------------------------------

# Vamos a ver las importancia de las variables en el modelo de random forest final 
importancia_pred <- modelo$variable.importance %>%
  enframe(name = "predictor", value = "importancia")

importancia_pred<-as.data.table(importancia_pred)
variables_top<-head(importancia_pred[order(importancia,decreasing = TRUE)],15)


# Gráfico de Importancia de las Variables
graf_importancia<-ggplot(
  data = importancia_pred,
  aes(x    = reorder(predictor, importancia),
      y    = importancia,
      fill = importancia)
) +
  labs(x = "Predictor", title = "Importancia predictores") +
  geom_col() +
  geom_text(aes(label = round(importancia, 0)), hjust = 0, size = 4) +  # Agrega etiquetas con los números
  scale_fill_viridis_c() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 15))


print(graf_importancia)
ggsave(paste0(dir.r,"/","grafico_importancia_rf.png"), plot = graf_importancia, width = 10, height = 8, units = "in", dpi = 300)

# ---------------------------------------------------------- Score Modelo  --------------------------------------------------------------------

# Obtenemos el SCORE (Probabilidad de ser Bueno)
predicciones<-modelo|>predict(data = datos)
datos[,SCORE_RF:=ceiling(1000*predicciones$predictions[,1])]

#--------------------------------------------- Tablas Performance: Modelo-Base-Modelizacion -----------------------------------------------

#~~~~~~~~~~~~ Obtenemos la Información:
# Base de modelizacion
mod <- datos[ModVal == 0 ]
res_mod <- data.table(Var=mod$VarDep, Score=mod$SCORE_RF)
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
res_val <- data.table(Var=val$VarDep, Score=val$SCORE_RF)
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
saveWorkbook(plantilla,paste(dir.r,"Tablas_Performance_RF_VC.xlsx",sep="/"),overwrite = TRUE)
setwd(dir.r)
write.xlsx(list("GRILLA" = mejores_1), file = "Grilla_Random_Forest.xlsx")

# Se guarda la data generada paa este modelo
setwd(paste(dir.p,"BDD","RData",sep="/"))
save(list = c("datos"), file = "InfoRF_VC.RData")

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


