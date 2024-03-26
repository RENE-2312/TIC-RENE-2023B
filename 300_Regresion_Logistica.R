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

# Se utilizó el test KS y VI para seleccionar las variables a entrar en el modelo, buscando aquellas que tenían la menor
# correlación, así como aquellas que proporcionaban información sobre deuda, operaciones, entidades, acreedores, días de vencimiento, 
# características idiosincráticas y estados financieros para los diferentes sistemas de crédito del país. 

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

#---------------------------------------------------- Regresion Logística Final -------------------------------------------------------

# Escogemos variables:

variables_modelo<-c("prbm_PROVINCIA_ACTIVIDAD_DES",
                    "prbm_TOTAL_INGRESOS",
                    "MARCA_DVEN_SF_3M",
                    "r_NUM_OPE_VIG_SF_12s36M","prbm_NOPE_APERT_OP_24M",
                    "prbb_DEUDA_VENCIDA_SCE_36M","prbm_DEUDA_TOTAL_OP_12M",
                    "prbm_NUM_ENT_VIG_SB_12M")

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
message( '\t\t\t\t\tFormula del Modelo RGL' )
message(formula)
message( paste( rep( '~', 100 ), collapse = '' ) )

#~~~~~~~~~~~~ Ajuste regresión logistica (data modelamiento)
modelo <- glm(formula = as.formula(formula), family = binomial("logit"), data = rmod)
summary(modelo)

resultados_modelo<-summary(modelo)$coefficients

#---------------------------------------------------- Analisis de Correlación -------------------------------------------------------

#~~~~~~~~~~~~ Matriz de Correlacion:
res <- cor(setDT(rmod)[,variables_modelo, with=FALSE])
res

#~~~~~~~~~~~~ Graficos de la matriz de correlación
ggcorrplot(res, hc.order = TRUE, type = "upper",method = "circle",lab = TRUE)

correlacion<-ggcorrplot(res, hc.order = TRUE, type = "lower",outline.col = "white",
           lab = TRUE)

print(correlacion)
ggsave(paste0(dir.r,"/","grafico_correlacion_rgl.png"), plot = correlacion, width = 10, height = 8, units = "in", dpi = 300)

#~~~~~~~~~~~~ Análisis de Condicionamiento
# Condición de número de la matriz de correlación
sqrt(max(eigen(res)$values)/min(eigen(res)$values))
min(eigen(res)$values)
max(eigen(res)$values)
# Como el valor es menor a 10 nos indica que no hay autocorrelación

#---------------------------------------------------------- GVIF ------------------------------------------------------------------------
vif(modelo)

#----------------------------------------------- Importancia de las variables ------------------------------------------------------------------------
importancia<-varImp(modelo)
importancia_var<-data.table(Predictor=rownames(importancia),Importancia=importancia$Overall)
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
  geom_text(aes(label = round(Importancia, 1)), hjust = 0, size = 4) +  # Agrega etiquetas con los números
  scale_fill_viridis_c() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 15))

print(graf_importancia)
ggsave(paste0(dir.r,"/","grafico_importancia_rgl.png"), plot = graf_importancia, width = 10, height = 8, units = "in", dpi = 300)

#------------------------------------------------------ Score Modelo --------------------------------------------------------------------------

# Calculo del Score para todos los individuos (bases de modelamiento y validacion y todas las categorias de VarDep)

#~~~~~~~~~~~~ Coeficientes del Modelo 
coeficientes_modelo<-modelo$coefficients

#~~~~~~~~~~~~ Fórmula ajustada del modelo
formula_modelo<-paste0(coeficientes_modelo[1])
numvar<-1

for(variable in variables_modelo){#variable<-variables_modelo[1]
  if(coeficientes_modelo[numvar+1]>0){
    formula_modelo<-paste0(formula_modelo," + ",coeficientes_modelo[numvar+1],"*",variable," ") 
  }else{
    formula_modelo<-paste0(formula_modelo," ",coeficientes_modelo[numvar+1],"*",variable," ")
  }
  numvar<-numvar+1
}

message( paste( rep( '~', 100 ), collapse = '' ) )
message( '\t\t\t\t\tModelo Ajustado' )
message(paste0("VarDep~ ",formula_modelo))
message( paste( rep( '~', 100 ), collapse = '' ) )

#~~~~~~~~~~~~ Scores dados por el modelo
eval(parse(text=paste0("datos[, Y:=",formula_modelo,"]")))
datos[, SCORE_RGL:= ceiling(1000/(1+exp(Y)))]# SCORE: Probabilidad de ser bueno 

# La regresion logistica da las probabilidades de ser malo porque hemos considerado 1 a los malos

#--------------------------------------------- Tablas Performance: Modelo-Base-Modelizacion -----------------------------------------------

#~~~~~~~~~~~~ Obtenemos la Información:
# Base de modelizacion
mod <- datos[ModVal == 0 ]
res_mod <- data.table(Var=mod$VarDep, Score=mod$SCORE_RGL)
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
res_val <- data.table(Var=val$VarDep, Score=val$SCORE_RGL)
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
saveWorkbook(plantilla,paste(dir.r,"Tablas_Performance_RGL.xlsx",sep="/"),overwrite = TRUE)

# Se guarda la data generada paa este modelo
setwd(paste(dir.p,"BDD","RData",sep="/"))
save(list = c("datos"), file = "InfoRGL.RData")

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

