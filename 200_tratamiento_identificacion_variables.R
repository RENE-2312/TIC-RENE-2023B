# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Tratamiento e Identificación  de Variables %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este script se lleva a cabo la ingeniería de variables, abordando aspectos como idiosincrasias, estado financiero, información 
# de deuda ($), número de días de vencimiento, número de operaciones, número de entidades, número de acreedores y ratios. La información
# generada se guarda en un archivo RData, InfoModelamiento.RData, ubicado en la subcarpeta RData de la carpeta BDD.

# Además, se realiza el análisis de variables para el modelamiento futuro. Se extraen inicialmente registros de buenos y malos pagadores 
# de la base de modelamiento, y se aplican pruebas específicas para identificar las variables más informativas en la predicción de la 
# variable dependiente. Estas variables se consideran candidatas para ingresar al modelo. Los resultados de este análisis se almacenan en
# un archivo Excel, Analisis_Variables_Tratamiento_Final.xlsx, en la carpeta de Resultados.

# Para la creación de nuevas variables, se utilizará exclusivamente la información de individuos clasificados como buenos y malos en el 
# conjunto de modelado, con el objetivo de trabajar con datos completos. Esto se hace para evitar el sobreajuste al modelo y garantizar que
# los resultados de la validación sean lo más transparentes posibles. Además, los datos de validación representan teóricamente a individuos 
# nuevos, por lo que su análisis no se incluye en la generación de variables. Además, es importante señalar que las personas indeterminadas,
# sin desempeño y no bancarizadas se considerarán como buenos o malos, solo que aún no se pueda predecir su comportamiento.


# Se ha decidido que la información numérica discreta (por ejemplo, NOPE_VENC) se tome únicamente del SB o SF (SB+SEPS),
# mientras que la información numérica continua (valores de deuda) se tomará del SCE (SICOM+SF).

# Esta decisión se tomó por el siguiente motivo: si consideramos las variables discretas de todo el SCE, estaríamos, por ejemplo, 
# poniendo al mismo nivel una deuda en el SB que en SICOM. Esto podría perjudicar y beneficiar a otras personas, ya que puede haber 
# deudas en retails bajas con un tiempo alto de vencimiento, mientras que puede haber deudas en SF o SB altas con poco tiempo de vencimiento.


# ¡¡¡¡¡¡Aviso importante!!!!!

# No es necesario ejecutar este script de forma independiente. Se puede ejecutar desde el script 001_Ejecutar_Proyecto.R
# para asegurar una ejecución ordenada y completa del proyecto.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\t\t\tTratamiento e Identificación de Variables' )
message( paste( rep( '-', 100 ), collapse = '' ) )

#---------------------------------------------------------------- Direcciones y Funciones --------------------------------------------------------
options(scipen = 999)

# Direccion de los RData
dir.b <- paste0(dir.p, "/BDD/RData")
setwd(dir.b)
list.files()
#~~~~~~~~~~~~~~~~~~~ Lectura información Tratamiento Inicial:
load("InfoTratamientoInicial.RData")

nombres_variables<-data.frame(Variable=colnames(datos))

#~~~~~~~~~~~~~~~~~~~ Función de reemplazo de NA's por columna:
reemplazo_col = function(dt, vars, valor){
  na.replace = function(v, value=valor) { v[is.na(v)] = value; v }
  for (i in vars)
    eval(parse(text=paste("dt[,",i,":=na.replace(",i,")]")))
}


#---------------------------------------------------------------------  EDA:  Inicial  -------------------------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~ Función de estadísticos:

fun <- function(x, rm=TRUE){
  if(class(x)[1] %in% c("numeric", "integer")){
    res <- c(tipo=class(x),
             perdidos =100*mean(is.na(x)),
             nulos = 100*mean(x==0, na.rm = rm),
             minimo = min(x, na.rm = rm),
             p1 = quantile(x, probs=0.01, na.rm=rm),
             p2 = quantile(x, probs=0.02, na.rm=rm),
             p5 = quantile(x, probs=0.05, na.rm=rm),
             p10 = quantile(x, probs=0.1, na.rm=rm),
             p25 = quantile(x, probs=0.25, na.rm=rm),
             mediana = median(x, na.rm = rm),
             p75 = quantile(x, probs=0.75, na.rm=rm),
             p90 = quantile(x, probs=0.9, na.rm=rm),
             p95 = quantile(x, probs=0.95, na.rm=rm),
             p98 = quantile(x, probs=0.98, na.rm=rm),
             p99 = quantile(x, probs=0.99, na.rm=rm),
             maximo = max(x, na.rm = rm),
             media = mean(x, na.rm = rm),
             sd = sd(x, na.rm = rm)
    )
  } else {
    res <- c(tipo=class(x),perdidos = 100*mean(is.na(x)),0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  }
  return(res)
}

# Filtramos la data:
base <- datos

# Se calculan los estadisticos usando la función fun:
res <- do.call(cbind, lapply(base, fun))
res <- as.data.table(res)
t_res <- t(res)
colnames(t_res) <- c("tipo variable","perdidos", "nulos", "minimo", "p1", "p2", "p5", "p10", "p25", "mediana", "p75", "p90", "p95", "p98", "p99",  "maximo", "media", "sd")
t_res <- as.data.table(t_res)
t_res[, Variable := colnames(res)]
setcolorder(t_res, c(ncol(t_res), 1:(ncol(t_res)-1)))

# Se guardan los estadísticos en un Excel:
setwd(dir.r)
write.xlsx(list("Descriptivos" = t_res), file = "Descriptivos_Institucion_Inicial.xlsx")

rm(list=setdiff(ls(),c("dir.p","dir.r","dir.s","datos","reemplazo_col")))
gc()

# Se restaura la dirección de trabajo a la carpeta principal del proyecto
setwd(dir.p)


#---------------------------------------------------- - Análisis de Variables Inicial: KS-VI ------------------------------------------------------------


# Filtramos la data para los individuos  buenos y malos de la base de modelamiento (Esta sera la información a modelar)
# ModVal=1 si la información corresponde a  validación, 0 si corresponde a modelamiento
mod <- datos[ModVal == 0 & VarDep %in% c(0,1)]
dim(mod)
datos[,table(ModVal)]

# Fijamos  la dirección de la carpeta en donde se encuentra el script con las funciones auxiliares para el análisis de variables
setwd(dir.s)
source("Tidy_data.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Identificación de variables con alto porcentaje de NA's

porc <- sort(sapply(mod, porcNA), decreasing = TRUE)
PorcentajeNA <- data.frame(names(porc), as.numeric(porc))
colnames(PorcentajeNA) <- c("Var", "Porc")

# Almacenamos las variables validas
dvars <- setdiff(colnames(mod), names(porc)[porc > 0.3])
PorcentajeNA
dvars
# Numero de variables validas
length(dvars)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Identificación de variables constantes

# Nos quedamos unicamente con las variables no constantes:
dvars <- dvars[!unname(unlist(sapply(mod[,dvars,with=FALSE], constante)))]
# Numero de variables validas
length(dvars)
rm(list = c("PorcentajeNA", "porc", "porcNA", "constante"))
length(dvars)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selección de Variables Validas

# Nos quedamos con las variables validas de la base de modelamiento ( variables con un porcentaje de NA menor al 30% y que no sean constantes)
mod <- mod[, dvars, with = FALSE]

# Seleccionamos las variables numericas
vnum <- colnames(mod)[unname(sapply(mod, class)) %in% c("numeric", "integer")]
dnum <- mod[, vnum, with=FALSE]

# Seleccionamos las variables categoricas
vcat <- colnames(mod)[unname(sapply(mod, class)) %in% c("character", "logical")]
dcat <- mod[, vcat, with=FALSE]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Ejecucion de KS sobre variables numericas

KS <- sapply(seq_along(dnum), function(i){TestKS(dnum[[i]], mod$VarDep)}) # Revisar variable dependiente
dKS <- data.frame(colnames(dnum), KS); dKS <- dKS[order(dKS$KS, decreasing = TRUE),]
colnames(dKS) <- c("Variable", "KS"); rownames(dKS) <- NULL
dKS

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Ejecucion de VI sobre variables categóricas

VI <- sort(sapply(dcat, TestVI, y=mod$VarDep), decreasing = T) # Revisar variable dependiente
dVI <- data.frame(names(VI), VI)
colnames(dVI) <- c("Variable", "VI"); rownames(dVI) <- NULL
dVI

# Se guardan los resultados del test de KS y VI en un Excel:
setwd(dir.r)
write.xlsx(list("KS_Var" = dKS, "VI" = dVI), file = "Analisis_Variables_Tratamiento_Inicial.xlsx")

rm(list=setdiff(ls(),c("dir.p","dir.r","dir.s","datos","reemplazo_col")))
gc()

# Se restaura la dirección de trabajo a la carpeta principal del proyecto
setwd(dir.p)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Ingeniería de Variables %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#------------------------------------------------------- Recodificación: Provincia Actividad  -------------------------------------------------------------------

#~~~~~~~~~~~~~~ Frecuencias de las Provincias
datos[,.N,PROVINCIA_ACTIVIDAD]

#~~~~~~~~~~~~~~ Estandarizamos las Provincias por nombre
# Los codificacion la encontramos en: https://www.ecuadorencifras.gob.ec/documentos/web-inec/Multiproposito/201812_Marco_Maestro_de_Muestreo_Multiproposito.pdf
# En la pag 8
datos[!is.na(PROVINCIA_ACTIVIDAD),PROVINCIA_ACTIVIDAD:=case_when(PROVINCIA_ACTIVIDAD=="07"~"EL ORO",
                                                                 PROVINCIA_ACTIVIDAD=="01"~"AZUAY",
                                                                 PROVINCIA_ACTIVIDAD=="23"~"SANTO DOMINGO DE LOS TSÁCHILAS",
                                                                 PROVINCIA_ACTIVIDAD=="06"~"CHIMBORAZO",
                                                                 PROVINCIA_ACTIVIDAD=="17"~"PICHINCHA",
                                                                 PROVINCIA_ACTIVIDAD=="03"~"CAÑAR",
                                                                 PROVINCIA_ACTIVIDAD=="09"~"GUAYAS",
                                                                 PROVINCIA_ACTIVIDAD=="24"~"SANTA ELENA",
                                                                 PROVINCIA_ACTIVIDAD=="02"~"BOLIVAR",
                                                                 PROVINCIA_ACTIVIDAD=="18"~"TUNGURAHUA",
                                                                 PROVINCIA_ACTIVIDAD=="05"~"COTOPAXI",
                                                                 PROVINCIA_ACTIVIDAD=="12"~"LOS RIOS",
                                                                 PROVINCIA_ACTIVIDAD=="14"~"MORONA SANTIAGO",
                                                                 PROVINCIA_ACTIVIDAD=="13"~"MANABI",
                                                                 PROVINCIA_ACTIVIDAD=="22"~"ORELLANA",
                                                                 PROVINCIA_ACTIVIDAD=="21"~"SUCUMBIOS",
                                                                 PROVINCIA_ACTIVIDAD=="19"~"ZAMORA CHINCHIPE",
                                                                 PROVINCIA_ACTIVIDAD=="15"~"NAPO",
                                                                 TRUE~PROVINCIA_ACTIVIDAD)]
#~~~~~~~~~~~~~~ Frecuencias de las Provincias
datos[,.N,PROVINCIA_ACTIVIDAD]

# Agrupamos en la Categoria Otros a los individuos con creditos en otros lugares fuera del país:

datos[!is.na(PROVINCIA_ACTIVIDAD),PROVINCIA_ACTIVIDAD:= case_when(PROVINCIA_ACTIVIDAD %in% c("NY","US","OK","AA","VE")~"OTRAS",
                                                                  TRUE~PROVINCIA_ACTIVIDAD)]
#~~~~~~~~~~~~~~ Frecuencias de las Provincias
datos[,.N,PROVINCIA_ACTIVIDAD]

#------------------------------------------------------- Recodificación: Tipo Vivienda -------------------------------------------------------------------

#~~~~~~~~~~~~~~ Frecuencias de las Viviendas
datos[,.N,TIPO_VIVIENDA]
datos[,prop.table(table(TIPO_VIVIENDA,VarDep),1)]

#~~~~~~~~~~~~~~ Estandarizamos las Viviendas por nombre
datos[!is.na(TIPO_VIVIENDA),TIPO_VIVIENDA:=case_when(TIPO_VIVIENDA=="A"~"ARRENDADA",
                                                     TIPO_VIVIENDA=="F"~"VIVE CON FAMILIARES",
                                                     TIPO_VIVIENDA=="H"~"PROPIA HIPOTECADA",
                                                     TIPO_VIVIENDA=="P"~"PROPIA NO HIPOTECADA",
                                                     TRUE~TIPO_VIVIENDA)]
#~~~~~~~~~~~~~~ Frecuencias de las Viviendas
datos[,.N,TIPO_VIVIENDA]

#------------------------------------------------------- Recodificación: Estado Civil  -------------------------------------------------------------------

#~~~~~~~~~~~~~~ Frecuencias del Estado Civil
datos[,.N,ESTADO_CIVIL]
datos[,table(ESTADO_CIVIL,useNA = "always")]
datos[,ESTADO_CIVIL:=case_when(ESTADO_CIVIL=="CASADO/A"~"CASADO",
                               ESTADO_CIVIL=="VIUDO/A"~"VIUDO",
                               ESTADO_CIVIL=="DIVORCIADO/A"~"DIVORCIADO",
                               ESTADO_CIVIL=="UNIONLIBRE"~"UNION LIBRE",
                               ESTADO_CIVIL=="SOLTERO/A"~"SOLTERO",
                               TRUE~ESTADO_CIVIL)]
datos[,.N,ESTADO_CIVIL]

#----------------------------------------------------------------- Selección Datos  -------------------------------------------------------------------

# Para el análisis posterior, se selecciono a los individuos  buenos y malo de la base de modelado. Con esta información, crearemos 
# nuevas variables para todos los individuos. Es importante recordar que la base original se dividió en una proporción de 50-50 para
# entrenamiento y prueba, esto se realizó debido a la gran cantidad de registros.

mod <- datos[ModVal == 0 & VarDep %in% c(0,1)]
dim(mod)
mod[,table(VarDep)]
mod[,prop.table(table(VarDep))]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Variables de Marca ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#-------------------------------------------------------------- MAX_DVEN_SB_3M -------------------------------------------------------------------

datos[,MARCA_DVEN_SB_3M:=ifelse(MAX_DVEN_SB_3M>0,1,0)]
datos[,table(MARCA_DVEN_SB_3M)]

#-------------------------------------------------------------- NENT_VENC_SB_24M ------------------------------------------------------------------- 

datos[,MARCA_NENT_VENC_SB_24M:=ifelse(NENT_VEN_SB_24M>0,1,0)]

#--------------------------------------------------------------- MAX_DVEN_SF_3M -------------------------------------------------------------------

datos[,MARCA_DVEN_SF_3M:=ifelse(MAX_DVEN_SB_3M+MAX_DVEN_SC_3M>0,1,0)]
datos[,table(MARCA_DVEN_SF_3M)]

#-------------------------------------------------------------- NENT_VEN_SB_36M ------------------------------------------------------------------- 

datos[,MARCA_NENT_VEN_SB_36M:=ifelse(NENT_VEN_SB_36M>0,1,0)]
datos[,table(MARCA_NENT_VEN_SB_36M,useNA = "always")]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Ratios  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#---------------------------------------------------------- r_NUM_OPE_VIG_SF_12s36M ------------------------------------------------------

datos[,r_NUM_OPE_VIG_SF_12s36M:=ifelse(NUM_OPE_VIG_SB_36M+NUM_OPE_VIG_SC_36M>0,(NUM_OPE_VIG_SB_12M+NUM_OPE_VIG_SC_12M)/(NUM_OPE_VIG_SB_36M+NUM_OPE_VIG_SC_36M),0)]

#------------------------------------------------------------ r_MAX_DVEN_SB_6a12M  -------------------------------------------------------------------

datos[, r_MAX_DVEN_SB_6a12M := ifelse(MAX_DVEN_SB_12M > 0, MAX_DVEN_SB_6M/MAX_DVEN_SB_12M, 0)]

#---------------------------------------------------------- r_DEUDA_TOTAL_SF_3a6M -------------------------------------------------------------------

datos[, r_DEUDA_TOTAL_SF_3a6M := ifelse(DEUDA_TOTAL_SF_6M > 0, DEUDA_TOTAL_SF_3M/DEUDA_TOTAL_SF_6M, 0)]

#---------------------------------------------------------- r_NUM_OPE_VIG_SB_12s24M -------------------------------------------------------------------

datos[, r_NUM_OPE_VIG_SB_12s24M := ifelse(NUM_OPE_VIG_SB_24M > 0, NUM_OPE_VIG_SB_12M/NUM_OPE_VIG_SB_24M, 0)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Variables de Probabilidad  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#---------------------------------------------------------------- Clustering  -------------------------------------------------------------------

# Agrupamos las variables (categóricas/numéricas) utilizando el método de árboles fijando una semilla.
# Creamos categorías e intervalos con la misma proporción de individuos clasificados como malos o buenos. 
# Por ahora, hemos decidido categorizar las variables en tres subconjuntos para obtener perfiles de bajo, medio y alto riesgo.

set.seed(12345)
fc_tree_malos <- mod |> woebin(y='VarDep', positive = 1, method = 'tree', bin_num_limit = 3)
fc_tree_buenos <- mod |> woebin(y='VarDep', positive = 0, method = 'tree', bin_num_limit = 3)

#~~~~~~~~~~~~~ Analisis del anterior resultado:

# Haremos el Ejemplo con la variable Estado Civil
fc_tree_malos$ESTADO_CIVIL

# En bin tenemos los valores/categorias agrupadas para cada variable
# En count_distr vemos la representatividad de las nuevas categorias
# En posprob vemos la  probabilidad de ser mal pagador/buen pagador dado la nueva categoria agrupada
# A continuacion, la grafica de las probabilidades de ser malos por categoria con un diagrama de columnas para ver el porcentaje de malos/buenos
# dentro de cada nueva categoria

woebin_plot(fc_tree_malos$ESTADO_CIVIL)

# ¡¡¡¡¡¡Aviso importante!!!!!
# A continuación, se llevará a cabo la ingeniería de variables final, basándonos en gran medida en la agrupación de datos proporcionada 
# por el método de árboles.


#-------------------------------------------------------------- DEUDA VENCIDA SCE 12M -------------------------------------------------------------------

# Para crear intervalos de deuda ($), se decidió subdividir aún más los intervalos para lograr una mejor segregación.
variables_deuda<-c("DEUDA_VENCIDA_SCE_12M","DEUDA_VENCIDA_SCE_36M","DEUDA_TOTAL_OP_12M")
deuda_malos<-mod|> woebin(y='VarDep',x=variables_deuda,positive = 1, method = 'tree',bin_num_limit = 15) 

# Son 0.6% puntos porcentuales mas, se agrupa en uno solo a los de [0,90[ de deuda vencida
deuda_malos$DEUDA_VENCIDA_SCE_12M

woebin_plot(deuda_malos$DEUDA_VENCIDA_SCE_12M)

mod[,Marca_DEUDA_VENCIDA_SCE_12M:=case_when(DEUDA_VENCIDA_SCE_12M==0~1,
                                            DEUDA_VENCIDA_SCE_12M>0 & DEUDA_VENCIDA_SCE_12M<100 ~2,
                                            DEUDA_VENCIDA_SCE_12M>=100 & DEUDA_VENCIDA_SCE_12M<260 ~3,
                                            TRUE~4)]

mod[,prop.table(table(Marca_DEUDA_VENCIDA_SCE_12M,VarDep,useNA = "always"),1)]

datos[,prbm_DEUDA_VENCIDA_SCE_12M:=case_when(DEUDA_VENCIDA_SCE_12M==0~0.05918367,
                                             DEUDA_VENCIDA_SCE_12M>0 & DEUDA_VENCIDA_SCE_12M<100 ~0.07909605,
                                             DEUDA_VENCIDA_SCE_12M>=100 & DEUDA_VENCIDA_SCE_12M<260 ~0.17523364,
                                             TRUE~0.30717489)]


#-------------------------------------------------------------- DEUDA VENCIDA SCE 36M -------------------------------------------------------------------

deuda_malos$DEUDA_VENCIDA_SCE_36M

woebin_plot(deuda_malos$DEUDA_VENCIDA_SCE_36M)

mod[,Marca_DEUDA_VENCIDA_SCE_36M:=case_when(DEUDA_VENCIDA_SCE_36M==0~1,
                                            DEUDA_VENCIDA_SCE_36M>0 & DEUDA_VENCIDA_SCE_36M<180 ~2,
                                            DEUDA_VENCIDA_SCE_36M>=180 & DEUDA_VENCIDA_SCE_36M<1060 ~3,
                                            TRUE~4)]

100*mod[,prop.table(table(Marca_DEUDA_VENCIDA_SCE_36M))]
mod[,prop.table(table(Marca_DEUDA_VENCIDA_SCE_36M,VarDep,useNA = "always"),1)]


datos[,prbm_DEUDA_VENCIDA_SCE_36M:=case_when(DEUDA_VENCIDA_SCE_36M==0~0.0507335,
                                             DEUDA_VENCIDA_SCE_36M>0 & DEUDA_VENCIDA_SCE_36M<180 ~0.0800000,
                                             DEUDA_VENCIDA_SCE_36M>=180 & DEUDA_VENCIDA_SCE_36M<1060 ~0.1866667,
                                             TRUE~0.2950601)]

datos[,prbb_DEUDA_VENCIDA_SCE_36M:=1-prbm_DEUDA_VENCIDA_SCE_36M]

#-------------------------------------------------------------- DEUDA_TOTAL_OP_12M -------------------------------------------------------------------

deuda_malos$DEUDA_TOTAL_OP_12M

woebin_plot(deuda_malos$DEUDA_TOTAL_OP_12M)

mod[,Marca_DEUDA_TOTAL_OP_12M:=case_when(DEUDA_TOTAL_OP_12M==0~1,
                                         DEUDA_TOTAL_OP_12M>0 & DEUDA_TOTAL_OP_12M<6500~2,
                                         DEUDA_TOTAL_OP_12M>=6500 & DEUDA_TOTAL_OP_12M<11500~3,
                                         TRUE~4)]

100*mod[,prop.table(table(Marca_DEUDA_TOTAL_OP_12M))]
mod[,prop.table(table(Marca_DEUDA_TOTAL_OP_12M,VarDep,useNA = "always"),1)]

datos[,prbm_DEUDA_TOTAL_OP_12M:=case_when(DEUDA_TOTAL_OP_12M==0~0.05726316,
                                          DEUDA_TOTAL_OP_12M>0 & DEUDA_TOTAL_OP_12M<6500~0.07704655,
                                          DEUDA_TOTAL_OP_12M>=6500 & DEUDA_TOTAL_OP_12M<11500~0.14315789,
                                          TRUE~0.26140684)]

#-------------------------------------------------------------- NUM_ENT_VIG_SB_12M ------------------------------------------------------------------- 

fc_tree_malos$NUM_ENT_VIG_SB_12M
woebin_plot(fc_tree_malos$NUM_ENT_VIG_SB_12M)

mod[,Marca_NUM_ENT_VIG_SB_12M:=case_when(NUM_ENT_VIG_SB_12M<1~1,
                                         NUM_ENT_VIG_SB_12M==1 | NUM_ENT_VIG_SB_12M==2 ~2,
                                         TRUE~3)]

mod[,prop.table(table(Marca_NUM_ENT_VIG_SB_12M,VarDep,useNA = "always"),1)]

datos[,prbm_NUM_ENT_VIG_SB_12M:=case_when(NUM_ENT_VIG_SB_12M<1~ 0.0688019,
                                          NUM_ENT_VIG_SB_12M==1 | NUM_ENT_VIG_SB_12M==2 ~0.1418372,
                                          TRUE~0.3120805)]

#-------------------------------------------------------------- NOPE_APERT_OP_24M ------------------------------------------------------------------- 

fc_tree_malos$NOPE_APERT_OP_24M
woebin_plot(fc_tree_malos$NOPE_APERT_OP_24M)

mod[,Marca_NOPE_APERT_OP_24M:=case_when(NOPE_APERT_OP_24M<1~1,
                                        NOPE_APERT_OP_24M==1~2,
                                        TRUE~3)]

mod[,prop.table(table(Marca_NOPE_APERT_OP_24M,VarDep,useNA = "always"),1)]

datos[,prbm_NOPE_APERT_OP_24M:=case_when(NOPE_APERT_OP_24M<1~0.0620858,
                                         NOPE_APERT_OP_24M==1~0.1257426,
                                         TRUE~0.2160633)]

datos[,prbb_NOPE_APERT_OP_24M:=case_when(NOPE_APERT_OP_24M<1~1-0.0620858,
                                         NOPE_APERT_OP_24M==1~1-0.1257426,
                                         TRUE~1-0.2160633)]

#-------------------------------------------------------------- Provincia Actividad  -------------------------------------------------------------------

#~~~~~~~~~~~~~~ Análisis en la base de modelamiento
fc_tree_malos$PROVINCIA_ACTIVIDAD
woebin_plot(fc_tree_malos$PROVINCIA_ACTIVIDAD)

fc_tree_malos$PROVINCIA_ACTIVIDAD$bin
# Vemos si la agrupacion tiene sentido
100*mod[,prop.table(table(PROVINCIA_ACTIVIDAD,useNA = "always"))]
mod[,table(PROVINCIA_ACTIVIDAD,useNA = "always")]
100*mod[,prop.table(table(PROVINCIA_ACTIVIDAD,VarDep,useNA = "always"),1)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Forma Actual desagregada:

mod[,prop.table(table(VarDep))]

# Representavidad mayor al 1% y tasa de malos menor al 10.79%
Provincias_1<-c("AZUAY","BOLIVAR","CAÑAR","CHIMBORAZO","COTOPAXI","GUAYAS")
# Representatividad mayor al 1% y tasa de malos mayor al 10.79% pero no superior a los peores
Provincias_2<-c("PICHINCHA","SANTA ELENA", "SANTO DOMINGO DE LOS TSÁCHILAS")
# Provincias mayor al 1% y tasa de malos superior al 10.79% y superior al 20%
Provincias_3<-c("EL ORO", "TUNGURAHUA")
# Representatividad menor al 1%
Provincias_4<-c("IMBABURA","LOJA","LOS RIOS","MANABI","NAPO","ORELLANA","OTRAS","PASTAZA","ZAMORA CHINCHIPE")

# A pesar de que las provincias con baja representatividad tienen una tasa baja de malos, se ha decidido incluirlos en la categoría de
# provincias de malos pagadores debido a su baja representatividad. Se adopta una postura conservadora,además, su porcentaje no supera el 1.5%.

Provincias_malas<-c(Provincias_3,Provincias_4)

mod[!is.na(PROVINCIA_ACTIVIDAD),Marca_PROVINCIA_AGRUPADA:=case_when(PROVINCIA_ACTIVIDAD %in% Provincias_1~1,
                                                                    PROVINCIA_ACTIVIDAD %in% Provincias_2~2,
                                                                    TRUE~3)]


mod[,prop.table(table(Marca_PROVINCIA_AGRUPADA,useNA = "always"))]
mod[,prop.table(table(Marca_PROVINCIA_AGRUPADA,VarDep,useNA = "always"),1)]

# En caso de que exista una categoría no representativa, es decir, que no aparezca en la base de modelización,
# se asignará la probabilidad de ser malo del peor caso, se toma una postura conservadora. Lo mismo se aplicará para los valores NA.


datos[,prbm_PROVINCIA_ACTIVIDAD_DES:=case_when(PROVINCIA_ACTIVIDAD %in% Provincias_malas | is.na(PROVINCIA_ACTIVIDAD)~0.25304136,
                                               PROVINCIA_ACTIVIDAD %in% Provincias_2~0.15691789,
                                               PROVINCIA_ACTIVIDAD %in% Provincias_1~0.06738392,
                                               TRUE~0.25304136)]

datos[,prbb_PROVINCIA_ACTIVIDAD_DES:=case_when(PROVINCIA_ACTIVIDAD %in% Provincias_malas | is.na(PROVINCIA_ACTIVIDAD)~1-0.25304136,
                                               PROVINCIA_ACTIVIDAD %in% Provincias_2~1-0.15691789,
                                               PROVINCIA_ACTIVIDAD %in% Provincias_1~1-0.06738392,
                                               TRUE~1-0.25304136)]

datos[,table(PROVINCIA_ACTIVIDAD,prbm_PROVINCIA_ACTIVIDAD_DES)]

#------------------------------------------------------------------- Total Ingresos  -------------------------------------------------------------------

ingresos_buenos<-mod|> woebin(y='VarDep',x="TOTAL_INGRESOS",positive = 0, method = 'tree',bin_num_limit = 10) 
ingresos_buenos$TOTAL_INGRESOS

woebin_plot(ingresos_buenos$TOTAL_INGRESOS)

mod[,Marca_TOTAL_INGRESOS:=case_when(TOTAL_INGRESOS<700~1,
                                     TOTAL_INGRESOS>=700 & TOTAL_INGRESOS<900~2,
                                     TOTAL_INGRESOS>=900 & TOTAL_INGRESOS<2900~3,
                                     TRUE~4)]

mod[,prop.table(table(Marca_TOTAL_INGRESOS,useNA = "always"))]
mod[,prop.table(table(Marca_TOTAL_INGRESOS,VarDep,useNA = "always"),1)]

datos[,prbb_TOTAL_INGRESOS:=case_when(TOTAL_INGRESOS<700~0.85363128,
                                      TOTAL_INGRESOS>=700 & TOTAL_INGRESOS<900~0.88369305,
                                      TOTAL_INGRESOS>=900 & TOTAL_INGRESOS<2900~0.89579784,
                                      TRUE~0.94615385)]

datos[,prbm_TOTAL_INGRESOS:=case_when(TOTAL_INGRESOS<700~1-0.85363128,
                                      TOTAL_INGRESOS>=700 & TOTAL_INGRESOS<900~1-0.88369305,
                                      TOTAL_INGRESOS>=900 & TOTAL_INGRESOS<2900~1-0.89579784,
                                      TRUE~1-0.94615385)]


#-------------------------------------------------------------- NOPE_VENC_SB_36M ------------------------------------------------------------------- 

nope_malos<-mod|> woebin(y='VarDep',x="NOPE_VENC_SB_36M",positive = 1, method = 'tree',bin_num_limit = 10) 
nope_malos$NOPE_VENC_SB_36M
woebin_plot(nope_malos$NOPE_VENC_SB_36M)

mod[,Marca_NOPE_VENC_SB_36M:=case_when(NOPE_VENC_SB_36M<1~1,
                                       NOPE_VENC_SB_36M>=1 & NOPE_VENC_SB_36M <4~2,
                                       NOPE_VENC_SB_36M>=4 & NOPE_VENC_SB_36M <15~3,
                                       TRUE~4)]

mod[,prop.table(table(Marca_NOPE_VENC_SB_36M,VarDep,useNA = "always"),1)]

datos[,prbm_NOPE_VENC_SB_36M:=case_when(NOPE_VENC_SB_36M<1~0.04110079,
                                        NOPE_VENC_SB_36M>=1 & NOPE_VENC_SB_36M <4~0.07394366,
                                        NOPE_VENC_SB_36M>=4 & NOPE_VENC_SB_36M <15~0.20588235,
                                        TRUE~0.41561713)]


#-------------------------------------------------------------- NUM_ACREED_SB_3M ------------------------------------------------------------------- 

acreed<-mod|> woebin(y='VarDep',x="NUM_ACREED_SB_3M",positive = 1, method = 'tree',bin_num_limit = 10) 
woebin_plot(acreed$NUM_ACREED_SB_3M)

mod[,Marca_NUM_ACREED_SB_3M:=case_when(NUM_ACREED_SB_3M<1~1,
                                       NUM_ACREED_SB_3M>=1 & NUM_ACREED_SB_3M<3~2,
                                       NUM_ACREED_SB_3M>=3 & NUM_ACREED_SB_3M<6~3,
                                       NUM_ACREED_SB_3M>=6 & NUM_ACREED_SB_3M<9~4,
                                       TRUE~5)]

100*mod[,prop.table(table(Marca_NUM_ACREED_SB_3M))]
mod[,prop.table(table(Marca_NUM_ACREED_SB_3M,VarDep,useNA = "always"),1)]

datos[,prbm_NUM_ACREED_SB_3M:=case_when(NUM_ACREED_SB_3M<1~0.05033809,
                                        NUM_ACREED_SB_3M>=1 & NUM_ACREED_SB_3M<3~0.05732484,
                                        NUM_ACREED_SB_3M>=3 & NUM_ACREED_SB_3M<6~0.08477998,
                                        NUM_ACREED_SB_3M>=6 & NUM_ACREED_SB_3M<9~0.16322870,
                                        TRUE~0.27340824)]

#-------------------------------------------------------------- NOPE_APERT_OP_24M ------------------------------------------------------------------- 

fc_tree_malos$NOPE_APERT_OP_24M
woebin_plot(fc_tree_malos$NOPE_APERT_OP_24M)

mod[,Marca_NOPE_APERT_OP_24M:=case_when(NOPE_APERT_OP_24M<1~1,
                                        NOPE_APERT_OP_24M==1~2,
                                        TRUE~3)]

mod[,prop.table(table(Marca_NOPE_APERT_OP_24M,VarDep,useNA = "always"),1)]

datos[,prbm_NOPE_APERT_OP_24M:=case_when(NOPE_APERT_OP_24M<1~0.0620858,
                                         NOPE_APERT_OP_24M==1~0.1257426,
                                         TRUE~0.2160633)]

datos[,prbb_NOPE_APERT_OP_24M:=case_when(NOPE_APERT_OP_24M<1~1-0.0620858,
                                         NOPE_APERT_OP_24M==1~1-0.1257426,
                                         TRUE~1-0.2160633)]


#-------------------------------------------------------------- Tipo Vivienda -------------------------------------------------------------------

#~~~~~~~~~~~~~~ Análisis en la base de modelamiento
fc_tree_malos$TIPO_VIVIENDA
woebin_plot(fc_tree_malos$TIPO_VIVIENDA)

# Vemos si la agrupacion tiene sentido
100*mod[,prop.table(table(TIPO_VIVIENDA,useNA = "always"))]
100*mod[,prop.table(table(TIPO_VIVIENDA,VarDep,useNA = "always"),1)]

# Creamos las variables agrupadas
mod[!is.na(TIPO_VIVIENDA),Marca_VIVIENDA_AGRUPADA:=case_when(TIPO_VIVIENDA %in% c("ARRENDADA","PROPIA HIPOTECADA")~1,
                                                             TIPO_VIVIENDA %in% c("PROPIA NO HIPOTECADA")~2,
                                                             TRUE~3)]


100*mod[,prop.table(table(Marca_VIVIENDA_AGRUPADA,useNA = "always"))]
mod[,prop.table(table(Marca_VIVIENDA_AGRUPADA,VarDep,useNA = "always"),1)]


#~~~~~~~~~~~~~~ Creacion de la Variable para todos los datos

# La información para la creación se obtendrá mediante el análisis de la base de modelado.
# En caso de que exista una categoría no representativa, es decir, que no aparezca en la base de modelización,
# se asignará la probabilidad de ser malo del peor caso, se toma una postura conservadora. Lo mismo se aplicará para los valores NA.

datos[,prbm_VIVIENDA:=case_when(TIPO_VIVIENDA %in% c("ARRENDADA","PROPIA HIPOTECADA") | is.na(TIPO_VIVIENDA)~0.16197183,
                                TIPO_VIVIENDA %in% c("PROPIA NO HIPOTECADA")~0.09260373,
                                TIPO_VIVIENDA=="VIVE CON FAMILIARES"~0.11846318,
                                TRUE~0.16197183)]

datos[,table(TIPO_VIVIENDA,prbm_VIVIENDA,useNA = "always")]


#-------------------------------------------------------- Almacenamiento de la Data ------------------------------------------------------------------

# Se guarda la data generada
setwd(paste(dir.p,"BDD","RData",sep="/"))
save(list = c("datos"), file = "InfoModelamiento.RData")

rm(list=setdiff(ls(),c("dir.p","dir.r","dir.s")))
gc()

# Se restaura la dirección de trabajo a la carpeta principal del proyecto
setwd(dir.p)

#---------------------------------------------------- - Análisis de Variables Final: KS-VI  ------------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~ Lectura informacion de Modelamiento:
patron <- "\\InfoModelamiento\\b"
archivos_filtrados <- list.files(dir.p, pattern = patron, full.names = TRUE, recursive = TRUE)
load(archivos_filtrados)

# Filtramos la data para los individuos  buenos y malos de la base de modelamiento (Esta sera la información a modelar)
# ModVal=1 si la información corresponde a  validación, 0 si corresponde a modelamiento
mod <- datos[ModVal == 0 & VarDep %in% c(0,1)]
dim(mod)
datos[,table(ModVal)]

# Fijamos  la dirección de la carpeta en donde se encuentra el script con las funciones auxiliares para el análisis de variables
setwd(dir.s)

source("Tidy_data.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Identificación de variables con alto porcentaje de NA's

porc <- sort(sapply(mod, porcNA), decreasing = TRUE)
PorcentajeNA <- data.frame(names(porc), as.numeric(porc))
colnames(PorcentajeNA) <- c("Var", "Porc")

# Almacenamos las variables validas
dvars <- setdiff(colnames(mod), names(porc)[porc > 0.3])
PorcentajeNA
dvars
# Numero de variables validas
length(dvars)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Identificación de variables constantes

# Nos quedamos unicamente con las variables no constantes:
dvars <- dvars[!unname(unlist(sapply(mod[,dvars,with=FALSE], constante)))]
# Numero de variables validas
length(dvars)
rm(list = c("PorcentajeNA", "porc", "porcNA", "constante"))
length(dvars)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selección de Variables Validas

# Nos quedamos con las variables validas de la base de modelamiento ( variables con un porcentaje de NA menor al 30% y que no sean constantes)
mod <- mod[, dvars, with = FALSE]

# Seleccionamos las variables numericas
vnum <- colnames(mod)[unname(sapply(mod, class)) %in% c("numeric", "integer")]
vnum<-c(vnum)
dnum <- mod[, vnum, with=FALSE]

# Seleccionamos las variables categoricas
vcat <-colnames(mod)[unname(sapply(mod, class)) %in% c("character", "logical")]

var_ing<-c(grep(colnames(mod), pattern = "prbm", value = TRUE),
           grep(colnames(mod), pattern = "prbb", value = TRUE),
           grep(colnames(mod), pattern = "MARCA", value = TRUE))

vcat<-c(vcat,var_ing)
dcat <- mod[, vcat, with=FALSE]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Ejecucion de KS sobre variables numericas

KS <- sapply(seq_along(dnum), function(i){TestKS(dnum[[i]], mod$VarDep)}) # Revisar variable dependiente
dKS <- data.frame(colnames(dnum), KS); dKS <- dKS[order(dKS$KS, decreasing = TRUE),]
colnames(dKS) <- c("Variable", "KS"); rownames(dKS) <- NULL
dKS

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Ejecucion de VI sobre variables categóricas

VI <- sort(sapply(dcat, TestVI, y=mod$VarDep), decreasing = T) # Revisar variable dependiente
dVI <- data.frame(names(VI), VI)
colnames(dVI) <- c("Variable", "VI"); rownames(dVI) <- NULL
dVI

# Se guardan los resultados del test de KS y VI en un Excel:
setwd(dir.r)
write.xlsx(list("KS_Var" = dKS, "VI" = dVI), file = "Analisis_Variables_Tratamiento_Final.xlsx")



rm(list=setdiff(ls(),c("dir.p","dir.r","dir.s")))
gc()

# Se restaura la dirección de trabajo a la carpeta principal del proyecto
setwd(dir.p)




