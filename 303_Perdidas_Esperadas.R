# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modelo Regresion Logistica %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este script se calculan las perdidas esperadas, para ello primero es necesario abrir y  guardar manualmente las tablas
# performance generadas para poder realizar los calculos corrrespondientes.

# ¡¡¡¡¡¡Aviso importante!!!!!

# No es necesario ejecutar este script de forma independiente. Se puede ejecutar desde el script 001_Ejecutar_Proyecto.R 
# para asegurar una ejecución ordenada y completa del proyecto.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\t\t\t\t\tPerdidas Esperadas' )
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

#---------------------------------------------------------- Lectura Resultados: RGL - PD ------------------------------------------------------------

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


#---------------------------------------------------------- Lectura Resultados: RF - PD  ------------------------------------------------------------

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

#---------------------------------------------------------- Lectura Resultados: XGB - PD ------------------------------------------------------------

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

#------------------------------------------------------- EAD  -------------------------------------------------------------

resultados$EAD<-datos$VALOR_CREDITO
resultados$VarDep<-datos$VarDep
resultados$Fecha<-datos$FECHA_CORTE

#------------------------------------------------------- LGD -------------------------------------------------------------

resultados[,LGD:=0.45]

#--------------------------------------------- Calculo Perdida Esperada  -------------------------------------------------------------------------

resultados[,PE_RGL:=EAD*LGD*r_malo_int_RGL]
resultados[,PE_RF:=EAD*LGD*r_malo_int_RF]
resultados[,PE_XGB:=EAD*LGD*r_malo_int_XGB]

resultados[,r_PE_RGL:=PE_RGL/EAD]
resultados[,r_PE_RF:=PE_RF/EAD]
resultados[,r_PE_XGB:=PE_XGB/EAD]

#------------------------------------------------ Gráficos -----------------------------------------------------------
ggplot(resultados, aes(x = PE_XGB)) +
  geom_density(fill = "steelblue", color = "black") +
  labs(title = "Distribución de Pérdidas - Modelo: RGL ", x = "Pérdida Esperada (PE)") +
  theme_minimal()

mean(resultados$PE_RGL)

quantile(resultados$PE_RGL,probs = c(0.999))

#-------------------------------------------------- Comparación --------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Modelización: RGL
ratios_perdidas<-c()

mod <- resultados[ModVal == 0 & Rango_RGL<=10]
r_PE<-sum(mod$PE_RGL)/sum(mod$EAD)
ratios_perdidas<-c(ratios_perdidas,r_PE)

mod <- resultados[ModVal == 0 & Rango_RF<=10]
r_PE<-sum(mod$PE_RF)/sum(mod$EAD)
ratios_perdidas<-c(ratios_perdidas,r_PE)

mod <- resultados[ModVal == 0 & Rango_XGB<=10]
r_PE<-sum(mod$PE_XGB)/sum(mod$EAD)
ratios_perdidas<-c(ratios_perdidas,r_PE)

data_ratios<-data.frame(Modelos=c("RGL","RF","XGB"),r_Modelizacion=ratios_perdidas)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Validación: RGL

ratios_perdidas<-c()
val <- resultados[ModVal == 1  & Rango_RGL<=10]
r_PE<-sum(val$PE_RGL)/sum(val$EAD)
ratios_perdidas<-c(ratios_perdidas,r_PE)

val <- resultados[ModVal == 1  & Rango_RF<=10]
r_PE<-sum(val$PE_RF)/sum(val$EAD)
ratios_perdidas<-c(ratios_perdidas,r_PE)

val <- resultados[ModVal == 1  & Rango_XGB<=100]
r_PE<-sum(val$PE_XGB)/sum(val$EAD)
ratios_perdidas<-c(ratios_perdidas,r_PE)

data_ratios$r_Validacion<-ratios_perdidas

# mod <- resultados[ModVal == 0 & Rango_RGL==7]
# r_PE<-sum(mod$PE_RGL)/sum(mod$EAD)
# mod <- resultados[ModVal == 0 & Rango_RF==7]
# r_PE<-sum(mod$PE_RF)/sum(mod$EAD)
# mod <- resultados[ModVal == 0 & Rango_XGB==7]
# r_PE<-sum(mod$PE_XGB)/sum(mod$EAD)


#----------------------------------------------------- Grafica Temporal -----------------------------------------

resultados<-resultados|>mutate(Fecha=yearmonth(Fecha))


serie <- resultados |> 
  select(Fecha, ModVal, PE_RGL, EAD) |> 
  rename(PE = PE_RGL)
serie$Modelo<-"RGL"

info <- resultados |> 
  select(Fecha, ModVal, PE_RF, EAD) |> 
  rename(PE = PE_RF)
info$Modelo<-"RF"

serie<-rbind(serie, info)


info <- resultados |> 
  select(Fecha, ModVal, PE_XGB, EAD) |> 
  rename(PE = PE_XGB)
info$Modelo<-"XGB"

serie<-rbind(serie, info)

serie<-serie|>filter(ModVal==1)

serie<-serie|>select(-ModVal)

serie<-serie|>group_by(Fecha,Modelo)|>summarise(EAD=sum(EAD),
                                                PE=sum(PE))

serie<-as.data.frame(serie)
serie<-serie|>mutate(r_perdida=PE/EAD)

serie_medias<-serie|>group_by(Modelo)|>summarise(r_promedio=mean(r_perdida))

serie<-serie|>as_tsibble(index=Fecha,key=Modelo)

grafico_series<-serie|>autoplot(100*r_perdida) +
  labs(title = "Ratio de Pérdida por Modelo",
       subtitle = expression(italic("De: Agosto 2020 a Julio 2021")),
       x = "Mes de Desempeño",
       y = "Ratio de Pérdida [%]")+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, color = "black", size = 14),
    #axis.text.x = element_text(angle = 45, hjust = 1)  # Girar las etiquetas del eje x para mejor legibilidad
  )

# Muestra el gráfico combinado
print(grafico_series)

# Se guarda el gráfico
ggsave(paste0(dir.r,"/","grafico_series_ratio_perdidas.png"), plot = grafico_series, width = 10, height = 8, units = "in", dpi = 300)

#----------------------------------------------------- Almacenamiento de Resultados -----------------------------------------------------------------

# Se guarda la data generada paa este modelo
setwd(paste(dir.p,"BDD","RData",sep="/"))
save(list = c("resultados","data_ratios","serie_medias","serie"), file = "Resultados_Malos.RData")


rm(list=setdiff(ls(),c("dir.p","dir.r","dir.s")))
gc()

# Se restaura la dirección de trabajo a la carpeta principal del proyecto
setwd(dir.p)
