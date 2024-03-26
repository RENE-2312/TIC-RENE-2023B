# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Gráficos %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este script se realizan los gráficos para el documento escrito de la TIC.

# ¡¡¡¡¡¡Aviso importante!!!!!

# No es necesario ejecutar este script de forma independiente. Se puede ejecutar desde el script 001_Ejecutar_Proyecto.R 
# para asegurar una ejecución ordenada y completa del proyecto.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\t\t\t\t\tGráficos' )
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


#--------------------------------------------------------  PASTEL: MARCA_DVEN_SB_3M --------------------------------------------------------
df<-datos[,prop.table(table(MARCA_DVEN_SB_3M))]
df<-as.data.frame(df)
df<-df|>mutate(Freq=100*Freq)

pastel <- ggplot(df, aes(x=2, y=Freq, fill=MARCA_DVEN_SB_3M)) +
  geom_bar(stat = "identity", color="black", size = 0.5, linetype="solid") +  # Ajustar el grosor del filo
  geom_text(aes(label=scales::percent(Freq/100, accuracy = 0.01)), position=position_stack(vjust=0.5), color="black", size=5) +
  coord_polar(theta = "y") +
  scale_fill_manual(values=c("steelblue","salmon" ), name = "MARCA_DVEN_SB_3M") +  # Añadir un nombre a la leyenda
  theme_void() +
  labs(title="Personas con Días de Vencido",
       subtitle=expression(italic("En el Sistema Bancario en los últimos 3 meses"))) +  # Subtítulo en cursiva
  xlim(0.5, 2.5) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  # Ajustar el tamaño del título
    plot.subtitle = element_text(hjust = 0.5, color = "black", size = 14),  # Ajustar el tamaño del subtítulo
    legend.title = element_text(face = "bold", size = 14),  # Ajustar el tamaño del título de la leyenda
    legend.position = "bottom",
    panel.background = element_rect(fill = "#DDDDDD", color = NA)  # Fondo del gráfico
  ) +
  # Ajustar el tamaño general de la leyenda
  guides(fill = guide_legend(title = "MARCA_DVEN_SB_3M", keywidth = 1.5, keyheight = 1.5, title.theme = element_text(size = 14)))
 

print(pastel)

# Se guarda el gráfico
ggsave(paste0(dir.r,"/","pastel_marca_dven_sb_3m.png"), plot = pastel, width = 10, height = 8, units = "in", dpi = 300)

#----------------------------------------------------------- MIXTO:  TIPO VIVIENDA --------------------------------------------------------------

mod[!is.na(TIPO_VIVIENDA),Marca_VIVIENDA_AGRUPADA:=case_when(TIPO_VIVIENDA %in% c("ARRENDADA","PROPIA HIPOTECADA")~3,
                                                             TIPO_VIVIENDA %in% c("PROPIA NO HIPOTECADA")~1,
                                                             TRUE~2)]

tabla_resultados <- mod |>
  group_by(Marca_VIVIENDA_AGRUPADA) |>
  summarise(
    Proporcion_A = n() / nrow(mod),  # Proporción según columna A
    Porcentaje_B = sum(VarDep == "1") / n()  # Porcentaje según CategoriaX en columna B
  )


grafico_barras<-ggplot(tabla_resultados, aes(x = Marca_VIVIENDA_AGRUPADA, y = Proporcion_A)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = scales::percent(Proporcion_A), y = Proporcion_A), vjust = 1, size = 4) +
  labs(title = "Distribución de Tipos de Vivienda",
       subtitle = expression(italic("Representatividad y Probabilidad de Malos por Agrupación")),
       x = "Grupo de Vivienda",
       y = "Frecuencia") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, color = "black", size = 14),
    #axis.text.x = element_text(angle = 45, hjust = 1)  # Girar las etiquetas del eje x para mejor legibilidad
  )

# Ajustes para las etiquetas en el gráfico de líneas
grafico_lineas <- grafico_barras +
  geom_line(aes(y = Porcentaje_B * 3), color = "red", size = 1, group = 1) +
  geom_point(aes(y = Porcentaje_B * 3), color = "red", size = 3, shape = 19) +
  geom_text(aes(label = scales::percent(Porcentaje_B), y = Porcentaje_B * 3), color = "red", vjust = -1, size = 4) +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Probabilidad de Malos"))


# Muestra el gráfico combinado
print(grafico_lineas)

# Se guarda el gráfico
ggsave(paste0(dir.r,"/","grafico_mixto_tipo_vivienda.png"), plot = grafico_lineas, width = 10, height = 8, units = "in", dpi = 300)

#---------------------------------------------------------- MIXTO: DEUDA VENCIDA SCE 12M --------------------------------------------------------------

mod[,Marca_DEUDA_VENCIDA_SCE_12M:=case_when(DEUDA_VENCIDA_SCE_12M==0~1,
                                            DEUDA_VENCIDA_SCE_12M>0 & DEUDA_VENCIDA_SCE_12M<100 ~2,
                                            DEUDA_VENCIDA_SCE_12M>=100 & DEUDA_VENCIDA_SCE_12M<260 ~3,
                                            TRUE~4)]

tabla_resultados <- mod |>
  group_by(Marca_DEUDA_VENCIDA_SCE_12M) |>
  summarise(
    Proporcion_A = n() / nrow(mod),  # Proporción según columna A
    Porcentaje_B = sum(VarDep == "1") / n()  # Porcentaje según CategoriaX en columna B
  )


grafico_barras<-ggplot(tabla_resultados, aes(x = Marca_DEUDA_VENCIDA_SCE_12M, y = Proporcion_A)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = scales::percent(Proporcion_A), y = Proporcion_A), vjust = 1, size = 3.5) +
  labs(title = "Distribución de Deuda Vencida en el SCE 12M",
       subtitle = expression(italic("Representatividad y Probabilidad de Malos por Agrupación")),
       x = "Grupo de Deuda Vencida",
       y = "Frecuencia") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    plot.subtitle = element_text(hjust = 0.5, color = "black", size = 14),
    #axis.text.x = element_text(angle = 45, hjust = 1)  # Girar las etiquetas del eje x para mejor legibilidad
  )

# Ajustes para las etiquetas en el gráfico de líneas
grafico_lineas <- grafico_barras +
  geom_line(aes(y = Porcentaje_B * 3), color = "red", size = 1, group = 1) +
  geom_point(aes(y = Porcentaje_B * 3), color = "red", size = 3, shape = 19) +
  geom_text(aes(label = scales::percent(Porcentaje_B), y = Porcentaje_B * 3), color = "red", vjust = -1, size = 3.5) +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Probabilidad de Malos"))

# Muestra el gráfico combinado
print(grafico_lineas)

# se guarda el gráfico
ggsave(paste0(dir.r,"/","grafico_mixto_deuda_vencida.png"), plot = grafico_lineas, width = 10, height = 8, units = "in", dpi = 300)


rm(list=setdiff(ls(),c("dir.p","dir.r","dir.s")))
gc()

# Se restaura la dirección de trabajo a la carpeta principal del proyecto
setwd(dir.p)

