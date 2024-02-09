#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejecutar Proyecto %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Este script se encarga de ejecutar todos los scripts del proyecto en el orden establecido. 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#--------------------------------------------------------- Configuración del Proyecto --------------------------------------------------------------------------------

# Se cargan las librerías y se establecen las direcciones base para todo el proyecto.
source( '000_Configuracion_Proyecto.R' )

#------------------------------------------------------- Procesamiento de la Información  -------------------------------------------------------------------------------

# Se realiza Ingeniería de Variables así  como la identificación de Variables candidatas para los modelos (KS-VI)
source( '200_tratamiento_identificacion_variables.R' )

#----------------------------------------------------------------- Modelos  -------------------------------------------------------------------------------

# Se realiza el modelo de credit scoring ajustandolo mediante una regresion logistica
source('300_Regresion_Logistica.R')

# Se realiza el modelo de credit scoring ajustandolo mediante un random forest bajo Validación cruzada
source('301_Random_Forest_VC.R')

# Se realiza el modelo de credit scoring ajustandolo mediante un XGB bajo Validación cruzada
source('302_XGBoost.R')

# Se realiza el cálculo de las pérdidas esperadas
source('303_Perdidas_Esperadas.R')

#----------------------------------------------------------------- Gráficos -------------------------------------------------------------------------------

# Se realizan gráficos para el overleaf de la TIC
source('400_Graficos.R')
