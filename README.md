# TIC RENE 2023B

## Evaluación de modelos de machine learning aplicados al cálculo de pérdidas esperadas en entidades de microfinanzas

Este repositorio contiene los códigos necesarios para replicar los resultados de mi proyecto de titulación, el cual puede ser revisado en el siguiente enlace. El objetivo principal del proyecto fue evaluar el impacto de los algoritmos de Machine Learning (Random Forest y XGBoost) en el cálculo de las pérdidas esperadas en comparación con la metodología tradicional (Logit). Para ello, se trabajó con información de microcréditos y créditos de consumo de una institución financiera ecuatoriana. Los datos necesarios para replicar los resultados se encuentran en el archivo `InfoTratamientoInicial.RData`, almacenado en la carpeta BDD de este repositorio.

La pérdida esperada, $PE$, se estimó de acuerdo con lo determinado por la Superintendencia de Bancos del Ecuador:

$$ PE = EAD \cdot LGD \cdot PD$$

Donde:

- $EAD$ es el nivel de exposición del riesgo de crédito.
- $LGD$ es la severidad de la pérdida.
- $PD$ es la probabilidad de incumplimiento.

Se siguió el enfoque básico según Basilea II, lo que implica que $LGD = 45\\%$. Se determinó la probabilidad de incumplimiento a través de tres modelos de Credit Scoring: Regresión Logística (RGL), Random Forest (RF) y XGBoost (XGB), con el propósito de comparar sus resultados.

## Repositorio

La estructura del repositorio es la siguiente:

### Carpetas

- **BDD**: Contiene una subcarpeta llamada RData, donde se encuentran almacenadas las bases en formato RData utilizadas y generadas a lo largo del proyecto.
- **Scripts Adicionales**: Aquí se encuentra el script `Tidy_data.R`, que contiene funciones utilizadas para la selección de variables como el Test de Kolmogorov-Smirnov (KS) y el test de Valor de Información (VI).
- **Resultados**: Incluye los resultados del Análisis Exploratorio de los Datos, así como los resultados del Test KS y VI para la selección de variables. Además, presenta los resultados de las grillas de hiperparámetros para los modelos RF y XGB, junto con la plantilla de las tablas performance y las tablas resultantes para cada modelo. También, se incluyen los gráficos generados para el documento de la TIC.

### Scripts

El orden para ejecutar los scripts es el siguiente:

1. `000_Configuracion_Proyecto.R`: Carga las librerías y establece las direcciones base para todo el proyecto.
2. `200_tratamiento_identificacion_variables.R`: Realiza la Ingeniería de Variables y la identificación de Variables candidatas para los modelos (KS-VI).
3. `300_Regresion_Logistica.R`: Realiza el modelo de credit scoring ajustándolo mediante una regresión logística.
4. `301_Random_Forest_VC.R`: Realiza el modelo de credit scoring ajustándolo mediante un random forest bajo Validación Cruzada.
5. `302_XGBoost.R`: Realiza el modelo de credit scoring ajustándolo mediante un XGB bajo Validación Cruzada.
6. `303_Perdidas_Esperadas.R`: Calcula las pérdidas esperadas.
7. `400_Graficos.R`: Genera gráficos para el documento de la TIC.

Además, se puede ejecutar el proyecto de forma ordenada a través del script `001_Ejecutar_Proyecto.R`.
