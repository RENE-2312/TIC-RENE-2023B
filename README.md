# TIC RENE 2023B

## *Evaluación de modelos de machine learning aplicados al cálculo de pérdidas esperadas en entidades de microfinanzas*

En este repositorio se presentan los codigos para replicar los resultados de mi proyecto de titulación, que puede ser revisado en el siguiente enlace. El proyecto consisitio en evaluar el impacto sobre el cálculo de las pérdidas esperadas que tienen los algoritmos de Machine Learning (Random Forest y XGBoost ) respecto a la metodología tradicional (Logit). Para ello, se trabajo con los datos de microcrédito y de consumo de una institución financiera ecuatoriana, donde, los datos necesarios para poder replicar los resultados se encuentran en el archivo `InfoTratamientoInicial.RData` almacenado en la subcarpeta RData de la carpeta BDD de este repositorio. 

Se estimo la Pérdida Esperada, $PE$, a través de lo determinado por la Superintendencia de Bancos del Ecuador:
$$PE= EAD \cdot LGD \cdot PD$$

Donde:
- $EAD$, es el nivel de exposición del riesgo de crédito.
- $LGD$ , es la severidad de la pérdida.
- $PD$, es la probabilidad de default.

En este proyecto se trabajo con el enfoque básico según Basilea II, es decir, se trabajo con una $LGD=45%$ y se determino la probabilidad de default a través de tres modelos, Regresión Logística, Random Forest y XGBoost.

## Objetivo 
Construir modelos analíticos de Machine Learning que permitan estimar la probabilidad de incumplimiento de una persona natural para hacer frente a sus obligaciones crediticias en una entidad financiera.
Proyecto de Titulación: Cálculo de Pérdidas Esperadas basado en 3 modelos de Credit Scoring (Regresión Logística, Random Forest y XGBoost) para una institución financiera del Ecuador.

#

