# Postwork Sesión 3.
# EQUIPO 6
# INTEGRANTES
# Omar Arellano Carballo
# Emmanuel Acevedo Díaz
# José Bernal Fonseca
# Israel Uriel Ramírez montoya
# Haniel Mizraim Rojo Salazar
# Oscar Herrasti Real

#### Objetivo

#- Realizar un análisis descriptivo de las variables de un dataframe
#### Desarrollo

"Utilizando el dataframe `boxp.csv` realiza el siguiente análisis descriptivo. 
No olvides excluir los missing values y transformar las variables a su
tipo y escala correspondiente."
" Se cargan las librelias para el análisis de los datos"
# manipulación de datos
library(dplyr)
# análisis estadístico
library(DescTools)
# visualización de gráficas
library(ggplot2)
# análisis de la distribución de los datos
library(moments)
# se cargan los datos a analizar
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/boxp.csv")
# se analizan los datos cargados
class(df)
str(df)
dim(df)
View(df)
#1) Calcula e interpreta las medidas de tendencia central de la variable `Mediciones`
"primero se eliminan datos basura o incompletos"
datos_limpios <- df[complete.cases(df),]
View(datos_limpios)
" se calcula la media"
mean(datos_limpios$Mediciones)  
# la media es = 62.88494
mediana <- median(datos_limpios$Mediciones)
# la mediana es = 49.3 y es donde se dividen los datos a la mitad
moda <- Mode(datos_limpios$Mediciones)
# la moda es el número 23.3 que tiene una frecuencia = 6

#2) Con base en tu resultado anterior, ¿qué se puede concluir respecto al sesgo de `Mediciones`?
" se genera el histograma"
hist(datos_limpios$Mediciones)
# Los datos tienen sesgo a la derecha, por eso la media y la mediana están separadas


#3) Calcula e interpreta la desviación estándar y los cuartiles de la distribución de `Mediciones`
var(datos_limpios$Mediciones)
#la varianza es = 2891.183
sd(datos_limpios$Mediciones)
# la desviación estándar es = 53.76972
# lo anterior nos idica que los datos se dispersan de la media en 53.769
cuartiles <- quantile(datos_limpios$Mediciones, probs = c(0.25, 0.50, 0.75))
cuartiles
# 25%   50%   75% 
# 23.45 49.30 82.85 
# lo anterior se interpreta como:
# el 25% de los datos son menor o igual a 23.45
# el 50% de los datos es menor o igual a 49.30
# el 75% de los datos es menor o igual a 82.85

"4) Con ggplot, realiza un histograma separando la distribución de `Mediciones` por `Categoría`
¿Consideras que sólo una categoría está generando el sesgo?"

" se grafican los datos limpíos"
ggplot(datos_limpios, aes(Mediciones)) +
  geom_histogram(bins = 8) + 
  labs(title = "Histograma", 
       x = "Medición",
       y = "Frequency") + 
  theme_classic()
" se grafican los datos resaltando los valores de tendencia central para analizar el sesgo"
ggplot(datos_limpios, aes(x=Mediciones, color=Categoria)) +
  geom_histogram(bins=10,fill="white", alpha=0.99, position="dodge") +
  geom_vline(aes(xintercept=mean(Mediciones)),
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=moda),
             color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mediana),
             color="orange", linetype="dashed", size=1)

"No parece que el sesgo a la derecha lo cause una sola categoría"


"5) Con ggplot, realiza un boxplot separando la distribución de `Mediciones` por `Categoría` 
y por `Grupo` dentro de cada categoría. ¿Consideras que hay diferencias entre categorías? 
¿Los grupos al interior de cada categoría podrían estar generando el sesgo?"
" se grafica por categoria"
boxplot(datos_limpios$Mediciones ~ datos_limpios$Categoria)
" se grafica por grupo"
boxplot(datos_limpios$Mediciones ~ datos_limpios$Grupo)
" tanto por categoría como por grupo, se observan mediciones muy separadas de la media
Hay Diferencia de distribucion entre las categorias, se presenta sesgo debido a la 
concenstracion de datos,
Tambien se denotan datos fuera de rango."
