# Postwork Sesión 6.
# EQUIPO 6
# INTEGRANTES
# Omar Arellano Carballo
# Emmanuel Acevedo Díaz
# José B
# Israel uriel
# Haniel Mizraim Rojo Salazar
# Oscar Herrasti Real

# Planteamiento:
# Supongamos que nuestro trabajo consiste en aconsejar a un cliente sobre como mejorar las 
# ventas de un producto particular, y el conjunto de datos con el que disponemos son datos 
# de publicidad que consisten en las ventas de aquel producto en 200 diferentes mercados, 
# junto con presupuestos de publicidad para el producto en cada uno de aquellos mercados 
# para tres medios de comunicación diferentes: TV, radio, y periódico. No es posible para 
# nuestro cliente incrementar directamente las ventas del producto. Por otro lado, ellos 
# pueden controlar el gasto en publicidad para cada uno de los tres medios de comunicación. 
# Por lo tanto, si determinamos que hay una asociación entre publicidad y ventas, entonces 
# podemos instruir a nuestro cliente para que ajuste los presupuestos de publicidad, y así 
# indirectamente incrementar las ventas.

# Objetivo
# En otras palabras, nuestro objetivo es desarrollar un modelo preciso que pueda ser usado 
# para predecir las ventas sobre la base de los tres presupuestos de medios de comunicación. 
# Ajuste modelos de regresión lineal múltiple a los datos advertisement.csv y elija el modelo 
# más adecuado siguiendo los procedimientos vistos

#Considera:
  
#  Y: Sales (Ventas de un producto)
# X1: TV (Presupuesto de publicidad en TV para el producto)
# X2: Radio (Presupuesto de publicidad en Radio para el producto)
# X3: Newspaper (Presupuesto de publicidad en Periódico para el producto)
#se leen los datos:
adv <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/advertising.csv")

adv

# se cargan las librerías para el análisis
library(dplyr)
library(ggplot2)
#MATRIZ DE CORRELACION
round(cor(adv),4)  

#           TV      Radio     Newspaper   Sales
# TV        1.0000  0.0548    0.0566      0.9012
# Radio     0.0548  1.0000    0.3541      0.3496
# Newspaper 0.0566  0.3541    1.0000      0.1580
# Sales     0.9012  0.3496    0.1580      1.0000

# se grafica las correlaciones
pairs(~ Sales + TV + Radio + Newspaper, 
      data = adv, gap = 0.4, cex.labels = 1.5)
# por las graficas se podría indicar que hay una relación entre las ventas y la
# publicidad por TV, el resto muestra mucha dispersión lo que indica baja corelación.
# Con esta prueba se puede sugerir que incrementando la publicidad de la TV, las ventas
# pueden subir.

# Estimación por Mínimos Cuadrados Ordinarios (OLS)
# se cargan los datos en R para que se puedan obtener en el análisis
attach(adv)
# Se aplica la función de modelo lineal, la variable dependiente son las ventas
m1 <- lm(Sales ~ TV + Radio + Newspaper)
summary(m1)
help(lm)
# Call:
# lm(formula = Sales ~ TV + Radio + Newspaper)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.3034 -0.8244 -0.0008  0.8976  3.7473 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 4.6251241  0.3075012  15.041   <2e-16 ***
#   TV          0.0544458  0.0013752  39.592   <2e-16 ***
#   Radio       0.1070012  0.0084896  12.604   <2e-16 ***
#   Newspaper   0.0003357  0.0057881   0.058    0.954    
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 1.662 on 196 degrees of freedom
# Multiple R-squared:  0.9026,	Adjusted R-squared:  0.9011 
# F-statistic: 605.4 on 3 and 196 DF,  p-value: < 2.2e-16

# la relación entre las ventas y la publicidad en los medios es positiva
# significa que si aumenta la publicidad aumentan las ventas.
# de acuerdo a este análisis el radio representa la mejor opción,
# despuésla TV y al final el Radio.
# el 90.26 % de la variación de las ventas se explica por la publicidad

mTV <- lm(Sales ~ TV)
summary(mTV) 
#Multiple R-squared:  0.8122

mRad <- lm(Sales ~ Radio)
summary(mRad)  
#Multiple R-squared:  0.1222

mNP <- lm(Sales ~ Newspaper)
summary(mNP) 
#Multiple R-squared:  0.02495

# La relación más fuerte se da entre las ventas y la TV

# Análisis por residuos estandarizados
StanRes2 <- rstandard(m1)

par(mfrow = c(2, 2))

plot(TV, StanRes2, ylab = "Residuales Estandarizados")
plot(Radio, StanRes2, ylab = "Residuales Estandarizados")
plot(Newspaper, StanRes2, ylab = "Residuales Estandarizados")
#compara cuantiles de una dist normal contra los cuantiles de estra distribución
qqnorm(StanRes2)
#agrega la línea de 45° para mejorar el análisis
qqline(StanRes2)

# conclusión
# De acuerdo al análisis, si existe relación entre la publicidad y las ventas
# la variación de las ventas tiene una fuerte relación con la publicidad
# por gráficas la mejor relación es entre la televisión y las ventas
# por el modelo lineal el radio tiene mayor peso.

