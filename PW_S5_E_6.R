# Postwork Sesión 5.
# EQUIPO 6
# INTEGRANTES
# Omar Arellano Carballo
# Emmanuel Acevedo Díaz
# José B
# Israel uriel
# Haniel Mizraim Rojo Salazar
# Oscar Herrasti Real

#OBJETIVO
#Realizar inferencia estadística para extraer información de la muestra que sea 
#contrastable con la población
#DESARROLLO
#El data frame iris contiene información recolectada por Anderson sobre 50 flores 
#de 3 especies distintas (setosa, versicolor y virginca), incluyendo medidas en 
#centímetros del largo y ancho del sépalo así como de los pétalos.

#Estudios recientes sobre las mismas especies muestran que:
  
#En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es igual a 5.7 cm
#con base en lo planteado tenemos las siguientes hipotesis
# hipótesis nula        Ho = Sepal.Length == 5.7
# hipótesis alternativa Ha = Sepal.Length != 5.7
#mu = 5.7
# se ejecuta la función para obtener el valor de p
# la prueba se hace de dos lados por ser el valor de la media el que se evalua
t.test(iris[iris$Species == 'setosa', "Sepal.Length"], 
       alternative = 'two.sided', mu=5.7)
#p-value = 2.2e-16
# el valor de p es muy pequeño y para un nivel de 99% se concluye: p < 0.01
# Existe evidencia para rechazar Ho, el largo del sépalo en promedio no es de 5.7 cm



#En promedio, el ancho del pétalo de la especie virginica (Petal.Width) es menor a 2.1 cm
#con base en lo planteado tenemos las siguientes hipotesis
# hipótesis nula        Ho = Sepal.Length >= 2.1
# hipótesis alternativa Ha = Sepal.Length < 2.1 prueba de cola inferior
# mu 2.1
# se ejecuta la función para obtener el valor de p
# la prueba se hace del lado menor por querer saber si es menor a 2.1
t.test(iris[iris$Species == 'virginica', "Petal.Width"], 
       alternative = 'less', mu=2.1)
#p-value = 0.03132
# Con el valor de p para un nivel de 99% se concluye: p > 0.01
# Existe evidencia para no rechazar Ho, el ancho del pétalo en promedio es mayor a 2.1 cm



#En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más grande 
#que el promedio del largo del pétalo de la especie versicolor.
#con base en lo planteado tenemos las siguientes hipotesis
# hipótesis nula        Ho = virginica.petal.Length <= versicolor.petal.Length
# hipótesis alternativa Ha = virginica.petal.Length > versicolor.petal.Length
# mu 1.1
# se ejecuta la función para obtener el valor de p
t.test(x = iris[iris$Species == "virginica", "Petal.Length"],
       y = iris[iris$Species == "versicolor", "Petal.Length"],
       alternative = "greater", mu = 1.1, var.equal = FALSE)

#p-value = 0.03206
# Con el valor de p para un nivel de 99% se concluye: p > 0.01
# Existe evidencia para no rechazar Ho, el largo de la especie virginica no es mayor al
# dela especie versicolor



#En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies.
#con base en lo planteado tenemos las siguientes hipotesis
# hipótesis nula        Ho = virginica.sepal.Width = versicolor.sepal.width = setosa.sepal.width
# hipótesis alternativa Ho = virginica.sepal.Width != versicolor.sepal.width != setosa.sepal.width

library(ggplot2)
str(iris)

boxplot(log(iris$Sepal.Width))

boxplot(log(iris$Sepal.Width) ~ Species,
        data = iris)
# analizando las gráficas de boxplot generadas, se puede ver que cada especie tiene anchos de
# sépalos diferentes
# hacemos el análisis ANOVA
anova <- aov(log(iris$Sepal.Width) ~ Species,
             data = iris)

summary(anova)
# el valor de p es 2.29e-16 por ser menor a 0.01 por el 99% de confianza, se rechaza Ho, y 
# se concluye que si hay diferencia en los anchos del sépalo entre las tres especies.



#Utilizando pruebas de inferencia estadística, concluye si existe evidencia suficiente 
#para concluir que los datos recolectados por Anderson están en línea con los nuevos estudios.

#Utiliza 99% de confianza para toda las pruebas, en cada caso realiza el planteamiento 
#de hipótesis adecuado y concluye.

