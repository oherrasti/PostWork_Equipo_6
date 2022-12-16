# Postwork Sesión 2.
# EQUIPO 6
# INTEGRANTES
# Omar Arellano Carballo
# Emmanuel Acevedo Díaz
# José B
# Israel uriel
# Haniel Mizraim Rojo Salazar
# Oscar Herrasti Real

#### Desarrollo
#Objetivo
"Conocer algunas de las bases de datos disponibles en R
Observar algunas características y manipular los DataFrames con dplyr
Realizar visualizaciones con ggplot
1.	Inspecciona el DataSet iris_meaniris disponible directamente en R. Identifica 
las variables que contiene y su tipo, asegúrate de que no hayan datos faltantes 
y que los datos se encuentran listos para usarse.

Se léen los datos y se pasa data set a una variable para su análisis"
ds <- iris
"data frame con los datos"
class(ds)
"descripción de la estructura"
str(ds)
"primeros 6  filas"
head(ds)
"clase del objeto <- data frame"
class(ds)
"ver que dimensión tiene <- 150 5"
dim(ds)
"ver los datos"
View(ds)
"limpieza de datos, validar que no haya NA, todos son TRU <- no hay NA"
complete.cases(ds)

"2.	Crea una gráfica de puntos que contenga Sepal.Lenght en el eje horizontal, 
Sepal.Width en el eje vertical, que identifique Species por color 
y que el tamaño de la figura está representado por Petal.Width. 
Asegúrate de que la geometría contenga shape = 10 y alpha = 0.5.

se carga librería de gráficas"
library(ggplot2)

"se ejecuta la función para crear la gráfica considerando el eje x sepal.length
y las y con sepal.with y se agrega la geometría shape con valor 10 y alpha con valor 0.5"

ggplot(ds, aes(x = ds$Sepal.Length, y = ds$Sepal.Width, 
              color = ds$Species, size = ds$Petal.Width )) +
  geom_point(shape = 10, alpha = 0.5)

"3.	Crea una tabla llamada iris_mean que contenga el promedio de todas las variables 
agrupadas por Species.

cargamos la libreria para el manejo de tablas"
library(dplyr)

"traemos los datos"
ds
"se cargan los datos con las siguientes consideraciones, 
se saca la media de las variables length, width de sepal y length, width de petal
estos valores se agrupan para la media con la variable species"
iris_mean <- ds %>%
          group_by(Species) %>%
          summarize(media_sepal_len = mean(Sepal.Length),
                   media_sepal_wid = mean(Sepal.Width),
                   media_petal_len = mean(Petal.Length),
                   media_petal_wid = mean(Petal.Width))
head(iris_mean)
"resutado completo:"
iris_mean
# A tibble: 3 × 5
"Species    media_sepal_len media_sepal_wid media_petal_len media_petal_wid
<fct>                <dbl>           <dbl>           <dbl>           <dbl>
  1 setosa                5.01            3.43            1.46           0.246
2 versicolor            5.94            2.77            4.26           1.33 
3 virginica             6.59            2.97            5.55           2.03 

4.	Con esta tabla, agrega a tu gráfica anterior otra geometría de puntos 
para agregar los promedios en la visualización. 

Asegúrate que el primer argumento de la geometría sea el nombre de tu tabla y 
que los parámetros sean shape = 23, size = 4, fill = black y stroke = 2. 
También agrega etiquetas, temas y los cambios necesarios para mejorar tu visualización.

se carga librería de gráficas, se pone como referencia pues no se requiere cargarla dos veces"
library(ggplot2)

"se ejecuta la función, con las siguintes consideraciones
grafica original y se graban sus características en la variable graph"
graph <- ggplot(ds, aes(x = Sepal.Length, y = Sepal.Width, 
                        color = Species, size = Petal.Width )) +
  geom_point(shape = 10, alpha = 0.5)

"se pinta la gráfica para ver el resultado"
graph

"se añaden a la función nuevas características y los datos de las medias para incorporarlos
a la gráfica"

graph <- graph +  
  geom_point(data = iris_mean, shape = 23, size = 4, fill = "black", stroke = 2)

"se pinta la gráfica para ver el resultado"
graph  



