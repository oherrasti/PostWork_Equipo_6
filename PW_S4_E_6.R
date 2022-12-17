# Postwork Sesión 4.
# EQUIPO 6
# INTEGRANTES
# Omar Arellano Carballo
# Emmanuel Acevedo Díaz
# José Bernal Fonseca
# Israel Uriel Ramírez montoya
# Haniel Mizraim Rojo Salazar
# Oscar Herrasti Real#Desarrollo

#Desarrollo
#Instrucciones
#Utilizando la variable total_intl_charge de la base de datos telecom_service.csv de la sesión 3, 
#realiza un análisis probabilístico. Para ello, debes determinar la función de distribución 
#de probabilidad que más se acerque el comportamiento de los datos. Hint: Puedes apoyarte de 
#medidas descriptivas o técnicas de visualización.

# se cargan las librerías para análisis y graficación de datos
library(DescTools)
library(ggplot2)

"se leen los datos "
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")
" se revisan los datos cargados"
summary(df)
View(df$total_intl_charge)

Mode(df$total_intl_charge)
" la moda es el 2.7 con una frecuencia de 62"
mean(df$total_intl_charge)
" la media es 2.764, lo cual es muy cercano a la moda"
median(df$total_intl_charge)
" la mediana es de 2.78, lo cual nos confirma que la istribución parece normal"
sd(df$total_intl_charge)
" la desviación están dar es de 0.75377"
#Por los datos, se puede determinar que tiene comportamiento de distribución normal

#Una vez que hayas seleccionado el modelo, realiza lo siguiente:
  
#Grafica la distribución teórica de la variable aleatoria total_intl_charge
(hist.cargos <-
    ggplot(df, aes(x = total_intl_charge)) +
    geom_histogram(aes(y = after_stat(density)),color = "#086788", fill = "#07A0C3", position = "identity", bins = 11, alpha = 0.4) +
    geom_vline(aes(xintercept = mean(total_intl_charge)), color = "#D16014", linetype = "dashed", size = .5) + 
    labs(title = "Cargos internacionales",x = "Mediciones", y = "Densidad") + 
    theme_light()
)
ggsave("Histograma cargos intl.jpg", plot = hist.cargos)


#¿Cuál es la probabilidad de que el total de cargos internacionales sea menor a 1.85 usd?
"Calcula P(X <= 1.85):"
"se usa pnorm, por el comportamiento normal que tiene la distribución"
pnorm(q = 1.85, mean = 2.765, lower.tail = TRUE )
#la probabilidad es de 18.00958 %

#¿Cuál es la probabilidad de que el total de cargos internacionales sea mayor a 3 usd?
"Calcula P(X >= 3):"
1-pnorm(q = 2.99, mean = 2.765, lower.tail = TRUE )
#La probabilidad es de 41.09896 %

#¿Cuál es la probabilidad de que el total de cargos internacionales esté entre 2.35usd y 4.85 usd?
"Calcula P(2.35 <= X <= 4.85):"

pnorm(q = 4.85, mean = 2.765, lower.tail = TRUE ) - pnorm(q = 2.35, mean = 2.765, lower.tail = TRUE )
#La probabilidad es de 64.23944 %

#Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales más alto que podría 
#esperar?
" se usa la funcion de cuantiles"
x <- qnorm(p = 0.48, mean = 2.765, sd = 0.7537)
x
#El cargo más alto sería de 2.7271


#¿Cuáles son los valores del total de cargos internacionales que dejan exactamente al centro el 
#80% de probabilidad?
a <- qnorm(p = 0.10, mean = 2.765, sd = 0.7537)
b <- qnorm(p = 0.90, mean = 2.765, sd = 0.7537)
a
b
#del rango de 1.7999095 al de 3.730905



