# Postwork Sesión 7.
# EQUIPO 6
# INTEGRANTES
# Omar Arellano Carballo
# Emmanuel Acevedo Díaz
# José Bernal Fonseca
# Israel Uriel Ramírez montoya
# Haniel Mizraim Rojo Salazar
# Oscar Herrasti Real

# Desarrollo
# Utilizando el siguiente vector numérico, realiza lo que se indica:
  
# se leen los datos  
  url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-07/Data/global.txt"

# se asignan a la variable Global
  Global <- scan(url, sep="")
  head(Global)
  class(Global)
  Global

# Crea un objeto de serie de tiempo con los datos de Global. 
# La serie debe ser mensual comenzando en Enero de 1856
  
# con la funcion ts, pasamos del data frame a una serie de tiempo  
# se inicia en el 1856 y se hace con frecuencia 12 para que sea mensual  
  Global.ts <- ts(Global, start = c(1856,1), frequency = 12)
# se comprueba que se tiene una variable de serie de tiempo = ts
  class(Global.ts)
# se revisan los datos de la serie de tiempo
  Global.ts
  cycle(Global.ts)
  summary(Global.ts)
  
# Realiza una gráfica de la serie de tiempo anterior
  plot(Global.ts, 
       main = "Serie de tiempo", 
       xlab = "Tiempo",
       sub = "Enero de 1856 - Diciembre de 2005")
  
  
# Ahora realiza una gráfica de la serie de tiempo anterior, transformando a la primera diferencia
# se calcula la primera diferencia con la siguiente función  
  diff(Global.ts)
# se genera la gráfica usando la función de la primera diferencia  
  plot(diff(Global.ts), type = "l", main = "Primera diferencia de Global", 
       xlab = "tiempo", ylab = expression(x[t]), 
       sub = expression(x[t]==x[t-1]+w[t]))
# se muestra un análisis más a detalle de la composición de la serie de tiempo a la
#  primera diferencia
  plot(decompose(diff(Global.ts)))
  summary(diff(Global.ts))
  acf(diff(Global.ts))
  pacf(diff(Global.ts))
  
# ¿Consideras que la serie es estacionaria en niveles o en primera diferencia?
# La serie no es estacionaria en niveles pues presenta una tendencia
# La serie se vuelve estacionaria en primera diferencia  
  
  
# Con base en tu respuesta anterior, obtén las funciones de 
# autocorrelación y autocorrelación parcial?
# función de autocorrelación  
  acf(diff(Global.ts))
# función de autocorrelación parcial
  pacf(diff(Global.ts))
  
  
