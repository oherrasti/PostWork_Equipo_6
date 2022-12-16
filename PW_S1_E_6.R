# Postwork Sesión 1.
# EQUIPO 6
# INTEGRANTES
# Omar Arellano Carballo
# Emmanuel Acevedo Díaz
# José B
# Israel uriel
# Haniel Mizraim Rojo Salazar
# Oscar Herrasti Real

#### Desarrollo

#1. Del siguiente enlace, descarga los datos de soccer de la temporada 2019/2020 
#de la primera división de la liga española: https://www.football-data.co.uk/spainm.php
"Se descarga el archivo como CSV a nuestro equipo"

#2. Importa los datos a R como un Dataframe.
" Se carga  libreraía para lectura de los datos"
library(readr)
" Se lee el archivo y se pasa a variable SP1"
SP1 <- read_csv("SP1.csv")
" Se revisan los datos cargados"
View(SP1)
" Se ve el número de columnas de los datos"
length(SP1) 
" Comando para en concreto ver las columnas"
ncol(SP1)
" Comando para ver los renglones"
nrow(SP1)
" Ver qué tipo de dato es: se trata de una tabla"
class(SP1)

#3. Del dataframe que resulta de importar los datos a `R`, extrae las columnas 
#que contienen los números de goles anotados por los equipos que jugaron en casa 
#(FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG); 
#guárdalos en vectores separados
" extracción de datos, los goles en casa"
gol_casa <- c(SP1$FTHG)
gol_casa
" extracción de datos, los goles de visitante"
gol_visita <- c(SP1$FTAG)
gol_visita

#4. Consulta cómo funciona la función `table` en `R`. 
help("table")
#5. Responde a las siguientes preguntas:
#  a) ¿Cuántos goles tuvo el partido con mayor empate?

#recorre el data set buscando los empates y los guarda en otro data set

empate <- 0
x <- 1
partido_empatado <- c()
goles_empate <- c()
length(gol_casa)
for(x in 1: length(gol_casa)) {
  if (gol_casa[x] == gol_visita[x]) #es empate
    {
    if (length(partido_empatado) == 0)
    {
      empate <- empate + 1
      partido_empatado <- c(x)
      goles_empate <- c(gol_casa[x])
    }
    if (length(partido_empatado) != 0)
    {
      empate <- empate + 1
    
      partido_empatado <- c(partido_empatado,x)
      goles_empate <- c(goles_empate,gol_casa[x])
      print(x)
    }
    }
}

empate
" hubo 105 empates"
length(partido_empatado)
length(goles_empate)

goles_mayor_empate <- 0
partido_mayor_empate <- 0

max(goles_empate)

for(x in 1: length(goles_empate)) {
  
  if (goles_mayor_empate < goles_empate[x]) #es empate mayor
  {
    goles_mayor_empate <- goles_empate[x]
    goles_mayor_empate
    partido_mayor_empate <- partido_empatado[x]
    partido_mayor_empate
  }
}
print("la mayor cantidad de goles en un empate fue:")
goles_mayor_empate
" La respuesta es 4, es decir el mayor número de goles en un empate fue de 4 por cada equipo"
print("y se metieron en el partido número:")
partido_mayor_empate
" El partido 6 fue el de mayor goles en empate por 4 por cada bando"

#  b) ¿En cuántos partidos ambos equipos empataron 0 a 0?
partido_empatado_cero <- 0
for(x in 1: length(goles_empate)) {
  print(x)
  if (goles_empate[x] == "0") #es empate a cero
  {
    
    partido_empatado_cero <- partido_empatado_cero + 1
    
  }
}
print("la cantidad de partidos con empate a cero:")
partido_empatado_cero
" en 33 partidos se dio el empate a cero"

#  c) ¿En cuántos partidos el equipo local (HG) tuvo la mayor goleada sin dejar 
# que el equipo visitante (AG) metiera un solo gol?
partido_gana_local <- c()
goles_local <- c()
goles_visita <- 0

for(x in 1: length(gol_casa)) {
  if (gol_casa[x] > "0" & gol_visita[x] == "0") #es ganado por el local con cero goles de visitante
  {
    partido_gana_local <- c(partido_gana_local,x)
    goles_local <- c(goles_local,gol_casa[x])
    print(x)
  }
}
partido_gana_local
goles_local
mayor_goleada <- max(goles_local)
mayor_goleada
" la mayor goleada fue 6 a 0"
partidos_mayor_goleada <- c()
for(x in 1: length(goles_local)) {
  if (mayor_goleada == goles_local[x]) #es un partido con mas goles
  {
    partidos_mayor_goleada <- c(partidos_mayor_goleada,partido_gana_local[x])
    
  }
}
print("la mayor goleada fue de ") 
mayor_goleada 
print(" goles")
print("los partidos con mayor goleada fueron los número ") 
partidos_mayor_goleada
" el partido con mayor goleada fue el 298 con 6 a 0"
 