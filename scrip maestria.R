#--------------- librerias --------------
library(dplyr)# sirve para manipular base de datos 
library(readxl) #leer archivos en excel
library(ggplot2) # Para graficar 
library(readr) #para importar datos
library(tidyverse)
library(FactoMineR) #para recodificar
library(DescTools)
#--------------- carga de datos --------------
getwd()
setwd("C:\\Users\\Aura Guerrero\\Documents\\2023\\Investigaciones\\maestría")
archivo <- read_excel("SHARON2.xlsx")
datos<-data.frame(archivo)
str(datos)

# convertir a factores
data<-datos %>%
  mutate(across(where(is.character),as.factor))
str(data)
attributes(data)$names

##--------------- Etiquetas de las variables  -------------------
# P1 => Usted tiene un proyecto de vida a corto plazo, ósea, usted se pone metas para cumplirlas a un plazo no mayor a un año
# P2 => Planifica sus actividades diarias
# P3 => Cumple todas sus actividades diarias
# P4 => Se siente orgulloso/a de las cosas que ha logrado
# P5 => Es importante para usted, planificar sus actividades
# P6 => Si sus actividades no fueron culminadas con éxito, las vuelve a replanificar.
# P7 => En una emergencia, Ud. es alguien en quien la gente puede confiar
# P8 => Es resuelto y decidido
# P9 => Siente que puede manejar muchas situaciones a la vez
# P10 => Su reaacion frente a las dificultades es de temor al fracaso por que ya lo ha experimentado en el pasado
# P11 => Pone interes en las cosas
# P12 => Puedo encontrar, generalmente, algo sobre lo que reirse
# P13 => La seguridad en usted mismo le ayuda en los momentos dificiles
# P14 => Cuando se encuentra en una situacion dificil, por lo general puede encontrar una solucion
# P15 => Su vida tiene sentido
# P16 => En general, se toma las cosas con calma
# P17 => Se considera una persona exitosa, perseverante, optimista.
# P18 => Es una persona disciplinada
# P19 => Se lamenta por las cosas que se escapan de su control
# P20 => Tiene la energia suficiente para hacer lo que debe de hacer
# P21 => Acepta que hay personas a las que no les agrada
# P22 => Cuando las cosas le salen bien, cree que es fruto del trabajo persistente y organizado
# P23 => El éxito desde su optica, es solo para ciertas personas
# P24 => La formacion, la perseverancia y el autoaprendizaje marcan el éxito de un individuo

#-------------------- Distribución de datos ----------------------

frecuenciasP1<-data.frame(table(data$P1))
frecuenciasP1
frecuenciasP1<-as.data.frame(table(data$P1))
attach(frecuenciasP1)
tipo<-frecuenciasP1$Freq
names(tipo)<-Var1
tipo
r <- barplot(tipo, col = rainbow(20), main = "Proyecto de vida a corto plazo")
#- type = "h" plotting *is* 'bar'plot
lines(r, tipo, type = "l", col = "black", lwd = 0)

#-------------------- Entropía ----------------------
# Instalar y cargar el paquete entropy
#install.packages("entropy")
library(entropy)

# Calcular la entropía de una variable categórica
entropiaP1 <- entropy(table(data$P1))
entropiaP2 <- entropy(table(data$P2))
entropiaP3 <- entropy(table(data$P3))
entropiaP4 <- entropy(table(data$P4))
entropiaP5 <- entropy(table(data$P5))
entropiaP6 <- entropy(table(data$P6))
entropiaP7 <- entropy(table(data$P7))
entropiaP8 <- entropy(table(data$P8))
entropiaP9 <- entropy(table(data$P9))
entropiaP10 <- entropy(table(data$P10))
entropiaP11 <- entropy(table(data$P11))
entropiaP12 <- entropy(table(data$P12))
entropiaP13 <- entropy(table(data$P13))
entropiaP14 <- entropy(table(data$P14))
entropiaP15 <- entropy(table(data$P15))
entropiaP16 <- entropy(table(data$P16))
entropiaP17 <- entropy(table(data$P17))
entropiaP18 <- entropy(table(data$P18))
entropiaP19 <- entropy(table(data$P19))
entropiaP20 <- entropy(table(data$P20))
entropiaP21 <- entropy(table(data$P21))
entropiaP22 <- entropy(table(data$P22))
entropiaP23 <- entropy(table(data$P23))
entropiaP24 <- entropy(table(data$P24))

#----------------Grafico para distribucion de datos ---------
tabla_contingencia<-table(data$P2,data$P3)
grafico_barras <- ggplot(dat = as.data.frame(tabla_contingencia), aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Planificación", y = "Frecuencia", fill = "Cumplimiento en tareas") +
  theme_minimal()
print(grafico_barras)

#-------- Gráfico para crear un mosaico ----------
library(vcd)
mosaic(data$P3~data$P2,data,main = "Cumplimiento de actividades", 
       shade = TRUE, legend = TRUE)

#-------------------- Hamming ----------------------
# Instalar y cargar el paquete strngdist
#install.packages("stringdist")
library(stringdist)

# creación de la matriz
# Convierte la variable categórica en una matriz
matriz <- model.matrix(~ data$P1 - 1) #convierte la matriz en binaria
matriz
matriz1<-matrix()

# Calcula la matriz de distancias de Hamming
distancias <- dist(datos, method = "binary")
distancias
distancia_hamming <- stringdistmatrix(as.character(datos$P1),as.character(datos$P1), method = "soundex")
distancia_hamming

# Aplicar k-medias a la matriz de distancia
resultados_km <- kmeans(distancia_hamming, centers = 2)  # 'k' representa el número deseado de clusters

# Obtener las etiquetas de clasificación asignadas a cada observación
etiquetas <- resultados_km$cluster

#-------------------- Análisis de correspondencia ----------------------
resultados <- MCA(data,quali.sup = 3:4)
desc_dimensiones <- dimdesc(resultados)
resultados

# Obtener información sobre la varianza explicada
eigen(resultados)
varianza_explicada <- eigen(resultados)$eigenvalue

# Ejemplo de visualización de las categorías
plot(resultados, choix = "var")

# Ejemplo de visualización de las variables
library(factoextra)
fviz_ca(resultados, axes = c(1,5), col.var = "blue",)
