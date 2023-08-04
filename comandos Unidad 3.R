# Algunas definiciones

# El vector

vector<-c(4,2,-8)

v <- c(8, 7, -3, 2, 182)
v[5] # El quinto elemento

#seleccionar una secuencia de elementos

v <- rnorm(1000)
head(v)
plot(v)
(subv <- v[5:15])
plot(subv, col="red")

(sub1.v <- v[c(2, 3, 5:8)])
plot(sub1.v)

#Los índices negativos
sub2.v <- v[-c(2, 3, 5:8)]
plot(sub2.v)

# extraer valores de una data.frame

?trees

datos<-trees
summary(trees)
datos[1:10, ] # Selecciono las  primeras 10 filas
datos[,1] # Selecciono la primer columna
datos[1,1] # Selecciono el valor de la primer fila de la primer columna

#`list` (lista)

familia <- list("Maria", "Juan", 10, c("Petra", "Hugo" ), c(8,6))
familia

# Primer elemento de la lista
familia[1]

# Primer elemento de la lista, simplificando la salida
familia[[1]]

# Primer elemento del primer elemento de la lista.
familia[[1]][1]
unlist(familia[1])[1] # Equivalente

# Primera columna del cuarto elemento
familia[[4]][1]

# indexada por nombres. Para el ejemplo anterior,

familia <- list('Madre' = "Maria", 
                'Padre' = "Juan",
                'Años_casados' = 10, 
                'Hijos' = c("Hugo", "Petra"),
                'Edad_hijos' = c(8,6))

familia [['Madre']][1]


# Borrar variables y objetos
mi.iris<-iris
ls() #lista los objetos
rm(mi.iris) #remueve el objeto iris
ls()
# tipo de variables trabajamos
mi.iris<-iris
class(mi.iris)
str(mi.iris)
is.data.frame(mi.iris)
as.data.frame(mi.iris)

x <- 1:10
is.vector(x)
class(x)

# Simulaciones de funciones de distribución

# Ejemplo: función de distribución normal**


set.seed(100)
data<-rnorm(10000000, mean = 0, sd = 1)
mean(data)
sd(data)
plot(density(data), main="Función de densidad Normal (0,1)")


qnorm(0.95, mean = 0, sd = 1)
qnorm(0.05, mean = 0, sd = 1)
qnorm(0.99, mean = 0, sd = 1)
qnorm(0.01, mean = 0, sd = 1)

pnorm(0,0,1)
dnorm(0.05)

# Extraer valores de un base de datos


library(readxl)
# 1.Leer los datos----
lluvia<- read_excel("lluvia_pampa.xlsx")
names(lluvia)
summary(lluvia)

# el registro que corresponde a la primer localidad

lluvia[1,1]

# los valores de la primer fila de la base de datos

lluvia[1,]

# ver la columna de las localidades

lluvia[,1]

# solamente los elementos de los meses de verano y otoño

(verano<-lluvia[,c(3:5, 14)])

(otonio<-lluvia[,6:9])

#¿Cuántas estaciones tiene la base de datos?

unique(lluvia$localidad)
length(unique(lluvia$localidad))

# solo los registros de la localidad MIRAMAR puede

miramar <- lluvia[lluvia$localidad=="MIRAMAR", ]

# usando `dplyr`:

library(dplyr)
miramar <- filter(lluvia, localidad == "MIRAMAR")


lluvia %>% select(1) %>% slice(1)
#_____________________________________________

# La familia `apply`
# apply(matrix, 1/2, f)`
#`tapply(vector, grouping, f)`

#valor de lluvia máxima en enero para todas las localidades?

a<-apply(lluvia[3], 2, max) #  2 indica columnas
a

# el valor de  lluvia máxima en enero para Miramar?

apply(lluvia[lluvia$localidad=="MIRAMAR",3 ],2,max)

# el valor de lluvia máxima enero para "TRES ARROYOS-AERO"?

apply(lluvia[lluvia$localidad=="TRES ARROYOS-AERO",3 ],2,max)

# Ejemplos de `tapply`

# la suma de las precipitaciones en el mes de enero 
# en cada localidad?

(tabla.1<-tapply(lluvia$ene, lluvia$localidad, sum))

#la suma de las precipitaciones de enero en 
# cada localidad antes de 1970?

(tabla.2<-tapply(lluvia$ene[lluvia$año<1970], lluvia$localidad[lluvia$año<1970], sum))

#¿Cuántos años tiene cada localidad ?

(años.localidad <- tapply(lluvia$año,lluvia$localidad,  
                          function(x) length(unique(x))))

#¿Cuántas localidades tuvieron más del 40 años de registros

años.localidad[años.localidad >40]


# __________________________________________________
# con library(base)
apply(lluvia[3], 2, max)

# con library(tidyverse)
lluvia %>% select(3) %>% max()
# __________________________________________________
# con library(base)
apply(lluvia[lluvia$localidad=="MIRAMAR",3 ],2,max)

# con library(tidyverse)
lluvia %>% 
  filter(localidad == 'MIRAMAR') %>%
  select(3) %>% 
  max()
# __________________________________________________
# con library(base)
apply(lluvia[lluvia$localidad=="TRES ARROYOS-AERO",3 ],2,max)

# con library(tidyverse)
lluvia %>% 
  filter(localidad == 'TRES ARROYOS-AERO') %>%
  select(3) %>% 
  max()
# __________________________________________________
# con library(base)
tapply(lluvia$ene, lluvia$localidad, sum)

# con library(tidyverse)
lluvia %>% group_by(localidad) %>% 
  summarise(total_enero = sum(ene))
# __________________________________________________
# con library(base)
tapply(lluvia$ene[lluvia$año<1970], lluvia$localidad[lluvia$año<1970], sum)

# con library(tidyverse)
lluvia %>% 
  filter(año<1970) %>%
  group_by(localidad) %>%
  summarise(total_enero = sum(ene))
# __________________________________________________
# con library(base)
# ¿Cuántos años tiene cada localidad ?
tapply(lluvia$año,lluvia$localidad,  function(x) length(unique(x)))

# con library(tidyverse)
lluvia %>% group_by(localidad) %>%
  summarise(cantidad = n_distinct(año))
# __________________________________________________
# con library(base)
# +40
#años.localidad[años.localidad >40]

# con library(tidyverse)
lluvia %>% group_by(localidad) %>%
  summarise(cantidad = n_distinct(año)) %>% 
  filter(cantidad > 40)


###################################################

# Procesos iterativos
## Bucle `for`
# 
#     for (var in vector){
#       # expresión que se repite
#     }

# ejemplo factorial
  
mi.factorial <- function(n){
  factorial <- 1
  for (i in 1:n){
    factorial <- factorial * i
  }
  return(factorial)
}

mi.factorial(7)

# Funciones propias y bucles


## Ejemplos del ciclo for en R

# Queremos conocer la distribución de la media muestral de n observaciones
# obtenidas independientemente de una distribución uniforme a lo largo del
# intervalo (0, 1).
# 
# -   Generar n observaciones con distribución uniforme en (0, 1).
# -   Calcular la media muestral de los datos.
# -   Repetir los pasos anteriores un elevado número de repeticiones.
# -   Aproximar la distribución de la media muestral con el histograma
# obtenido con las medias muestrales obtenidas en las repeticiones.

set.seed(1)  # Fijamos semilla para reproductibilidad
rep <- 5000 # Número de repeticiones
n <- 2       # Número de puntos
Media<- numeric(rep)

for (i in 1:rep) {
  x <- runif(n)
  Media[i] <- mean(x)
  
}
hist(Media, breaks = 40, main = paste("n = ", n), ylab="Frecuencia")

###################################################
vector<-# Definición de funciones propias

mi_suma <- function(a, b) {
  the_sum <- a + b
  return(the_sum)
}

mi_suma(3, 4)
mi_suma(20,10)
############################
cuadrado <- function(x){
  return(x^2)
}
cuadrado(9)
############################
cuadrado <- function(x) return(x^2)
cuadrado(9)

potencia <- function(x, exponente = 2) x^exponente
datos<-c(potencia(2), potencia(2, 3), potencia(2, exponente = 3))
datos

#######################################################****
# Veamos un ejemplo más de cómo programar una función propia 
# y aplicarla en un bucle, como vimos estamos interesados en 
# obtener la tendencia en la precipitación en las distintas 
# localidades en el tiempo de la base de datos lluvia.xlsX

library(readxl)
lluvia<- read_excel("lluvia_pampa.xlsx")
names(lluvia)

# ¿Cuáles son las valores que cambian?: localidad y mes

#################################################
tendencia.lluvia=function(j, i){
  #j=3 # Localidad en el vector de localidades
  #i=3 # posición columna (meses)
  print(i)
  print(j)
  lugar<-unique(lluvia$localidad)
  datos<-cbind(lluvia[1], lluvia[2], lluvia[,i])
  precipitacion<-datos[datos$localidad==lugar[j],3 ]# 
  tiempo<-datos[datos$localidad==lugar[j],2]
  #ajusto el modelo
  modelo<-lm(precipitacion~tiempo)
  #guardo los resultados
  resultados<-summary(modelo)
  resultados$coefficients
  #extraigo el valor p
  valor.p<-round(resultados$coefficients[2,4],2)
  # armo un vector con lo que quiero saber
  tendencia<-data.frame(lugar[j],colnames(datos)[3],valor.p )
  colnames(tendencia)<-c("localidad", "mes","valor.p")
  return(tendencia)
}
#######################################################
#función xa obtener el valor de tendencia 
# para una localidad y mes dado.

library(readxl)
lluvia<- read_excel("lluvia_pampa.xlsx")
dat<-tendencia.lluvia(3,4) # el primer valor corresponde a la posición de la localidad en el vector lugar y el segundo a la columna del mes en la base de datos
dat

# Uso del for
lugar<-unique(lluvia$localidad)
meses<-c(3:14)
salida.tendencia<-data.frame() # defino un data.frame vacio, donde se iran guardando los vectores de cada ajuste

####################################

for (j in 1:length(lugar)){
  for(i in meses){
    calculos<-tendencia.lluvia(j,i)
    salida.tendencia<-(rbind(salida.tendencia, calculos))
  }
}

#################################################
head(salida.tendencia )
summary(salida.tendencia)
significativas<-subset(salida.tendencia, valor.p<0.05)
significativas

