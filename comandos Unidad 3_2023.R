# Unidad 3
# Ejemplo: función de distribución normal

set.seed(100)
data<-rnorm(10000000, mean = 0, sd = 1)
mean(data)
sd(data)
dnorm(1, mean = 0, sd = 1)
plot(density(data), main="Función de densidad Normal (0,1)")

# Con los siguientes gráficos se puede observar cómo al aumentar el número
# de observaciones (n), los histogramas de los datos se acercan a la
# función de densidad normal real:

par(mfrow = c(1, 3))# Dividimos la ventana gráfica en una fila y tres columnas
x <- seq(-10, 10, length = 200)
set.seed(3)# Semilla

# n = 10
hist(rnorm(10, mean = 0, sd = 1), main = "n = 10",
     xlab = "", prob = TRUE)
lines(x, dnorm(x), col = "red", lwd = 2)

# n = 100
hist(rnorm(100, mean = 0, sd = 1), main = "n = 100",
     xlab = "", prob = TRUE)
lines(x, dnorm(x), col = "red", lwd = 2)

# n = 1000
hist(rnorm(1000, mean = 0, sd = 1), main = "n = 1000",
     xlab = "", prob = TRUE)
lines(x, dnorm(x), col = "red", lwd = 2)

# Volvemos a la ventana original
par(mfrow = c(1, 1))

#Ejemplo: Distribución normal cuantiles**
qnorm(0.95, mean = 0, sd = 1)
qnorm(0.05, mean = 0, sd = 1)
qnorm(0.99, mean = 0, sd = 1)
qnorm(0.01, mean = 0, sd = 1)

# La familia `apply`
library(readxl)
# 1.Leer los datos
lluvia<- read_excel("lluvia_pampa.xlsx")
names(lluvia)
summary(lluvia)
 
# -   `apply` se usa cuando desea aplicar una función sobre una matriz o
#     `data.frame`. Suma valores por fila o columna
# 
# -   `tapply` es una función que permite crear resúmenes de grupos
#     basados en niveles de los factores.

# Por ejemplo, si sobre la base de datos **lluvia** quisiéramos saber
# ¿cuál fue el valor de lluvia máxima que se presentó en enero para todas
# las localidades? Recuerde que enero está en la columna 3, entonces las
# sentencias serian:

a<-apply(lluvia[3], 2, max) #  2 indica columnas
a

# Para saber por ejemplo ¿Cuál fue el valor de lluvia máxima que se
# presentó en enero para Miramar?

apply(lluvia[lluvia$localidad=="MIRAMAR",3 ],2,max)

# ¿Cuál fue el valor de lluvia máxima que se presentó en enero para "TRES
# ARROYOS-AERO"?
apply(lluvia[lluvia$localidad=="TRES ARROYOS-AERO",3 ],2,max)


# **Ejemplos de `tapply`**
# ¿Cuál es la suma de las precipitaciones en el mes de enero en cada
# localidad?
(tabla.1<-tapply(lluvia$ene, lluvia$localidad, sum))

# ¿Cuál es la suma de las precipitaciones en el mes de enero en cada
# localidad antes de 1970?
(tabla.2<-tapply(lluvia$ene[lluvia$año<1970], lluvia$localidad[lluvia$año<1970], sum))

# ¿Cuántos años tiene cada localidad ?
(años.localidad <- tapply(lluvia$año,lluvia$localidad,  function(x) length(unique(x))))

# ¿Cuántas localidades tuvieron más del 40 años de registros
años.localidad[años.localidad >40]

# con library(base)
apply(lluvia[3], 2, max)
# con library(tidyverse)
lluvia %>% select(3) %>% max()

# con library(base)
apply(lluvia[lluvia$localidad=="MIRAMAR",3 ],2,max)
# con library(tidyverse)
lluvia %>% 
  filter(localidad == 'MIRAMAR') %>%
  select(3) %>% 
  max()

# con library(base)
apply(lluvia[lluvia$localidad=="TRES ARROYOS-AERO",3 ],2,max)
# con library(tidyverse)
lluvia %>% 
  filter(localidad == 'TRES ARROYOS-AERO') %>%
  select(3) %>% 
  max()

# con library(base)
tapply(lluvia$ene, lluvia$localidad, sum)
# con library(tidyverse)
lluvia %>% group_by(localidad) %>% 
  summarise(total_enero = sum(ene))
  
# con library(base)
tapply(lluvia$ene[lluvia$año<1970], lluvia$localidad[lluvia$año<1970], sum)
# con library(tidyverse)
lluvia %>% 
  filter(año<1970) %>%
  group_by(localidad) %>%
  summarise(total_enero = sum(ene))

# con library(base)
# ¿Cuántos años tiene cada localidad ?
tapply(lluvia$año,lluvia$localidad,  function(x) length(unique(x)))
# con library(tidyverse)
lluvia %>% group_by(localidad) %>%
  summarise(cantidad = n_distinct(año))

# con library(base)
# +40
#años.localidad[años.localidad >40]

# con library(tidyverse)
lluvia %>% group_by(localidad) %>%
  summarise(cantidad = n_distinct(año)) %>% 
  filter(cantidad > 40)
############################################################
# Definición de funciones propias

mi_suma <- function(a, b) {
  the_sum <- a + b
  return(the_sum)
}

mi_suma(3, 4)

cuadrado <- function(x){
  return(x^2)
}

cuadrado(9)

cuadrado <- function(x) return(x^2)
cuadrado(9)

cuadrado <- function(x) x^2
cuadrado(9)

# Las funciones en `R` pueden tener argumentos con valores por defecto.

potencia <- function(x, exponente = 2) x^exponente
datos<-c(potencia(2), potencia(2, 3), potencia(2, exponente = 3))
datos

# Procesos iterativos
## Bucle `for`

## Ejemplos del ciclo for en R

### Ejemplo 1
for (i in 1:5) {
   x1 <- i^2          
  print(x1)
}

### Ejemplo 2
# Queremos conocer la distribución de la media muestral de n observaciones
# obtenidas independientemente de una distribución uniforme a lo largo del
# intervalo (0, 1).

set.seed(1)  # Fijamos semilla para reproductibilidad
rep <- 5000 # Número de repeticiones
n <- 2       # Número de puntos
Media<- numeric(rep)

for (i in 1:rep) {
    x <- runif(n)
    Media[i] <- mean(x)
    
}
hist(Media, breaks = 40, main = paste("n = ", n), ylab="Frecuencia")


### Ejemplo 3
library(tidyverse)
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
df

# Queremos calcular la mediana de cada columna. Podrías hacerlo copiando y
# pegando el siguiente código:

median(df$a)
median(df$b)
median(df$c)
median(df$d)

output <- vector("double", ncol(df))  # 1. output
for (i in seq_along(df)) {            # 2. secuencia
  output[[i]] <- median(df[[i]])      # 3. cuerpo
}
output


### Ejemplo 4
# Veamos como usarlo en la base de datos lluvia, queremos obtener la suma de la precipitación de cada mes en cada localidad

library(readxl)
lluvia.inta<- read_excel("lluvia_pampa.xlsx")
head(lluvia.inta)


# Vamos a acomodar los datos usando la función `pivot_longer`
# ver: vignette("pivot")

# Pasa a 4 variables y elimina las ultima 3 columnas
lluvia <- lluvia.inta[-c(15:17)] %>% pivot_longer(!c(localidad, año),
                                             names_to = 'mes',
                                             values_to = 'ppm')



lluvia%>%
 filter(mes=="ene" & localidad=="MAR DEL PLATA-AERO" ) %>%
 summarise(total = sum(ppm))

# Lo podemos convertir en una función

precip<-function(data,lugar,fecha){
 data%>%
 filter(localidad==lugar & mes==fecha ) %>%
 summarise(total = sum(ppm))
}

precip(lluvia,"MAR DEL PLATA-AERO","ene")


# Esta función la deberíamos aplicar a cada mes de cada localidad
head(lugar<-unique(lluvia$localidad))
head(meses<-unique(lluvia$mes))


# Los podemos hacer usando un loop
resultado<-data.frame()
for (i in lugar) {
  for (j in meses) {
sum.prep<- data.frame(precip(lluvia,i,j ), i, j) 
resultado<-rbind(resultado,sum.prep)
  }
}
  
