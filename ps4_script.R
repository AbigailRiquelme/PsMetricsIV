
# ECONOMETRIA AVANZADA
# Profesor: Walter Sosa Escudero
# Asistente: Gaston Garcia Zavaleta

# Problem Set 4
# Integrantes: Capriata, Pacheco y Riquelme
# Fecha de entrega: 20 de mayo de 2022


# Antes de comenzar vamos a definir el directorio en el que guardaremos las bases 
# que vamos a exportar e importar.


dir <- 
dir <- "/Users/Abi/Documents/GitHub/PsMetricsIV"
dir <-

setwd(dir)

data <- read_dta("cuarto_trim_2019.dta")


#### Punto 2 ####

myprobit <- glm(deserta ~ +  + , family = binomial(link = "probit"), 
                data = data)



#### Punto 3 #### 

# Media 

marginal_media <- probitmfx(deserta ~  + + , data = data ,
                            atmean = TRUE, robust = TRUE) 
