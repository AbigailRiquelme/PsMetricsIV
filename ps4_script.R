
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



library(haven)
data <- read_dta("https://econometriaudesa.weebly.com/uploads/1/3/6/3/136372338/cuarto_trim_2019.dta")
data <- read_dta("cuarto_trim_2019.dta")


#### Punto 2 ####

library(fastDummies)

data_d <- dummy_cols(data, select_columns = "ch11")


myprobit <- glm(deserta ~ mujer + educ_jefe + hermanos + ingreso_per_capita + jmujer +
                  ch11_0 + ch11_1 + ch11_9 , family = binomial(link = "probit"), 
                data = data_d)

summary(myprobit)

#### Punto 3 #### 

# Media 

marginal_media <- probitmfx(deserta ~  + + , data = data ,
                            atmean = TRUE, robust = TRUE) 
