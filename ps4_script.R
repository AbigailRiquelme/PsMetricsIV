
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




# Punto 1


library(fastDummies)

data_d <- dummy_cols(data, select_columns = "ch11")




#### Punto 2 ####

# Vamos a escribir la fórmula del modelo a estimar. Primero vamos a guardar la formula dado que la utilizaremos
# luego. 

library(Formula)
modelo_desercion <- Formula(deserta ~ mujer + educ_jefe + hermanos + ingreso_per_capita + jmujer +
                     ch11_0 + ch11_1 + ch11_9 )

# Estimamos el probit

desercion_probit <- glm(modelo_desercion , family = binomial(link = "probit"), 
                data = data_d)

# Vamos a corregir la inferencia en caso de que haya heterocedasticidad.
# Utilizaremos la matriz de varianzas y covarianzas definida por White (1980)

# Abrimos las librerias necesarias

library(lmtest)
library(sandwich)

# Calculamos:

desercion_probit_robust <-  coeftest(desercion_probit, vcov = vcovHC(desercion_probit, "HC1"))  
summary(desercion_probit_robust)


#### Punto 3 #### 


# Media 

marginal_media <- probitmfx(deserta ~  + + , data = data ,
                            atmean = TRUE, robust = TRUE) 


### Punto 4 ###

# Ahora tenemos que estimar el mismo modelo pero como un modelo lineal de probabilidad. 
# Sabemos que este modelo es intrínsecamente heterocedástico, por lo que es necesario corregir la inferencia.
# Esto lo haremos utilizando los errores estandar robustos de White.


# Estimamos el modelo lineal

desercion_mlp <- lm(modelo_desercion, data = data_d)

# Mostramos la estimación utilizando la matriz de varianzas y covarianzas de White

desercion_mlp_robust <- coeftest(desercion_mlp, vcov = vcovHC(desercion_mlp, "HC1"))  

#stargazer(desercion_mlp_robust, desercion_mlp, type = "text")


#### Punto 5 #### 

# Reemplazamos todos los valores que son ceros por 1.

data_d$ingreso_per_capita[data_d$ingreso_per_capita == 0] <- 1

# Generamos la nueva variable

data_d$ln_ing <- log(data_d$ingreso_per_capita, base = exp(1))

# Exportamos las estimaciones:

stargazer(desercion_probit_robust, desercion_mlp_robust, type='text',
          dep.var.labels=c("Deserta", "Deserta"),
          notes = "Robust standard errors in parentheses")

#covariate.labels = c("Educacion",  "Edad", "Edad2", "Race", "Married", 
#                               "SMSA", "Primer trimestre", "Segundo trimestre", 
#                               "Tercer trimestre"), align = TRUE)

          
          







