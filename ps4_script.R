
# ECONOMETRIA AVANZADA
# Profesor: Walter Sosa Escudero
# Asistente: Gaston Garcia Zavaleta

# Problem Set 4
# Integrantes: Capriata, Pacheco y Riquelme
# Fecha de entrega: 20 de mayo de 2022


# Antes de comenzar vamos a definir el directorio en el que guardaremos las bases 
# que vamos a exportar e importar.

dir <- "/Users/Abi/Documents/GitHub/PsMetricsIV"
dir <- "C:/Users/estef/Desktop/San Andr閟/2022/Econometr韆 Avanzada/PsMetricsIV/PsMetricsIV"

setwd(dir)

library(haven)
data <- read_dta("https://econometriaudesa.weebly.com/uploads/1/3/6/3/136372338/cuarto_trim_2019.dta")
data <- read_dta("cuarto_trim_2019.dta")

#### Punto 1 ####

library(fastDummies)

# Creamos dummies para la varible de tipo de escuela

data_d1 <- dummy_cols(data, select_columns = "ch11")

# Lo mismo para las categorias de educacion del jefe/a de hogar

data_d1 <- dummy_cols(data_d1, select_columns = "educ_jefe")


# Eliminamos las observaciones para las cuales el ingreso per c?pita es cero 

data_d <- data_d1[data_d1$ingreso_per_capita > 1, ]

# De esta forma eliminamos 735 observaciones. 

library(ggplot2)
library(dplyr)

# Densidad para el ingreso per capita 

g1 <- ggplot(data_d, aes(x=`ingreso_per_capita`, group=`deserta`, color=factor(`deserta`)))+ 
  geom_density(adjust=2, size = 0.75) + 
  geom_vline(aes(xintercept = mean(ingreso_per_capita)), 
             linetype = "dashed", size = 0.6) +
  xlab("Ingreso per c谩pita") +
  ylab("Densidad")+
  theme_minimal() + 
  ggtitle("Densidad del ingreso per c谩pita")+
  scale_colour_manual(
    values = c("burlywood2", "azure3"), 
    labels = c("No", "Si"),
    name = "Deserta")+
  theme(
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(0.5,"cm"),
    plot.title = element_text(hjust = 0.5)
  )


# Histograma de frecuencia de la cantidad de hermanos

g2 <- ggplot(data_d, aes(x=`hermanos`, group=`deserta`, color=factor(`deserta`)))+ 
  geom_histogram(size = 0.75, fill="white") + 
  geom_vline(aes(xintercept = mean(hermanos)), 
             linetype = "dashed", size = 0.6) +
  xlab("Cantidad de hermanos") +
  ylab("Frecuencia")+
  theme_minimal() + 
  ggtitle("Histograma de la cantidad de hermanos")+
  scale_colour_manual(
    values = c("burlywood2", "azure3"), 
    labels = c("No", "Si"),
    name = "Deserta") + 
  theme(plot.title = element_text(hjust = 0.5))


# Realizamos un grafico de barras sobre el nivel educativo del jefe de 
# hogar diferenciando segun la variable deserta 

data_d$educ_lab[data_d$educ_jefe == 0] = "Missing"
data_d$educ_lab[data_d$educ_jefe == 2] = "Primaria"
data_d$educ_lab[data_d$educ_jefe == 3] = "EGB"
data_d$educ_lab[data_d$educ_jefe == 4] = "Secundaria"
data_d$educ_lab[data_d$educ_jefe == 5] = "Polimodal"
data_d$educ_lab[data_d$educ_jefe == 6] = "Terciaria"
data_d$educ_lab[data_d$educ_jefe == 7] = "Universitaria"
data_d$educ_lab[data_d$educ_jefe == 8] = "Posgrado"

data_d$deserta_lab <- ""
data_d$deserta_lab[data_d$deserta == 1] = "Si"
data_d$deserta_lab[data_d$deserta == 0] = "No"


g3 <- ggplot(data_d, mapping = aes(x = educ_lab,
                     y = after_stat(count/sum(count)),
                fill = deserta_lab)) +
  geom_bar() + 
  scale_fill_manual( values = c("burlywood2", "azure3"), 
    labels = c("No", "Si"),
    name = "Deserta") + 
  theme_minimal() + 
  coord_flip() +
  xlab("Educaci贸n") +
  ylab("% Sobre el total") + 
  ggtitle("Nivel educativo del jefe de hogar") + 
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))

# Exportamos los tres graficos en uno.

library(gridExtra)
grid.arrange(g1, g2, g3, nrow = 3, ncol = 1)


# Hacemos una tabla con informaci贸n sobre las varibles

library(stargazer)

data_stuct <- data.frame(data_d)
stargazer(data_stuct[c("deserta", "mujer",
                       "jmujer", "ingreso_per_capita")],
          type="latex")


#### Punto 2 ####

# Vamos a escribir la formula del modelo a estimar. Primero vamos a guardar la formula dado 
# que la utilizaremos luego. 

# Es importante destacar que usamos como categoria base educ_jefe_2 (primaria) para el nivel educativo 
# de los jefes de hogar y colegio privado como base de ch11.

library(Formula)
modelo_desercion <- Formula(deserta ~ mujer + educ_jefe_0 + educ_jefe_3 + educ_jefe_4 + educ_jefe_5 + 
                              educ_jefe_6 + educ_jefe_7 + educ_jefe_8 + hermanos + ingreso_per_capita + jmujer +
                     ch11_0 + ch11_1 + ch11_9)

# Estimamos el probit

desercion_probit <- glm(modelo_desercion , family = binomial(link = "probit"), 
                data = data_d)

# Vamos a corregir la inferencia en caso de que haya heterocedasticidad.
# Utilizaremos la matriz de varianzas y covarianzas definida por White (1980)

# Abrimos las librerias necesarias

library(lmtest)
library(sandwich)
library(mfx)
library(margins)

# Calculamos:

desercion_probit_robust <-  coeftest(desercion_probit, vcov = vcovHC(desercion_probit, "HC1"))  
desercion_probit_robust

#### Punto 3 #### 

#install.packages("modeest")
library(modeest)

# Computaremos distintos efectos marginales

# En la media 


marginal_media <- margins(desercion_probit, at = list(mujer = 0, 
                                                      hermanos = mean(data_d$hermanos), 
                                                      ingreso_per_capita = mean(data_d$ingreso_per_capita), 
                                                      educ_jefe_0 = 0, 
                                                      educ_jefe_3 = 0, 
                                                      educ_jefe_4 = 0, 
                                                      educ_jefe_5 = 0, 
                                                      educ_jefe_6 = 0, 
                                                      educ_jefe_7 = 0, 
                                                      educ_jefe_8 = 0, 
                                                      jmujer = 0, 
                                                      ch11_0 = 0,
                                                      ch11_1 = 0,
                                                      ch11_9 = 0))




c1 <- as.matrix(marginal_media[1,38:51])


# En la mediana 

marginal_median <- margins(desercion_probit, at = list(mujer = 0, 
                                               hermanos = median(data_d$hermanos),
                                               ingreso_per_capita = median(data_d$ingreso_per_capita),
                                               educ_jefe_0 = 0, 
                                               educ_jefe_3 = 0, 
                                               educ_jefe_4 = 0, 
                                               educ_jefe_5 = 0, 
                                               educ_jefe_6 = 0, 
                                               educ_jefe_7 = 0, 
                                               educ_jefe_8 = 0, 
                                               jmujer = 0, 
                                               ch11_0 = 0,
                                               ch11_1 = 0,
                                               ch11_9 = 0))

c2 <- as.matrix(marginal_median[1,38:51])


# En la moda

marginal_mfv <- margins(desercion_probit, at = list(mujer = 0,
                                       hermanos = mfv(data_d$hermanos), 
                                       ingreso_per_capita = mfv(data_d$ingreso_per_capita), 
                                       educ_jefe_0 = 0, 
                                       educ_jefe_3 = 0, 
                                       educ_jefe_4 = 0,
                                       educ_jefe_5 = 0, 
                                       educ_jefe_6 = 0,
                                       educ_jefe_7 = 0,
                                       educ_jefe_8 = 0,
                                       jmujer = 0, 
                                       ch11_0 = 0,
                                       ch11_1 = 0,
                                       ch11_9 = 0))


c3 <- as.matrix(marginal_mfv[1,38:51])


# En valores especificos (minimo ingreso, minima educacion del jefe de hogar, minima cantidad de hermanos)

marginal_1 <- margins(desercion_probit, at = list(mujer = 0,
                                       hermanos = min(data_d$hermanos),
                                       ingreso_per_capita = min(data_d$ingreso_per_capita), 
                                       educ_jefe_0 = 0,
                                       educ_jefe_3 = 0, 
                                       educ_jefe_4 = 0, 
                                       educ_jefe_5 = 0, 
                                       educ_jefe_6 = 0,
                                       educ_jefe_7 = 0,
                                       educ_jefe_8 = 0,
                                       jmujer = 0,
                                       ch11_0 = 0, 
                                       ch11_1 = 0, 
                                       ch11_9 = 0))

c4 <- as.matrix(marginal_1[1,38:51])


tabla <- matrix(data = NA, nrow = 5, ncol = 15)


tabla[2,2:15] <- c1
tabla[3,2:15] <- c2
tabla[4,2:15] <- c3
tabla[5,2:15] <- c4

tabla <- round(tabla,3)

tabla[1,1:15] <- c("Efecto marginal","Mujer", "Missing", "EGB", "Secundario", "Polimodal", "Terciario",
             "Universitario", "Posgrado", "Hermanos", "Ingreso PC", "Jefa de hogar", 
             "Missing", "Publico", "No responde")


tabla[2:5,1] <- c("Media", "Mediana", "Moda", "Caso particular")

tabla2 <- matrix(data = NA, nrow = 10, ncol = 8)

tabla2[1,1] <- c("Efecto marginal")

tabla2[2:5,1] <- c("Media", "Mediana", "Moda", "Caso particular")

tabla2[6,1] <- c("Efecto marginal")

tabla2[7:10,1] <- c("Media", "Mediana", "Moda", "Caso particular")

tabla2[1,2:8] <- c("Mujer", "Missing", "EGB", "Secundario", "Polimodal", "Terciario",
                   "Universitario")

tabla2[2:5, 2:8] <- tabla[2:5, 2:8]

tabla2[6, 2:8] <- c("Posgrado", "Hermanos", "Ingreso PC", "Jefa de hogar", "Missing", "Publico", "No responde")

tabla2[7:10, 2:8] <- tabla[2:5, 9:15]

# Exportamos la tabla

stargazer(tabla2,  type='latex')



#### Punto 4 ####

# Ahora tenemos que estimar el mismo modelo pero como un modelo lineal de probabilidad. 
# Sabemos que este modelo es intr?nsecamente heteroced?stico, por lo que es necesario 
# corregir la inferencia.
# Esto lo haremos utilizando los errores estandar robustos de White.

# Estimamos el modelo lineal

desercion_mlp <- lm(modelo_desercion, data = data_d)

# Corregimos los SE

desercion_mlp_robust <- coeftest(desercion_mlp, vcov = vcovHC(desercion_mlp, "HC1"))  


#### Punto 5 #### 

# Reemplazamos las osbervaciones que tienen ingreso igual a cero con un ingreso 
# igual a 1 (esto lo hacemos sobre la base de datos original, sin eliminar las 
# observaciones que tienen ingreso per capita igual a cero)

data_d1$ingreso_per_capita[data_d1$ingreso_per_capita==0] <- 1

# Generamos la nueva variabl

data_d1$ln_ing <- log(data_d1$ingreso_per_capita)


#### Punto 6 #### 

# Vamos a mantener las caracteristicas de los individuos constante y solo variaremos el ingreso.
# El objetivo de esto es ver como se modifican las probabilidades de desertar ante cambios en el ingreso

# Generamos una secuencia de ingresos. Esta va desde 0 a $100.000 y va aumentando de a 100 pesos.

ingreso <- seq(0,100000,500)
k <- length(ingreso)

# Hacemos las predicciones

pred_mujer <- predict(desercion_probit, with(data_d, data.frame(mujer=rep(1,k),
                                                          hermanos = rep(median(data_d$hermanos), k),
                                                          ingreso_per_capita=ingreso,
                                                          educ_jefe_0 = rep(0,k),
                                                          educ_jefe_3 = rep(1,k),
                                                          educ_jefe_4 = rep(0,k),
                                                          educ_jefe_5 = rep(0,k),
                                                          educ_jefe_6 = rep(0,k),
                                                          educ_jefe_7 = rep(0,k),
                                                          educ_jefe_8 = rep(0,k),
                                                          jmujer= rep(1,k),
                                                          ch11_0= rep(1,k),
                                                          ch11_1=rep(0,k),
                                                          ch11_9=rep(0,k))), type = "response")

pred_hombre <- predict(desercion_probit, with(data_d, data.frame(mujer=rep(0,k),
                                                                hermanos = rep(median(data_d$hermanos), k),
                                                                ingreso_per_capita=ingreso,
                                                                educ_jefe_0 = rep(0,k),
                                                                educ_jefe_3 = rep(1,k),
                                                                educ_jefe_4 = rep(0,k),
                                                                educ_jefe_5 = rep(0,k),
                                                                educ_jefe_6 = rep(0,k),
                                                                educ_jefe_7 = rep(0,k),
                                                                educ_jefe_8 = rep(0,k),
                                                                jmujer= rep(1,k),
                                                                ch11_0= rep(1,k),
                                                                ch11_1=rep(0,k),
                                                                ch11_9=rep(0,k))), type = "response")

pred_jm <- predict(desercion_probit, with(data_d, data.frame(mujer=rep(1,k),
                                                                 hermanos = rep(median(data_d$hermanos), k),
                                                                 ingreso_per_capita=ingreso,
                                                                 educ_jefe_0 = rep(0,k),
                                                                 educ_jefe_3 = rep(1,k),
                                                                 educ_jefe_4 = rep(0,k),
                                                                 educ_jefe_5 = rep(0,k),
                                                                 educ_jefe_6 = rep(0,k),
                                                                 educ_jefe_7 = rep(0,k),
                                                                 educ_jefe_8 = rep(0,k),
                                                                 jmujer= rep(1,k),
                                                                 ch11_0= rep(1,k),
                                                                 ch11_1=rep(0,k),
                                                                 ch11_9=rep(0,k))), type = "response")

pred_jh <- predict(desercion_probit, with(data_d, data.frame(mujer=rep(1,k),
                                                             hermanos = rep(median(data_d$hermanos), k),
                                                             ingreso_per_capita=ingreso,
                                                             educ_jefe_0 = rep(0,k),
                                                             educ_jefe_3 = rep(1,k),
                                                             educ_jefe_4 = rep(0,k),
                                                             educ_jefe_5 = rep(0,k),
                                                             educ_jefe_6 = rep(0,k),
                                                             educ_jefe_7 = rep(0,k),
                                                             educ_jefe_8 = rep(0,k),
                                                             jmujer= rep(0,k),
                                                             ch11_0= rep(1,k),
                                                             ch11_1=rep(0,k),
                                                             ch11_9=rep(0,k))), type = "response")

# Guardamos todo en la misma base de datos 

df_final <- data.frame(
  pred_jm,
  pred_jh, 
  pred_hombre, 
  pred_mujer)

# Graficamos:

# Seg煤n si el jefe es mujer u hombre

jhm <- ggplot()+
  geom_line(data=df_final,aes(y=pred_jm,x= ingreso,colour="Mujer"),size=1 )+
  geom_line(data=df_final,aes(y=pred_jh,x= ingreso,colour="Hombre"),size=1) +
  scale_color_manual(name = "Jefe de hogar", values = c("Mujer" = "burlywood2", "Hombre" = "azure3"))+
  ggtitle("Probabilidad de desercion escolar", subtitle = "Seg煤n el sexo del jefe de hogar") +
  xlab("Ingreso per capita")+
  ylab("Probabilidad predicha")+
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Seg煤n si es hombre o mujer

hm <- ggplot()+
  geom_line(data=df_final,aes(y=pred_mujer,x= ingreso,colour="Mujer"),size=1 )+
  geom_line(data=df_final,aes(y=pred_hombre,x= ingreso,colour="Hombre"),size=1) +
  scale_color_manual(name = "Sexo", values = c("Mujer" = "burlywood2", "Hombre" = "azure3"))+
  ggtitle("Probabilidad de deserci贸n escolar",
          subtitle = "Seg煤n sexo del individuo") +
  xlab("Ingreso per capita") + 
  ylab("")+
  theme_bw() + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Exportamos

grid.arrange(jhm, hm, nrow = 1, ncol = 2)



#### Punto 7 #### 

# Estimamos el modelo que pide la consigna

modelo_7 <- Formula(deserta ~ jmujer + mujer + educ_jefe_0 + educ_jefe_3 + educ_jefe_4 + educ_jefe_5 + 
                      educ_jefe_6 + educ_jefe_7 + educ_jefe_8 + hermanos + ln_ing)

desercion_probit7 <- glm(modelo_7 , family = binomial(link = "probit"), 
                         data = data_d1)              

desercion_probit7_robust <- coeftest(desercion_probit7, vcov = vcovHC(desercion_probit, "HC1"))  

# Tal como escribimos en el informe, tenemos que hacer un c谩lculo con beta[ln_ing] y beta[jmujer].
# Lo hacemos en el siguiente paso

ratio = desercion_probit7$coefficients[2]/desercion_probit7$coefficients[12]
exp(-ratio)-1


# Exportamos las estimaciones:

stargazer(desercion_probit_robust, desercion_mlp_robust, desercion_probit7, type='tex',
          dep.var.labels=c("Deserta", "Deserta", "Deserta"),
          covariate.labels = c("Mujer", "Educaci髇 JH (missing)", "Educaci贸n JH (EGB)", "Educaci贸n JH (Secundario)",
                               "Educaci贸n JH (Polimodal)", "Educaci贸n JH (Terciario)", "Educaci贸n JH (Universitario)",
                               "Educaci贸n JH (Posgrado)",
                               "Cantidad de hermanos", "Ingreso per c谩pita","Log ingreso per capita", "Jefe de hogar mujer",
                               "Establecimiento educativo (missing)", 
                               "Establecimiento educativo (p煤blico)", "Establecimiento educativo (no responde)"),
          notes = "Robust standard errors in parentheses", 
          add.lines=list(c("Modelo", "Probit", "MLP", "Probit")))





