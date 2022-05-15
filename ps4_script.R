
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
dir <- "C:/Users/estef/Desktop/San Andr?s/2022/Econometr?a Avanzada/PsMetricsIV/PsMetricsIV"

setwd(dir)



library(haven)
data <- read_dta("https://econometriaudesa.weebly.com/uploads/1/3/6/3/136372338/cuarto_trim_2019.dta")
data <- read_dta("cuarto_trim_2019.dta")



# Punto 1


library(fastDummies)

data_d <- dummy_cols(data, select_columns = "ch11")

data_d <- dummy_cols(data_d, select_columns = "educ_jefe")



# Eliminamos las observaciones para las cuales el ingreso per cápita es cero 

data_d <- data_d[data_d$ingreso_per_capita > 1, ]

# De esta forma eliminamos 735 observaciones 

library(ggplot2)
library(dplyr)

# Histograma y densidad para el ingreso per capita 


ingreso_den <- ggplot(data_d, aes(x=`ingreso_per_capita`, group=`deserta`, color=factor(`deserta`)))+ 
  geom_density(adjust=2, size = 0.75) + 
  geom_vline(aes(xintercept = mean(ingreso_per_capita)), 
             linetype = "dashed", size = 0.6) +
  xlab("Ingreso per cápita") +
  ylab("Densidad")+
  theme_minimal() + 
  ggtitle("Densidad del ingreso per cápita", subtitle = "EPH 2019")+
  scale_colour_manual(
    values = c("burlywood2", "azure3"), 
    labels = c("Si", "No"),
    name = "Deserta")+
  theme(
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(0.5,"cm") 
  )

ingreso_den

ggsave(file="ingreso_den.eps", width=6.5, height=4, dpi=300)




hermanos_den <- ggplot(data_d, aes(x=`hermanos`, group=`deserta`, color=factor(`deserta`)))+ 
  geom_histogram(size = 0.75, fill="white") + geom_vline(aes(xintercept = mean(hermanos)), 
                                                         linetype = "dashed", size = 0.6) +
  xlab("Cantidad de hermanos") +
  ylab("Observaciones")+
  theme_minimal() + 
  ggtitle("Histograma de la cantidad de hermanos", subtitle = "EPH 2019")+
  scale_colour_manual(
    values = c("burlywood2", "azure3"), 
    labels = c("Si", "No"),
    name = "Deserta")

hermanos_den

ggsave(file="hermanos_den.eps", width=6.5, height=4, dpi=300)


# Realizamos un gráfico de barras sobre el nivel educativo del jefe de hogar diferenciando según la variable deserta 

counts <- table(data_d$deserta, data_d$educ_jefe)

barplot(counts, main="Nivel educativo del jefe de hogar",
        xlab="Nivel educativo del jefe de hogar", ylab="Cantidad de observaciones",col=c("burlywood2", "azure3"),
        legend.text = c("Desertó", "No desertó"), beside=TRUE)

# Realizamos un gráfico de barras sobre el tipo de institución educativa a la que se asiste según la variable deserta 

counts_1 <- table(data_d$deserta, data_d$ch11)

barplot(counts_1, main="Tipo de establecimiento educativo",
        xlab="Tipo de establecimiento educativo", ylab="Cantidad de observaciones",col=c("burlywood2", "azure3"),
        legend.text = c("Desertó", "No desertó"), beside=TRUE)



# Realizamos una tabla para obtener la frecuencia de la cantidad de hombres y mujeres según si desertan o no

# el que está como columna es mujer y el que está como fila es deserta 

tabla_1 <- table(data_d$deserta, data_d$`mujer`)
tabla_2 <- prop.table(tabla_1)

# Exportamos la tabla 

stargazer(tabla_2, type='latex',
          dep.var.labels=c("Deserta", "No deserta"))


#### Punto 2 ####

# Vamos a escribir la fórmula del modelo a estimar. Primero vamos a guardar la formula dado que la utilizaremos
# luego. 

# Es importante destacar que usamos como categor?a base educ_jefe_2 para el nivel educativo de los jefes de hogar y 

# ch11_1. 

library(Formula)
modelo_desercion <- Formula(deserta ~ mujer + educ_jefe_0 + educ_jefe_3 + educ_jefe_4 + educ_jefe_5 + 
                              educ_jefe_6 + educ_jefe_7 + hermanos + ingreso_per_capita + jmujer +
                     ch11_0 + ch11_1 + ch11_9)

# esto va??

modelo_desercion <- Formula(deserta ~ mujer + hermanos + ingreso_per_capita + jmujer +
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

# Media 

marginal_media <- probitmfx(deserta ~ mujer + educ_jefe_0 + educ_jefe_3 + educ_jefe_4 + educ_jefe_5 + educ_jefe_6 + educ_jefe_7 + hermanos + ingreso_per_capita + jmujer +
                              ch11_0 + ch11_1 + ch11_9, data = data_d,
                            atmean = TRUE, robust = TRUE) 

# En la media para hombres y mujeres 

prueba1 <- margins(desercion_probit, at = list(mujer = 0:1))

# Mediana 


prueba2 <- margins(desercion_probit, at = list(mujer = median(data_d$mujer), educ_jefe = median(data_d$educ_jefe), 
                                       hermanos = median(data_d$hermanos), educ_jefe_0 = median(data_d$educ_jefe_0), educ_jefe_3 = median(data_d$educ_jefe_3), 
                                       educ_jefe_4 = median(data_d$educ_jefe_4), educ_jefe_5 = median(data_d$educ_jefe_5), educ_jefe_6 = median(data_d$educ_jefe_6), 
                                       educ_jefe_7 = median(data_d$educ_jefe_7), ingreso_per_capita = median(data_d$ingreso_per_capita), jmujer = median(data_d$jmujer), 
                                       ch11_0 = median(data_d$ch11_0), ch11_1 = median(data_d$ch11_1), ch11_9 = median(data_d$ch11_9)))

# Moda


prueba3 <- margins(desercion_probit, at = list(mujer = mfv(data_d$mujer), educ_jefe = mfv(data_d$educ_jefe), 
                                       hermanos = mfv(data_d$hermanos), ingreso_per_capita = mfv(data_d$ingreso_per_capita), 
                                       educ_jefe_0 = mfv(data_d$educ_jefe_0), educ_jefe_3 = mfv(data_d$educ_jefe_3), 
                                       educ_jefe_4 = mfv(data_d$educ_jefe_4), educ_jefe_5 = mfv(data_d$educ_jefe_5), 
                                       educ_jefe_6 = mfv(data_d$educ_jefe_6), educ_jefe_7 = mfv(data_d$educ_jefe_7),
                                       jmujer = mfv(data_d$jmujer), ch11_0 = mfv(data_d$ch11_0), ch11_1 = mfv(data_d$ch11_1), ch11_9 = mfv(data_d$ch11_9)))

# En valores espec?ficos (m?nimo ingreso, minima educaci?n del jefe de hogar, minima cantidad de hermanos)

prueba4 <- margins(desercion_probit, at = list(mujer = median(data_d$mujer), educ_jefe = min(data_d$educ_jefe), 
                                       hermanos = min(data_d$hermanos), ingreso_per_capita = min(data_d$ingreso_per_capita), 
                                       educ_jefe_0 = median(data_d$educ_jefe_0), educ_jefe_3 = median(data_d$educ_jefe_3), 
                                       educ_jefe_4 = median(data_d$educ_jefe_4), educ_jefe_5 = median(data_d$educ_jefe_5), 
                                       educ_jefe_6 = median(data_d$educ_jefe_6), educ_jefe_7 = median(data_d$educ_jefe_7),
                                       jmujer = median(data_d$jmujer), ch11_0 = median(data_d$ch11_0), ch11_1 = median(data_d$ch11_1), ch11_9 = median(data_d$ch11_9)))



prueba5 <- margins(myprobit, at = list(mujer = mfv(data_d$mujer), educ_jefe = max(data_d$educ_jefe), 
                                       hermanos = max(data_d$hermanos), ingreso_per_capita = max(data_d$ingreso_per_capita), 
                                       jmujer = mfv(data_d$jmujer), ch11_0 = mfv(data_d$ch11_0), ch11_1 = mfv(data_d$ch11_1), ch11_9 = mfv(data_d$ch11_9)))


# Queda armar la tabla VER 


### Punto 4 ###

# Ahora tenemos que estimar el mismo modelo pero como un modelo lineal de probabilidad. 
# Sabemos que este modelo es intr?nsecamente heteroced?stico, por lo que es necesario corregir la inferencia.
# Esto lo haremos utilizando los errores estandar robustos de White.


# Estimamos el modelo lineal

desercion_mlp <- lm(modelo_desercion, data = data_d)

# Mostramos la estimaci?n utilizando la matriz de varianzas y covarianzas de White

desercion_mlp_robust <- coeftest(desercion_mlp, vcov = vcovHC(desercion_mlp, "HC1"))  

#stargazer(desercion_mlp_robust, desercion_mlp, type = "text")


#### Punto 5 #### 

# No hacemos el reemplazo de las observaciones que tienen ingreso igual a cero, dado que consideramos 
# que no son informativas de la probabilidad de decersión escolar

# Generamos la nueva variable

data_d$ln_ing <- log(data_d$ingreso_per_capita, base = exp(1))

# Exportamos las estimaciones:

stargazer(desercion_probit_robust, desercion_mlp_robust, type='latex',
          dep.var.labels=c("Deserta", "Deserta"),
          covariate.labels = c("Mujer", "Educación JH (missing)", "Educación JH (EGB)", "Educación JH (Secundario)",
                               "Educación JH (Polimodal)", "Educación JH (Terciario)", "Educación JH (Universitario)", 
                               "Cantidad de hermanos", "Ingreso per cápita", "Jefe de hogar mujer", "Establecimiento educativo (missing)", 
                               "Establecimiento educativo (público)", "Establecimiento educativo (no responde)"),
          notes = "Robust standard errors in parentheses")

#covariate.labels = c("Educacion",  "Edad", "Edad2", "Race", "Married", 
#                               "SMSA", "Primer trimestre", "Segundo trimestre", 
#                               "Tercer trimestre"), align = TRUE)


#### Punto 6 #### 

# Generamos las bases de datos 


ingreso <- seq(0,100000,100)
k <- length(ingreso)



pred_mujer <- predict(desercion_probit, newdata = data.frame(mujer=rep(1,k),
                                                     educ_jefe= rep(mean(data_d$educ_jefe), k),
                                                     hermanos = rep(mean(data_d$hermanos), k),
                                                     ingreso_per_capita=ingreso,
                                                     educ_jefe_0 = rep(0,k),
                                                     educ_jefe_3 = rep(1,k),
                                                     educ_jefe_4 = rep(0,k),
                                                     educ_jefe_5 = rep(0,k),
                                                     educ_jefe_6 = rep(0,k),
                                                     educ_jefe_7 = rep(0,k),
                                                     jmujer= rep(1,k),
                                                     ch11_0= rep(1,k),
                                                     ch11_1=rep(0,k),
                                                     ch11_9=rep(0,k), type="response"))


pred_hombre <- predict(desercion_probit, newdata = data.frame(mujer=rep(0,k),
                                                             educ_jefe= rep(mean(data_d$educ_jefe), k),
                                                             hermanos = rep(mean(data_d$hermanos), k),
                                                             ingreso_per_capita=ingreso,
                                                             educ_jefe_0 = rep(0,k),
                                                             educ_jefe_3 = rep(1,k),
                                                             educ_jefe_4 = rep(0,k),
                                                             educ_jefe_5 = rep(0,k),
                                                             educ_jefe_6 = rep(0,k),
                                                             educ_jefe_7 = rep(0,k),
                                                             jmujer= rep(1,k),
                                                             ch11_0= rep(1,k),
                                                             ch11_1=rep(0,k),
                                                             ch11_9=rep(0,k), type="response"))



pred_jm <- predict(desercion_probit, newdata = data.frame(mujer=rep(1,k),
                                                           educ_jefe= rep(mean(data_d$educ_jefe), k),
                                                           hermanos = rep(mean(data_d$hermanos), k),
                                                           ingreso_per_capita=ingreso,
                                                           educ_jefe_0 = rep(0,k),
                                                           educ_jefe_3 = rep(1,k),
                                                           educ_jefe_4 = rep(0,k),
                                                           educ_jefe_5 = rep(0,k),
                                                           educ_jefe_6 = rep(0,k),
                                                           educ_jefe_7 = rep(0,k),
                                                           jmujer= rep(1,k),
                                                           ch11_0= rep(1,k),
                                                           ch11_1=rep(0,k),
                                                           ch11_9=rep(0,k), type="response"))



pred_jh <- predict(desercion_probit, newdata = data.frame(mujer=rep(1,k),
                                                       educ_jefe= rep(mean(data_d$educ_jefe), k),
                                                       hermanos = rep(mean(data_d$hermanos), k),
                                                       ingreso_per_capita=ingreso,
                                                       educ_jefe_0 = rep(0,k),
                                                       educ_jefe_3 = rep(1,k),
                                                       educ_jefe_4 = rep(0,k),
                                                       educ_jefe_5 = rep(0,k),
                                                       educ_jefe_6 = rep(0,k),
                                                       educ_jefe_7 = rep(0,k),
                                                       jmujer= rep(0,k),
                                                       ch11_0= rep(1,k),
                                                       ch11_1=rep(0,k),
                                                       ch11_9=rep(0,k), type="response"))


# Guardamos todo en la misma base de datos 

df_final <- data.frame(
  pred_jm, pred_jh, pred_hombre, pred_mujer)


# Graficamos 


ggplot()+
  geom_line(data=df_final,aes(y=pred_jm,x= ingreso,colour="Mujer"),size=1 )+
  geom_line(data=df_final,aes(y=pred_jh,x= ingreso,colour="Hombre"),size=1) +
  scale_color_manual(name = "Jefe de hogar", values = c("Mujer" = "burlywood2", "Hombre" = "azure3"))+
  ggtitle("Probabilidad de deserci?n en funci?n del ingreso para jefes de hogar") +
  xlab("Ingreso per capita")+
  ylab("Predicci?n")+
  theme_bw()


ggsave(file="jefehogar.eps", width=6.5, height=4, dpi=300)



ggplot()+
  geom_line(data=df_final,aes(y=pred_mujer,x= ingreso,colour="Mujer"),size=1 )+
  geom_line(data=df_final,aes(y=pred_hombre,x= ingreso,colour="Hombre"),size=1) +
  scale_color_manual(name = "Sexo", values = c("Mujer" = "burlywood2", "Hombre" = "azure3"))+
  ggtitle("Probabilidad de deserci?n en funci?n del ingreso para jefes de hogar") +
  xlab("Ingreso per capita")+
  ylab("Predicci?n")+
  theme_bw()

ggsave(file="sexo.eps", width=6.5, height=4, dpi=300)



