
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

marginal_media <- probitmfx(deserta ~ mujer + educ_jefe + hermanos + ingreso_per_capita + jmujer +
                              ch11_0 + ch11_1 + ch11_9 , data = data_d,
                            atmean = TRUE, robust = TRUE) 

# En la media para hombres y mujeres 

prueba1 <- margins(myprobit, at = list(mujer = 0:1))

# Mediana 

prueba2 <- margins(myprobit, at = list(mujer = median(data_d$mujer), educ_jefe = median(data_d$educ_jefe), 
                                       hermanos = median(data_d$hermanos), ingreso_per_capita = median(data_d$ingreso_per_capita), 
                                       jmujer = median(data_d$jmujer), ch11_0 = median(data_d$ch11_0), ch11_1 = median(data_d$ch11_1), ch11_9 = median(data_d$ch11_9)))

# Moda


prueba3 <- margins(myprobit, at = list(mujer = mfv(data_d$mujer), educ_jefe = mfv(data_d$educ_jefe), 
                                       hermanos = mfv(data_d$hermanos), ingreso_per_capita = mfv(data_d$ingreso_per_capita), 
                                       jmujer = mfv(data_d$jmujer), ch11_0 = mfv(data_d$ch11_0), ch11_1 = mfv(data_d$ch11_1), ch11_9 = mfv(data_d$ch11_9)))

# En valores específicos (mínimo ingreso, minima educación del jefe de hogar, minima cantidad de hermanos)

prueba4 <- margins(myprobit, at = list(mujer = mfv(data_d$mujer), educ_jefe = min(data_d$educ_jefe), 
                                       hermanos = min(data_d$hermanos), ingreso_per_capita = min(data_d$ingreso_per_capita), 
                                       jmujer = mfv(data_d$jmujer), ch11_0 = mfv(data_d$ch11_0), ch11_1 = mfv(data_d$ch11_1), ch11_9 = mfv(data_d$ch11_9)))



prueba5 <- margins(myprobit, at = list(mujer = mfv(data_d$mujer), educ_jefe = max(data_d$educ_jefe), 
                                       hermanos = max(data_d$hermanos), ingreso_per_capita = max(data_d$ingreso_per_capita), 
                                       jmujer = mfv(data_d$jmujer), ch11_0 = mfv(data_d$ch11_0), ch11_1 = mfv(data_d$ch11_1), ch11_9 = mfv(data_d$ch11_9)))


# Queda armar la tabla VER 


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


#### Punto 6 #### 

# Generamos las bases de datos 


ingreso <- seq(0,100000,100)
k <- length(ingreso)


pred_mujer <- predict(desercion_probit, newdata = data.frame(mujer=rep(1,k),
                                                     educ_jefe= rep(mean(data_d$educ_jefe), k),
                                                     hermanos = rep(mean(data_d$hermanos), k),
                                                     ingreso_per_capita=ingreso,
                                                     jmujer= rep(1,k),
                                                     ch11_0= rep(1,k),
                                                     ch11_1=rep(0,k),
                                                     ch11_9=rep(0,k), type="response"))


pred_hombre <- predict(desercion_probit, newdata = data.frame(mujer=rep(0,k),
                                                             educ_jefe= rep(mean(data_d$educ_jefe), k),
                                                             hermanos = rep(mean(data_d$hermanos), k),
                                                             ingreso_per_capita=ingreso,
                                                             jmujer= rep(1,k),
                                                             ch11_0= rep(1,k),
                                                             ch11_1=rep(0,k),
                                                             ch11_9=rep(0,k), type="response"))



pred_jm <- predict(desercion_probit, newdata = data.frame(mujer=rep(1,k),
                                                           educ_jefe= rep(mean(data_d$educ_jefe), k),
                                                           hermanos = rep(mean(data_d$hermanos), k),
                                                           ingreso_per_capita=ingreso,
                                                           jmujer= rep(1,k),
                                                           ch11_0= rep(1,k),
                                                           ch11_1=rep(0,k),
                                                           ch11_9=rep(0,k), type="response"))



pred_jh <- predict(desercion_probit, newdata = data.frame(mujer=rep(1,k),
                                                       educ_jefe= rep(mean(data_d$educ_jefe), k),
                                                       hermanos = rep(mean(data_d$hermanos), k),
                                                       ingreso_per_capita=ingreso,
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
  ggtitle("Probabilidad de deserción en función del ingreso para jefes de hogar") +
  xlab("Ingreso per capita")+
  ylab("Predicción")+
  theme_bw()


ggsave(file="jefehogar.eps", width=6.5, height=4, dpi=300)



ggplot()+
  geom_line(data=df_final,aes(y=pred_mujer,x= ingreso,colour="Mujer"),size=1 )+
  geom_line(data=df_final,aes(y=pred_hombre,x= ingreso,colour="Hombre"),size=1) +
  scale_color_manual(name = "Sexo", values = c("Mujer" = "burlywood2", "Hombre" = "azure3"))+
  ggtitle("Probabilidad de deserción en función del ingreso para jefes de hogar") +
  xlab("Ingreso per capita")+
  ylab("Predicción")+
  theme_bw()

ggsave(file="sexo.eps", width=6.5, height=4, dpi=300)



