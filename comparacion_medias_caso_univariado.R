# Muestras normales independientes con varianzas conocida -----------------


# Problema de pH

medias <- c(6.58, 5.74) # Muestrales
varianzas <- c(0.85, 1.22) # Poblacionales  
n <- c(20, 20)

# H0:media X - media Y = 0
# H1:media X - media Y != 0

(estadistico = (medias[1]-medias[2]) /
  sqrt(varianzas[1]/n[1] + varianzas[2]/n[2] ))

alpha = 0.05 # 1-Nivel de confianza
valor_critico <- qnorm(alpha/2,lower.tail=T )  

abs(estadistico)<valor_critico # Rechazo, pues cae en la regiÃ³n critica

(p_value = pnorm(estadistico, lower.tail = F)) * 2 # Al ser de 2 colas

# Construyo IC

IC_var = valor_critico * sqrt(varianzas[1]/n[1] + varianzas[2]/n[2] )
(IC1 = medias[1]-medias[2]+IC_var)
(IC2 = medias[1]-medias[2]-IC_var)
rbind(IC1, IC2)

# Con una confianza del 95% el intervalo construido contiene el verdadero valor
# de la diferencia entre las medias de pH de las dos regiones
# COmo el cero no pertenece al intervalo, coincide con la decision tomada por test de hipotesis




# Muestras normales independientes con varianzas desconocidas -------------


# Habichuelas

medias <- c(17.2, 18.3) # Muestrales
desvios <- c(0.7, 0.8) # Muestrales  
n <- c(15, 15)

# H0:media X - media Y >= 0 # Pruebo unilateral derecha
# H1:media X - media Y < 0

# Necesitamos asumir que tienen misma varianza poblacional

# Calculo varianza amalgamada

(sp = sqrt(((n[1]-1)*desvios[1]**2 + (n[2]-1)*desvios[2]**2)/(n[1]+n[2]-2)))


(estadistico = (medias[2]-medias[1]) /
    (sp * sqrt(1/n[1] + 1/n[2])))

alpha = 0.01 # 1- Nivel de confianza
(valor_critico <- qt(alpha,df = n[1]+n[2]-2 ,lower.tail=F )  ) #t 28 , 0.99
estadistico <= valor_critico # Rechazo H0


(p_value = pt(estadistico,df = n[1]+n[2]-2, lower.tail = F)) # Al ser de 2 colas

# Construyo IC

(valor_critico_2 <- qt(alpha/2,df = n[1]+n[2]-2 ,lower.tail=F )  ) #t 28 , 0.99
# Para el IC lo construyo de 2 colas

IC_var = valor_critico_2 * sp * sqrt(1/n[1] + 1/n[2])
(IC1 = medias[2]-medias[1]+IC_var)
(IC2 = medias[2]-medias[1]-IC_var)
rbind(IC1, IC2) # Los limites son los valores de la diferencia para que caiga sobre el borde



# Muestras independientes de poblaciones cualesquiera ---------------------


# Si las muestras son lo suficientemente grandes, aplicamos la dist normal para los promedios muestrales

medias <- c(6.6, 5.4) # Muestrales
desvios <- c(4.3, 3.6) # Muestrales  
n <- c(124, 110)

# H0:media X - media Y = 0
# H1:media X - media Y != 0


(estadistico = (medias[1]-medias[2]) /
    sqrt(desvios[1]**2/n[1] + desvios[2]**2/n[2] ))

alpha = 0.01 # 1-Nivel de confianza
(valor_critico <- qnorm(alpha/2,lower.tail=F ))

abs(estadistico)<valor_critico # No rechazo, pues no cae en la regiÃ³n critica

(p_value = pnorm(estadistico, lower.tail = F)) * 2 # Al ser de 2 colas





# Muestras apareadas ------------------------------------------------------



(fumar <- data.frame(sujeto = c(1:12), 
                    anterior = c(60,78,64,68,72,76,74,46,48,60,64,27),
                    posterior = c(70,82,66,69,75,72,77,49,54,66,69,36)
                    ))

# Aqui las 2 muestras no son independientes
# El interes no es evaluar si la media del primer conjunto es diferente a la del segundo
# Si no estudiar la media de las diferencias por individuo


# H0: media de las diferencias es <= 0 
# H1: media de las diferencias es mayor a 0
# Es un test unilateral derecho

# Lo que se pide es la normalidad de las diferencias no de las muestras

fumar$diferencia = fumar$posterior - fumar$anterior
qqnorm(fumar$diferencia)

(estadistico = (mean(fumar$diferencia)) / (sd(fumar$diferencia)/sqrt(nrow(fumar))))

alpha = 0.05
(valor_critico <- qt(alpha,df = nrow(fumar)-1 ,lower.tail=F )  )

abs(estadistico)<=valor_critico # Rechazo, pues cae en la regiÃ³n critica


# Estimo el IC

alpha = 0.05
(valor_critico_2 <- qt(alpha/2,df = nrow(fumar)-1 ,lower.tail=F )  )

(IC1 <- (mean(fumar$diferencia) - (sd(fumar$diferencia)/sqrt(nrow(fumar)))*valor_critico_2))
(IC2 <- (mean(fumar$diferencia) + (sd(fumar$diferencia)/sqrt(nrow(fumar)))*valor_critico_2))


### Pruebas no paramÃ©tricas para dos muestras independientes



# Test de Mann-Whitney-Wilcoxon -------------------------------------------

# Alternativas libres de distribucion, basados en rangos o scores, cuando la normalidad no se satisface



arbequina <- data.frame(aceite = c(34.5,20.1,21.8,18.2,19.5,20.2,22.5,23.9,22.1,24.2))
carolea <- data.frame(aceite = c(16.4,14.8,17.8,12.3,11.9,15.5,13.4,16.0,15.8,16.2))

shapiro.test(arbequina$aceite)  # Rechazo H0 de normalidad
shapiro.test(carolea$aceite) # No rechazo H0 de normalidad

# Como en un caso rechazo normalidad no podemos aplicar el test T y usamos Mann-Whitney-Wilcoxon
wilcox.test(arbequina$aceite, carolea$aceite, alternative = "two.sided") # Rechazo igualdad de medias

# Para poder decir que las medias son diferentes deben tener dist. similar
# SI no solo digo que las distribuciones son diferentes

arbequina$variedad = "arbequina"
carolea$variedad = "carolea"

df = rbind(arbequina, carolea)

ggplot(df, aes(variedad, aceite))+
  geom_boxplot(mapping = aes(fill = variedad))

# Aqui las distribuciones son similares por tanto vale la comparacion de medianas, pero no siempre es el caso
# Si no es asi, disponemos de un test que no tiene supuestos sobre las distribuciones de los grupos



# Test de la Mediana ------------------------------------------------------


# Este se generaliza a más de 2 poblaciones y no requiere supuestos de igualdad distribucional

# H0: mediana de las poblaciones independientes son iguales
# H1: alguna de las medianas de las pobs. independientes difiere

library(RVAideMemoire)

mood.medtest(aceite~variedad, data = df )
# Rechazo H0 porque el p-valor es pequeño




# ANOVA -------------------------------------------------------------------


# Queremos comparar la media de varios grupos
# Esta prueba requiere que las variables aleatorias sean normales
# y homocedásticas, esto es, tengan igual varianza

# H0: todas las medias poblacionales son iguales
# H1: al menos una media poblacional difiere del resto

te <- data.frame(marca= c(rep("Marca 1", 7), rep("Marca 2", 6), rep("Marca 3",6), rep("Marca 4",6)),
                 vitamina_b = c(7.9, 6.2, 6.6, 8.6, 8.9, 10.1, 9.6 , 5.7, 7.5, 9.8, 6.1, 8.4, 7.2, 6.8, 7.8, 5.1 , 7.4 , 5.3, 6.1 , 6.4 , 7.1 , 7.9 , 4.5 , 5, 4)
                 )

proms <- aggregate(. ~ marca, te, mean)
desvios <- aggregate(. ~ marca, te, sd)
varianzas <- aggregate(. ~ marca, te, var)
n <- aggregate(. ~ marca, te, length)

ggplot(te, mapping = aes(y = vitamina_b, x= marca, fill = marca))+
  geom_boxplot()


n_total = sum(n$vitamina_b)
k = length(te_list) 

(ssw = (varianzas[1,2] * (n[1,2]-1) +  varianzas[2,2] * (n[2,2]-1)
+varianzas[3,2] * (n[3,2]-1)+ varianzas[4,2] * (n[4,2]-1)) / (n_total-k) )

x_prom = mean(te$vitamina_b)

(ssb = ((proms[1,2]-x_prom)**2 * n[1,2] + (proms[2,2]-x_prom)**2 * n[2,2]+
  (proms[3,2]-x_prom)**2 * n[3,2] + (proms[4,2]-x_prom)**2 * n[4,2])
  / (k-1)
  )

estadistico = ssb/ssw

freedom_degrees_num = k - 1
freedom_degrees_den = n_total - k




estadistico<= qf(.95, freedom_degrees_num, freedom_degrees_den)

(p_valor <- pf(estadistico, freedom_degrees_num, freedom_degrees_den, lower.tail = F))
# Rechazamos con NC 95% pero no rechazamos con 99%

# Usemos el test de R

te_anova = aov(vitamina_b~marca, te)
summary(t_anova) # Rechazamos H0: al menos una media difiere




# Para que el test F sea válido el modelo de k muestras normales independientes con varianzas iguales
# tiene que ser aproximadamente cierto.





# Tests de Homocedasticidad ------------------------------------------------

# Test de Bartlet

# H0: todas las varianzas poblacionales son iguales
# H1: al menos una no lo es

bartlett.test(te$vitamina_b, te$marca)
# No rechazamos homocedasticidad
# Pero este test es muy sensible a la falta de normalidad, rechazando a veces
# porque no se cumple la normalidad y no por la falta de homocedasticidad

# Test de Levene

# Es más robusto que el anterior

library(car)

leveneTest(te$vitamina_b, te$marca)
# Tampoco rechazamos homocedasticidad aqui



# Tests de Normalidad -----------------------------------------------------

# QQPlot

# La evaluamos gráficamente

y=quantile ( te$vitamina_b, c (0.25 , 0.75) , type=5)
# Encuentra los cuartiles 1 y 3 para la muestra
x <- qnorm( c (0.25 , 0.75))
# Encuentra los cuartiles 1 y 3 para la distribuci ón Normal
slope <- diff (y) / diff (x) # Calcula la pendiente de la recta de regresi ón
int <- y[1] - slope * x[1] # Calcula la constante de la recta de regresi ón

ggplot (te , aes (sample=residuals ( te_anova ))) +
  stat_qq( alpha = 0.5 , color="royalblue") +
  xlab ("Valores teóricos") +
  ylab ("Valores de la muestra") +
  geom_abline ( int=int , slope=slope , color="indianred")


qqnorm(te_anova$residuals)
qqline(te_anova$residuals)


# Los Tests se aplican sobre los residuos de la ANOVA

# Test de Shapiro

shapiro.test(te_anova$residuals) # No rechazamos normalidad

# Test de Anderson-Darlin

library(nortest)

ad.test(te_anova$residuals) # Tampoco rechazamos normalidad

# Test de D'Agostino

library(moments)

agostino.test(te_anova$residuals) # Tampoco rechazamos normalidad


# Comparaciones a posteriori ----------------------------------------------

# Si rechazamos por ANOVA, queremos comparar las medias poblacionales de a pares, para ver cuales son distintas
# Se disponen varios tests para eso

conejos = data.frame(dieta = c(rep("Dieta 1", 8), rep("Dieta 2", 8), rep("Dieta 3", 8))
                     , colesterol = c(13.4, 11, 15.3, 16.7, 13.4, 20.1, 13.6, 18.3,
                                      10.4, 14.2, 20.5, 19.6, 18.5, 24.0, 23.4, 13.6,
                                      7.5, 7.2, 6.7, 7.6, 11.2, 9.6, 6.8, 8.5)
                     )
                     
      
ggplot(conejos, aes(dieta, colesterol))+
  geom_boxplot(mapping = aes(fill = dieta))+
  scale_fill_brewer( palette="Pastel1")

# Las distribuciones parecen distintas y no hay outliers

library(dplyr)

(resumen <- conejos %>%
  group_by(dieta) %>%
  summarise_at(vars(colesterol), funs(mean, var, sd, n(),)))

#pordieta=split ( conejos$colesterol , conejos$dieta ) # Separa los datos según dieta
#lapply ( pordieta ,mean) # Calcula las medias
#lapply ( pordieta , sd) # Calcula los desv í os estándar

conejos_anova = aov(colesterol ~ dieta, data = conejos)
summary(conejos_anova)
# Rechazamos igualdad de medias

# Vemos si los supuestos se cumplen

# Evaluamos normalidad
shapiro.test(conejos.anova$residuals)

# Y homocedasticidad

leveneTest(colesterol~dieta, data = conejos)
# Rechazamos homocedasticidad

# Se intenta transformacion de Box-Cox para cambiar la situación
library(MASS)

bc <- boxcox(colesterol~dieta, data = conejos)
max_bc <- bc$x[which(bc$y == max(bc$y))]

# Vemos que el maximo se alcanza alrededor de -0.5

# Hacemos ANOVA para los datos transformados

t_conejos_anova = aov(colesterol**max_bc ~ dieta, data = conejos)
summary(t_conejos_anova)
# Rechazamos igualdad de medias otra vez, pero debemos reevaluar los supuestos

shapiro.test(t_conejos_anova$residuals) # Normalidad OK
leveneTest(colesterol**max_bc~dieta, data = conejos) # Ahora homocedasticidad OK

# Por ultimo queremos saber, como es la intencion del apartado, cuales medias difieren

# Test de Tukey

TukeyHSD(t_conejos_anova, conf.level = .95)
# Donde vemos que la media de la dieta 3 difiere signif. de la de las otras 2
# Como era de esperarse de acuerdo al boxplot




# Test de Kruskal-Wallis --------------------------------------------------

# Esta prueba contrasta la hipótesis nula que establece que las k muestras independientes proceden
# de la misma población y, en particular, todas ellas tienen la misma posición central

# H0: Todas las medianas poblacionales son iguales
# H1: Alguna al menos no lo es

alumnos <- data.frame(grupo = c(rep("A", "6"), rep("B", 6), rep("C",7)),
                      puntajes = c(13,27,26,22,28,27
                                   ,43,35,47,32,31,37,
                                   33,33,33,26,44,33,54))
ggplot (alumnos , aes (x=grupo , y=puntajes , fill = grupo)) +
  geom_boxplot () +
  xlab ("") +
  scale_fill_brewer( palette="Pastel1")
# Produce boxplots


grupoA <- alumnos[alumnos$grupo=="A" ,2]
grupoB <- alumnos[alumnos$grupo=="B" ,2]
grupoC <- alumnos[alumnos$grupo=="C" ,2]

shapiro.test (grupoA) # Rechazamos normalidad en A
shapiro.test (grupoB)
shapiro.test (grupoC)


# Usamos entonces Kruskal-Wallis

library (pgirmess) 


kruskal.test(alumnos$puntajes,alumnos$grupo)

# Luego hago comparaciones de a pares para ver los grupos diferentes

kruskalmc(alumnos$puntajes,alumnos$grupo) # Vemos como A difiere de los otros 2

