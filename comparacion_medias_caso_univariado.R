#### Muestras normales independientes con varianzas conocidas ####

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

abs(estadistico)<valor_critico # Rechazo, pues cae en la región critica

(p_value = pnorm(estadistico, lower.tail = F)) * 2 # Al ser de 2 colas

# Construyo IC

IC_var = valor_critico * sqrt(varianzas[1]/n[1] + varianzas[2]/n[2] )
(IC1 = medias[1]-medias[2]+IC_var)
(IC2 = medias[1]-medias[2]-IC_var)
rbind(IC1, IC2)

# Con una confianza del 95% el intervalo construido contiene el verdadero valor
# de la diferencia entre las medias de pH de las dos regiones
# COmo el cero no pertenece al intervalo, coincide con la decision tomada por test de hipotesis


#### Muestras normales independientes con varianzas desconocidas ####

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

#### Muestras independientes de poblaciones cualesquiera ####

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

abs(estadistico)<valor_critico # No rechazo, pues no cae en la región critica

(p_value = pnorm(estadistico, lower.tail = F)) * 2 # Al ser de 2 colas



#### Muestras apareadas ####


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

abs(estadistico)<=valor_critico # Rechazo, pues cae en la región critica


# Estimo el IC

alpha = 0.05
(valor_critico_2 <- qt(alpha/2,df = nrow(fumar)-1 ,lower.tail=F )  )

(IC1 <- (mean(fumar$diferencia) - (sd(fumar$diferencia)/sqrt(nrow(fumar)))*valor_critico_2))
(IC2 <- (mean(fumar$diferencia) + (sd(fumar$diferencia)/sqrt(nrow(fumar)))*valor_critico_2))


### Pruebas no paramétricas para dos muestras independientes

#### Test de Mann-Whitney-Wilcoxon ####
# Alternativas libres de distribución, basados en rangos o scores, cuando la normalidad no se satisface

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


