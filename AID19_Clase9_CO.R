library(ggplot2)
library(stats)
library(reshape2)
library(car)
library(nortest)


###Ejemplo: ANOVA de 1 factor utilizando transformaciones de Box-Cox

#Con la intención de evaluar la eficacia de un medicamento en el nivel de 
#alerta de unos pacientes, tres dosis (a, b, c) de un determinado fármaco
#se administraron a 18 sujetos. Se pide analizar la eficacia del medicamento.

#Tener en cuenta que el Test no paramétrico de Kruskal Wallis no requiere 
#normalidad de los datos, pero sí homogeneidad de la varianza (homocedasticidad)!!!

dosis<-c(rep("a",6),rep("b",8),rep("c",4))
alerta<-c(30,38,35,41,27,24,32,26,31,29,27,35,21,25,17,21,20,19)

data <- data.frame(dosis,alerta)
head(data,8)
dim(data)

#Para analizar la eficacia del medicamento, veamos si existen diferencias 
#entre las medias de las tres dosis. Aplicamos ANOVA.

aov.data <- aov(alerta~dosis,data=data)
summary(aov.data)

#Se puede ver que existen diferencias estadísticamente significativas 
#entre los niveles del fármaco (p=0.00298 <0.05). 
#Para asegurar la validez de esta afirmación realizamos las siguientes pruebas diagnósticas.

#La siguiente tabla muestra las medias total y por nivel de los 3 
#medicamentos:

(model.tables(aov.data,"means"))

#Analizamos normalidad:

qqnorm(resid(aov.data))
qqline(resid(aov.data))

shapiro.test(residuals(aov.data)) # No rechazo la H0 de normalidad

ad.test(residuals(aov.data)) # Idem conclusion
agostino.test(residuals(aov.data))


#Analizamos igualdad de varianzas (homoscedasticidad):

boxplot(split(data$alerta,data$dosis),ylab="Alerta",xlab="Dosis") # Parece haber diferencia entre varianzas por el ancho de las cajas

#Las varianzas estimadas en cada grupo son:
tapply(data$alerta,data$dosis,var,na.rm=TRUE)

bartlett.test(alerta,dosis) # No rechaza; pero es sensible al desvío de normalidad

leveneTest(alerta~as.factor(dosis)) # Rechaza, pero es más confiable, ver nota abajo.

#OJO: Hay que tener en cuenta el tamaño muestral, cuando el tamaño 
#de la muestra es pequeño, incluso grandes desviaciones de la normal 
#no se detectan, y cuando el tamaño de la muestra es grande, incluso 
#la más mínima desviación de la normalidad logra rechazar la hipótesis
#nula. En este caso, al ser la muestra pequeña y el test de Bartlett 
#sensible a las desviaciones de la normalidad, este test no detecta 
#diferencia de varianzas (heterocedasticidad) en los niveles del factor
#(dosis). Por eso, es conveniente utilizar el test de Levene, el cual 
#rechaza la homoscedasticidad, lo que indica que NO se cumple uno de los
#supuestos del ANOVA.

#Para resolver este problema, puede ser útil alguna transformación de 
#Box-Cox:

library(MASS)
a <- boxcox(alerta~dosis,data=data,lambda = seq(-4, 4, 1/100), plotit=TRUE)# el máximo lambda se alcanza en -1.
a$x[which(a$y == max(a$y))]

#Se repite el procedimiento para la variable transformada, y se revisa 
#el cumplimiento de supuestos para aplicar ANOVA.

aov.data2=aov(alerta^(-1.03)~dosis,data=data)
summary(aov.data2)

#Se obtiene, como con la variable original, diferencias estadísticamente 
#significativas entre los niveles del factor dosis.

#Revisión de supuestos necesarios para aplicar ANOVA

qqnorm(resid(aov.data2))
qqline(resid(aov.data2))

shapiro.test(residuals(aov.data2)) # Sigue siendo normal
ad.test(residuals(aov.data)) # Idem
leveneTest(alerta^(-1.03)~as.factor(dosis),data=data) # Ahora hay homocedasticidad
leveneTest(y = alerta^(-1.03),group = as.factor(dosis),data=data, center = "mean") # Otra forma, y usando media


#Con la transformación de Box-Cox realizada se verifican los supuestos
#necesarios y por lo tanto el resultado del ANOVA aplicado es válido.

#Los intervalos de confianza simultáneos para las diferencias de medias 
#de Tukey resultan:

TukeyHSD(aov.data2,conf.level=0.95) #A y B tienen media similar, pero C difiere de ambas
# El IC de los son iguales contienen al 0
# Ojo que las diferencias son de la variable transformada, por lo tanto su magnitud no es tan relevante

####################################
####################################
#Ejemplo: Test no paramétrico de Kruskal Wallis para k muestras independientes

#Un estudio compara el número de huevos que pone un determinado insecto bajo 3 condiciones 
#distintas. ¿Existen diferencias significativas dependiendo de las condiciones?

datos <- data.frame(condicion = c(rep("condicion1", 18), rep("condicion2", 18), rep("condicion3", 18)), n_huevos = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 27, 28, 29, 30, 51, 52, 53, 342, 40, 41, 42, 43, 44, 45, 46, 47, 48, 67, 88, 89, 90, 91, 92, 93, 94, 293, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 25, 36, 37, 58, 59, 60, 71, 72)) 
head(datos)

ggplot(data = datos, mapping = aes(x = condicion, y = n_huevos, colour = condicion)) + 
  geom_boxplot() + theme_bw() + theme(legend.position = "none")

ggplot(data = datos, mapping = aes(x = n_huevos, colour = condicion)) + 
  geom_histogram() + theme_bw() + facet_grid(. ~ condicion) + 
  theme(legend.position = "none")# + stat_bin(binwidth=30)

leveneTest(n_huevos ~ condicion, data = datos)

kruskal.test(n_huevos ~ condicion, data = datos)

install.packages("pgirmess")
library(pgirmess)
kruskalmc(datos$n_huevos ~ datos$condicion)
####################################
####################################

?leveneTest
