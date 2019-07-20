# In LDA we assume equality of covariance matrix for all of the classes. 
# QDA assumes different covariance matrices for all the classes.


# LDA a mano ---------------------------------------------------------------------

# Este analisis solo tiene sentido cuando las medias de los grupos difieren significativamente
# La ausencia de normalidad multivariante o la presencia de outliers conlleva a problemas de estimacion
# No basta con que cada variable sea normal univariado, pues la distribucion conjunta puede no ser normal pese a ello.
# Aunque si una ya no es univariada, seguro la distribución conjunta no es normal multivariada.

# Si las matrices de covarianza son distintas se usa el análisis de discriminante cuadrático, no este.
# Se usa entonces prueba M de Box o, más robusto, Levene Multivariado
# Si se rechaza homocedasticidad una alternativa es estandarizar por separado cada grupo con su respectiva matriz de varianzas-covarianzas estimada, de
# modo tal de obtener grupos con igual matriz de varianzas-covarianzas y, sobre este nuevo espacio de representación, calcular el discriminante lineal.

# Los coeficientes estandarizados de la función discriminante son los que corresponden al cálculo de la
# función discriminante con todas las variables clasificadoras estandarizadas.
# Los coeficientes estandarizados aij pueden interpretarse como indicadores de la importancia
# relativa de cada una de las variables en cada función discriminante. Si el valor abs. de grande, es importante en la f(x) discrminante.

# Estos coeficientes son poco fiables si existen problemas de multicolinealidad entre las variables clasificadoras.

# Además recordar que no todos los conjuntos son linealmente separables

# En algunas ocasiones es necesario ponderar los errores cometidos. 
# Una manera de diferenciación en la regla discriminante entre los dos tipos de errores posibles es asignar un costo a cada error.


library(readxl)
avispas <- read_excel("C:/Users/hestrin/Desktop/Maestria DM/Especializacion/AID/Segunda Parte/avispas.xlsx")

# Diagrama de dispersión

ggplot ( avispas , aes (Antena , Pata ) ) +
  geom_point ( aes(colour= Especie ) ) +
  xlab ( 'Longitud de la antena (mm)' ) +
  ylab ( 'Longitud de l a pata (mm)' )

# Medias

especie.avispa = split(avispas, avispas$Especie)
(prom.esp1 = apply ( especie.avispa [[1]] [,1:2] , 2 , mean))
(prom.esp2 = apply ( especie.avispa [[2]] [,1:2] , 2 , mean))
(prom.total= apply ( avispas [ ,1:2] , 2 ,mean))

# Varianzas

S1= var (especie.avispa [[1]] [,1:2] )
round ( S1 , 4 )
S2= var ( especie.avispa [[2]] [,1:2])
round ( S2 , 4 )

S_comun = ((nrow(especie.avispa[[1]])-1)*S1 + (nrow(especie.avispa[[2]])-1)*S2) / (nrow(avispas)-2)




d2 <- t(prom.esp1-prom.esp2)%*% solve(S_comun) %*% (prom.esp1-prom.esp2)

# Hotelling con p, (n1+n2-2) g.l. 
estadistico_t = (nrow(especie.avispa[[1]]) *nrow(especie.avispa[[2]])) /
                   (nrow(especie.avispa[[1]]) + nrow(especie.avispa[[2]])) * d2 # con 2, 13 g.l.

# F con p, (n1+n2-p-1) g.l. 
estadistico_f = (nrow(especie.avispa[[1]]) + nrow(especie.avispa[[2]]) - (ncol(avispas)-1) - 1)/
  ((ncol(avispas)-1)*(nrow(especie.avispa[[1]]) + nrow(especie.avispa[[2]]) - 2)) * estadistico_t # Con 2, 12 g.l.
         

nc = .95           
p_value <- pf(estadistico_f, df1 = 2, df2 = 12, lower.tail = F)
valor_critico <- qf(nc, df1 = 2, df2 = 12, lower.tail = T) # T es acumulada desde la izquierda, F desde la derecha

estadistico_f <= valor_critico
p_value >= 1-nc
# Rechazo H0 de igualdad de vectores de media. Puedo pasar a hacer LDA.

W = S_comun*(nrow(avispas)-length(especie.avispa))
B = (prom.esp1 - prom.total)%*% t(prom.esp1 - prom.total) + (prom.esp2 - prom.total) %*% t(prom.esp2 - prom.total) 

matriz_discrim = solve(W)%*%B

# Primer autovector
avect1 = eigen(matriz_discrim)$vectors[,1] 

# Centroides por clase
centroide.esp1 = prom.esp1%*%avect1
centroide.esp2 = prom.esp2%*%avect1

# Puntaciones discriminantes
proyeccion = as.matrix(avispas[,1:2])%*%avect1

# Punto de corte
(corte = centroide.esp1  + (nrow(especie.avispa[[1]])/nrow(avispas))*(centroide.esp2 - centroide.esp1))

clase = 0

for (i in 1:nrow(avispas)){
  ifelse(proyeccion[i] > corte, 
         clase[i] <- "chaqueta amarilla", clase[i] <- "negra pequena")}
 # Comparo resultados vs original

table(clase, avispas$Especie)


# Uso el algoritmo
library(MASS)

z = lda(Especie ~ Antena + Pata, avispas, method = "mle")


#Puntaciones pero me falta encontrar la constante

cbind(as.matrix(avispas[,1:2])%*%z$scaling, avispas$Especie)


# Los scores los saco aplicando la prediccion del modelo a la data original

cbind(round(predict(z, newdata = avispas[,1:2])$x,12),avispas$Especie)

# Saco la constante restando los scores de la prediccion menos los obtenidos a mano

constante <- mean(predict(z, newdata = avispas[,1:2])$x - as.matrix(avispas[,1:2])%*%z$scaling) # Same numers, max min or avg should be the same


(centroide.esp1 = prom.esp1%*%z$scaling + constante)
(centroide.esp2 = prom.esp2%*%z$scaling + constante)



(corte2 = centroide.esp1  + (nrow(especie.avispa[[1]])/nrow(avispas))*(centroide.esp2 - centroide.esp1))



plot(z)

# Dos puntos en el punto de corte; no se xq no da igual que en el otro caso

new_data <- data.frame(Antena = c(1.3,1.3), Pata = c(1.846, 1.847))

predict(z, newdata = new_data)

# Otro ejemplo ------------------------------------------------------------


library(tidyverse)
library(caret)

# Load the data
data("iris")
# Split the data into training (80%) and test set (20%)
set.seed(123)
training.samples <- iris$Species %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- iris[training.samples, ]
test.data <- iris[-training.samples, ]


# Estimate preprocessing parameters
preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)


model <- lda(Species~., data = train.transformed)
model


lda.data <- cbind(train.transformed, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Species))


predictions <- model %>% predict(test.transformed)
names(predictions)

# Predicted classes
head(predictions$class, 6)
# Predicted probabilities of class memebership.
head(predictions$posterior, 6) 
# Linear discriminants
head(predictions$x, 6) 


# Model accuracy

mean(predictions$class==test.transformed$Species)



# By default, the probability cutoff used to decide group-membership is 0.5 (random guessing). 
sum(predictions$posterior[ ,1] >=.5)
sum(predictions$posterior[ ,2] >=.5)
sum(predictions$posterior[ ,3] >=.5)


# Exploratory Graph for LDA or QDA
library(klaR)
partimat(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris,method="lda") 



# QDA ---------------------------------------------------------------------

#Cuando el supuesto de homocedasticidad no puede sostenerse, una opción es utilizar el Análisis Discriminante Cuadrático de Fisher


# Ejemplo a mano ----------------------------------------------------------

library(ggplot2)
library(gridExtra)
library(corpcor)
library(Hotelling)
library(car)
library(mvnormtest)
library(biotools)
library(corrplot)
library(mclust)


data(banknote)


# Observamos las diferencias de medias por clase para ver qué tanto pueden servir para discriminar

ggplot(data = banknote, aes(x=Status, y= Length, fill = Status))+
  geom_boxplot(position = 'identity', alpha = .5)+
  scale_fill_brewer(palette = "Dark2")


ggplot(data = banknote, aes(x=Status, y= Left, fill = Status))+
  geom_boxplot(position = 'identity', alpha = .5)+
  scale_fill_brewer(palette = "Dark2")

ggplot(data = banknote, aes(x=Status, y= Length, fill = Status))+
  geom_boxplot(position = 'identity', alpha = .5)+
  scale_fill_brewer(palette = "Dark2")

ggplot(data = banknote, aes(x=Status, y= Right, fill = Status))+
  geom_boxplot(position = 'identity', alpha = .5)+
  scale_fill_brewer(palette = "Dark2")

ggplot(data = banknote, aes(x=Status, y= Bottom, fill = Status))+
  geom_boxplot(position = 'identity', alpha = .5)+
  scale_fill_brewer(palette = "Dark2")

ggplot(data = banknote, aes(x=Status, y= Top, fill = Status))+
  geom_boxplot(position = 'identity', alpha = .5)+
  scale_fill_brewer(palette = "Dark2")

ggplot(data = banknote, aes(x=Status, y= Diagonal, fill = Status))+
  geom_boxplot(position = 'identity', alpha = .5)+
  scale_fill_brewer(palette = "Dark2")

library(ggpubr)


(fit = hotelling.test(.~Status, data = data.frame(banknote)))
# Test de Hotelling
fit$stats # Rechazamos igualdad del vector de medias


# Para evaluar normalidad univariada 

qq1 <- ggplot(banknote, aes(sample=Length))+stat_qq()+stat_qq_line()+ylab("Length")
qq2 <- ggplot(banknote, aes(sample=Left))+stat_qq()+stat_qq_line()+ylab("Left")
qq3 <- ggplot(banknote, aes(sample=Right))+stat_qq()+stat_qq_line()+ylab("Right")
qq4 <- ggplot(banknote, aes(sample=Bottom))+stat_qq()+stat_qq_line()+ylab("Bottom")
qq5 <- ggplot(banknote, aes(sample=Top))+stat_qq()+stat_qq_line()+ylab("Top")
qq6 <- ggplot(banknote, aes(sample=Diagonal))+stat_qq()+stat_qq_line()+ylab("Diagonal")


ggarrange(qq1, qq2, qq3, qq4, qq5, qq6, nrow = 2, ncol = 3) # Muchos se apartan de la normalidad, pero igual hacemos test


C <- t(banknote[,2:7])
mshapiro.test(C) # Rechazamos normalidad multivariante, pero aun asi seguimos adelante

boxM(data = banknote[,2:7], grouping = banknote[,1]) # Rechazamos tambien homocedasticidad

genuino = cor(banknote[banknote$Status == 'genuine', 2:7])
apocrifo = cor(banknote[banknote$Status == 'counterfeit', 2:7])

par(mfrow = c(1, 2))
corrplot(genuino, tl.cex = .7, cl.cex = .7)
corrplot(apocrifo, tl.cex = .7, cl.cex = .7)



# Otro ejemplo ------------------------------------------------------------

library(MASS)
# Fit the model
model <- qda(Species~., data = train.transformed)
model
# Make predictions
predictions <- model %>% predict(test.transformed)
# Model accuracy
mean(predictions$class == test.transformed$Species)

library(klaR)
partimat(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,data=iris,method="qda") 


