# Test de Hotelling -> diferencia de grupos pero con variables como vectores de varios componentes
# Son x_vectores aleatorios

# LDA -> si mis medias de los grupos son distintas por Hotelling, y tengo una nueva observacion quiero ver como puedo clasificarla


#Ejemplo 1:

#Se midieron, en miligramos, 3 componentes químicos en 20 productos de dos clases de 
#sustancias A y B.
#Se quiere generar un modelo estadístico que permita identificar a qué clase pertenece un
#determinado producto.

clase<-c(rep("a",10),rep("b",10))
comp1<-c(10.7  , 9.84  , 9.55  , 10.13  , 9.84 , 10.7  , 10.99  , 11.28  , 11.28  , 10.7  , 9.26  , 9.26  , 9.26 , 9.55 , 8.97 , 8.39 , 8.39 , 9.26 , 8.39, 8.1)
comp2<-c(10.54  ,  9.74  ,  9.58  ,  10.56 ,  10.38  ,  9.44 ,  9.47  ,  10.67,  10.4,9.05  ,  8.52,  7.47 ,  8.46 ,  8.75  ,  8.19 ,  8.53  ,  8.38 ,  7.88  ,  8.4,  8.11)
comp3<-c(10.83,  8.51, 8.51, 10.06,  9.28,  9.28,  7.74,  9.28,  9.28, 7.74,  9.28,  6.96, 9.28,  9.28,  9.28,  6.96,  7.74,8.51,8.51, 7.74)

prod<-data.frame(clase,comp1,comp2,comp3)



options(repos = c(CRAN = "http://cran.rstudio.com"))
#install.packages("corpcor")
library(corpcor)
#install.packages("Hotelling")
library(Hotelling)



colMeans(list$a[,2:4])
colMeans(list$b[,2:4])

for (k in split(prod, prod$clase)){
  print(colMeans(k[,2:4]))
}


(fitProd = hotelling.test(. ~ clase, data =prod)) # Vectores media por clase difieren



library(ggplot2) 
library(gridExtra) 
p1<- ggplot(data = prod, aes(x = comp1, fill = clase)) + 
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = prod, aes(x = comp2, fill = clase)) + 
  geom_histogram(position = "identity", alpha = 0.5) 
p3 <- ggplot(data = prod, aes(x = comp3, fill = clase)) + 
  geom_histogram(position = "identity", alpha = 0.5) 
grid.arrange(p1, p2, p3) # Quiero ver si los grupos están separados en los diferentes componentes

pairs(x = prod[, c("comp1", "comp2", "comp3")], col = c("firebrick", "green3")[prod$clase], pch = 19)

library(scatterplot3d) 
scatterplot3d(prod$comp1, prod$comp2, prod$comp3, color = c("firebrick", "green3")[prod$clase], pch = 19, grid = TRUE, tick.marks = FALSE, xlab = "Comp 1", ylab = "Comp 2", zlab = "Comp 3", angle = 65) 
legend("bottomright", bty = "n", cex = .9, title = "Clase", c("a", "b"), fill = c("firebrick", "green3"))

par(mfcol = c(2, 3)) 
for (k in 2:4) { j0 <- names(prod)[k] 
x0 <- seq(min(prod[, k]), max(prod[, k]), le = 50) 
for (i in 1:2) { 
  i0 <- levels(prod$clase)[i] 
  x <- prod[prod$clase == i0, j0] 
  hist(x, proba = T, col = grey(0.8), main = paste("Clase", i0), xlab = j0) 
  lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2) 
}
}

for (k in 2:4) { 
  j0 <- names(prod)[k] 
  x0 <- seq(min(prod[, k]), max(prod[, k]), le = 50) 
  for (i in 1:2) { 
    i0 <- levels(prod$clase)[i] 
    x <- prod[prod$clase == i0, j0] 
    qqnorm(x, main = paste("Clase", i0," - ", j0), pch = 19, col = i + 1) 
    qqline(x) 
  } 
}

par(mfrow=c(1,1))

library(reshape2) 
library(knitr) 
library(dplyr) 
(datos_tidy <- melt(prod, value.name = "valor"))
kable(datos_tidy %>% group_by(clase, variable) %>% summarise(p_value_Shapiro.test = shapiro.test(valor)$p.value))
# No me sirve, salvo a nivel exploratorio, porque no me interesa que cada componente sea una normal sino todo una normal multivariada
# Porque puede ser normales univariados pero no normales multivariado

# install.packages("mvnormtest")
library(mvnormtest)
mshapiro.test(t(prod[,-1])) # Debo trasponer el DataFrame
# No rechazo que mi vector sea normal multivariado 


# install.packages("biotools")
library(biotools) 
boxM(data = prod[, 2:4], grouping = prod[, 1])
# No rechazo igualdad de homodastecidad

# Estoy en condiciones de aplicar LDA
library(MASS)
(modelo_lda <- lda(formula = clase ~ comp1 + comp2 + comp3, data = prod))


(nuevas_observaciones <- data.frame(comp1 = 10, comp2 = 10, comp3 = 7) )
sum(modelo_lda$scaling*nuevas_observaciones)

(predict(object = modelo_lda, newdata = nuevas_observaciones)) # Me da prob. a posteriori

# Calcular constante

colMeans(prod[,-1])
(groupmean<-(modelo_lda$prior%*%modelo_lda$means))
(constant<-(groupmean%*%modelo_lda$scaling))


# Falta encontrar punto de corte, porque R no lo da automáticamente

# Como el LDA escala, a la prediccion le sumo la constante por la que escaló y me deberia dar igual a los coeficientes por los valores de mi nueva observacion

(predict(object = modelo_lda, newdata = nuevas_observaciones))$x + constant == sum(modelo_lda$scaling*nuevas_observaciones)

# Evaluo con mis mismos datos de entrenamiento -> tasa de error ingenua

predicciones <- predict(object = modelo_lda, newdata = prod[, -1], method = "predictive") 
table(prod$clase, predicciones$class, dnn = c("Clase real", "Clase predicha"))

training_error <- mean(prod$clase != predicciones$class) * 100 
paste("training_error =", training_error, "%")
#paste("trainig_error=", trainig_error, "%")

with(prod, { 
  s3d <- scatterplot3d(comp1, comp2, comp3, color = c("firebrick", "green3")[prod$clase], pch = 19, grid = TRUE, tick.marks = FALSE, xlab = "Comp 1", ylab = "Comp 2", zlab = "Comp 3", angle = 65) 
  s3d.coords <- s3d$xyz.convert(comp1, comp2, comp3) # convierte coordenadas 3D en proyecciones 2D 
  text(s3d.coords$x, s3d.coords$y, labels = prod$clase, cex = .8, pos = 4) 
  legend("bottomright", bty = "n", cex = .9, title = "Clase", c("a", "b"), fill = c("firebrick", "green3")) 
})




################################################ 

# Ejemplo con datos de Iris

data("iris") 
head(iris, n = 3)

library(ggplot2) 
library(gridExtra) 
plot1 <- ggplot(data = iris, aes(x = Sepal.Length)) + 
  geom_density(aes(colour = Species)) + theme_bw() 
plot2 <- ggplot(data = iris, aes(x = Sepal.Width)) + 
  geom_density(aes(colour = Species)) + theme_bw() 
plot3 <- ggplot(data = iris, aes(x = Petal.Length)) + 
  geom_density(aes(colour = Species)) + theme_bw() 
plot4 <- ggplot(data = iris, aes(x = Petal.Width)) + 
  geom_density(aes(colour = Species)) + theme_bw() 
grid.arrange(plot1, plot2, plot3, plot4)
# Las variables del sépalo parecen discriminar mejor


pairs(x = iris[, -5], col = c("firebrick", "green3", "blue")[iris$Species], pch = 20)

par(mfcol = c(3, 4)) 
for (k in 1:4) { 
  j0 <- names(iris)[k] 
  x0 <- seq(min(iris[, k]), max(iris[, k]), le = 50) 
  for (i in 1:3) { 
    i0 <- levels(iris$Species)[i] 
    x <- iris[iris$Species == i0, j0] 
    hist(x, proba = T, col = grey(0.8), main = paste("especie", i0), xlab = j0) 
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2) 
  } 
}

for (k in 1:4) { 
  j0 <- names(iris)[k] 
  x0 <- seq(min(iris[, k]), max(iris[, k]), le = 50) 
  for (i in 1:3) { 
    i0 <- levels(iris$Species)[i] 
    x <- iris[iris$Species == i0, j0] 
    qqnorm(x, main = paste(i0, j0), pch = 19, col = i + 1) 
    qqline(x) 
  } 
}

par(mfrow=c(1,1))

library(reshape2) 
library(knitr) 
library(dplyr) 
datos_tidy <- melt(iris, value.name = "valor") 
kable(datos_tidy %>% group_by(Species, variable) %>% 
        summarise(p_value_Shapiro.test = round(shapiro.test(valor)$p.value, 5)))

#install.packages("mvnormtest")
library(mvnormtest)
mshapiro.test(t(iris[,-5]))

# Se calcula para todo, pero aca lo hago yo separado para cada clase
mshapiro.test(t(iris[iris$Species == 'setosa',-5]))
mshapiro.test(t(iris[iris$Species == 'virginica',-5]))
mshapiro.test(t(iris[iris$Species == 'versicolor',-5]))



boxM(data = iris[, 1:4], grouping = iris[, 5])
# Rechaza homogeneidad de varianzas entre grupos
# Se recomienda usar el cuadrático en vez del lineal
# Pero lo aplica igual y vamos a ver que al final resulta bueno

modelo_lda_iris <- lda(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data = iris) 
modelo_lda_iris # Tengo más de 1 discriminante

prediccionesIris <- predict(object = modelo_lda_iris, newdata = iris[, -5]) 
table(iris$Species, prediccionesIris$class, dnn = c("Clase real", "Clase predicha"))

training_error_iris <- mean(iris$Species != prediccionesIris$class) * 100
paste("training_error = ", training_error_iris, "%", sep = "")

# install.packages("klaR")
library(klaR) 
partimat(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data = iris, method = "lda", prec = 200,image.colors = c("darkgoldenrod1", "snow2", "skyblue2"), col.mean = "firebrick")



cutoff <- (t(colSums(prod$means)) %*% m1$scaling) * 0.5
