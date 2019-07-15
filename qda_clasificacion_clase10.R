library(ggplot2)
##QDA


# QDA ---------------------------------------------------------------------


# Ejemplo 1


# Generamos un conjunto de Datos simulados.

set.seed(1234)
grupoA_x <- seq(from = -3, to = 4, length.out = 100)-+ rnorm(100, sd = 1)
grupoA_y <- 6 + 0.15 * grupoA_x - 0.3 * grupoA_x^2 + rnorm(100, sd = 1)
grupoA <- data.frame(variable_z = grupoA_x, variable_w = grupoA_y, grupo = "A")

grupoB_x <- rnorm(n = 100, mean = 0.5, sd = 0.8)
grupoB_y <- rnorm(n = 100, mean = 2, sd = 0.9)
grupoB <- data.frame(variable_z = grupoB_x, variable_w = grupoB_y, grupo = "B")

datos <- rbind(grupoA, grupoB)

ggplot(datos, aes(x = variable_z, y = variable_w, col = grupo ))+
  geom_point()+theme_minimal()


### se aprecia en este gráfico que los datos no son linealmente separables
## es probable que QDA ofrezca una mejor alternativa que LDA

#Superponemos los histogramas de las variables z y w por grupo
library(ggplot2)
library(ggpubr)
p1 <- ggplot(data = datos, aes(x = variable_z, fill = grupo)) +
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = datos, aes(x = variable_w, fill = grupo)) +
  geom_histogram(position = "identity", alpha = 0.5)
ggarrange(p1, p2, nrow = 2, common.legend = TRUE, legend = "bottom")

# se ve que si bien se superponen tienen distribuciones bien distintas
# sobre todo para la variable w

##Análisis diagnóstico de los supuestos del modelo

# Representación mediante histograma de cada variable para cada grupo 
par(mfcol = c(2, 2))
for (k in 1:2) {
  j0 <- names(datos)[k]
  x0 <- seq(min(datos[, k]), max(datos[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(datos$grupo)[i]
    x <- datos[datos$grupo == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("grupo", i0),
         xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}

##Se aprecia que la normalidad ajusta en forma razonable a los histogramas

#QQplot normal por variable para cada grupo 
for (k in 1:2) {
  j0 <- names(datos)[k]
  x0 <- seq(min(datos[, k]), max(datos[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(datos$grupo)[i]
    x <- datos[datos$grupo == i0, j0]
    qqnorm(x, main = paste(i0, j0), pch = 19, col = i + 1)
    qqline(x)
  }
}

par(mfcol = c(1, 1))

#Contraste de normalidad Shapiro-Wilk para cada variable en cada grupo
library(reshape2)
(datos_tidy <- melt(datos, value.name = "valor"))
library(dplyr)
datos_tidy %>%
  group_by(grupo, variable) %>% 
  summarise(p_value_Shapiro.test = round(shapiro.test(valor)$p.value,5))

##la variable W no satisface normalidad univariada en el grupo A

library(mvnormtest)
mshapiro.test(t(datos[,-3])) # Lo que más me interesa es esto, a nivel multivariado

library(biotools)
boxM(data = datos[, 1:2], grouping = datos[, 3])


##No se verifica la normalidad multivariada

## Si bien QDA tiene cierta robustez frente a la falta de normalidad multivariante,
## es importante tenerlo en cuenta en la conclusión del análisis. 

##Modelo QDA

library(MASS)
modelo_qda <- qda(grupo ~ variable_z + variable_w, data = datos,CV=T) # CV es para cross-validation
modelo_qda
table(modelo_qda$class,datos$grupo)
mean(modelo_qda$class==datos$grupo) # tasa de buena clasificación ingenua del discr cuadrático


### con muestra de entrenamiento y validación
set.seed(102030)# fijo la semilla para que todos obtengamos los mismos resultados
train=sample(x = nrow(datos), size = nrow(datos)*.75)
df_train=data.frame(datos[train,])
df_test=data.frame(datos[-train,])
qda.train=qda(grupo~ variable_z + variable_w, data = df_train) # Aca sin CrossValidation

qda_predTest = predict(qda.train, df_test)
qda_class = predict(qda.train, df_test)$class
table(qda_class, df_test$grupo)
mean(qda_class== df_test$grupo) # tasa de buena clasificación

###también podemos aplicar qda robusto
##QdaCov Robust Quadratic Discriminant Analysis
##  LDA robusto con el método por default method (MCD )

library(rrcov)

rob.mcd=QdaCov( datos$grupo~., data = datos)
predict(rob.mcd)@classification
table(predict(rob.mcd)@classification,datos$grupo)
mean(predict(rob.mcd)@classification==datos$grupo) # tasa de buena clasif ingenua del discr robusto



###
###probemos otras alternativas robustas
rob.sde=QdaCov(datos$grupo~., data = datos, method="sde")
predict(rob.mcd)@classification
table(predict(rob.sde)@classification,datos$grupo)
mean(predict(rob.sde)@classification==datos$grupo) # tasa de buena clasif ingenua del discr robusto


rob.M=QdaCov(datos$grupo~., data = datos, method="M")
predict(rob.mcd)@classification
table(predict(rob.M)@classification,datos$grupo)
mean(predict(rob.M)@classification==datos$grupo) # tasa de buena clasif ingenua del discr robusto


roblog=QdaCov(datos$grupo~., data = datos, method=CovControlOgk())
predict(rob.mcd)@classification
table(predict(roblog)@classification,datos$grupo)
mean(predict(roblog)@classification==datos$grupo) # tasa de buena clasificaci?n ingenua del discr robusto

###Qué se observa??


# Ejemplo 2 


# Se	 observaron	 dos	 grupos de salmones de Alaska y Canadá y se quiere	
# determinar el origen de los mismos en función de los datos obtenidos
library(ggplot2)
library(ggpubr)
library(readxl)

salmon <- read_excel("Segunda Parte/salmon.xlsx")
View(salmon)
library(ggplot2)
Origen=factor(salmon$origen)
(p1=ggplot(aes(x=aguadulce,y=mar,fill=Origen,color=Origen),data=salmon)+
  geom_point(aes(x=aguadulce,y=mar)))


##observamos las variables candidatas a discriminar

binw <- 10

pdul <- ggplot(salmon, aes(aguadulce, colour = Origen)) +
  
  geom_freqpoly(binwidth = binw)


pdul2 <- ggplot(salmon, aes(aguadulce, fill = Origen)) +
  geom_histogram(binwidth = binw)


pmar <-ggplot(salmon, aes(mar, colour = Origen)) +
  geom_freqpoly(binwidth = binw)

pmar2 <- ggplot(salmon, aes(mar, fill = Origen)) +
  geom_histogram(binwidth = binw)


ggarrange(pdul, pdul2, pmar, pmar2, nrow = 2, ncol= 2, common.legend = TRUE, legend = "bottom")



###veamos si se satisfacen los supuestos de homoscedasticidad 
# y normalidad multivariada


library(mvnormtest)
mshapiro.test(t(salmon[,-1]))

##no rechazamos la normalidad multivariada


library(biotools) 
boxM(data =salmon[,-1], grouping = Origen)
##no se satisface la homoscedasticidad!! Intentamos discriminante cuadrático

####QDA
library(MASS)

##clasificacion ingenua
salmon_lda0 <- lda(Origen ~ aguadulce+mar, data = salmon)
salmon_lda0

salmon_qda0 <- qda(Origen ~ aguadulce+mar, data = salmon)
salmon_qda0

prediccionesLda <- predict(object = salmon_lda0, newdata = salmon[,-1]) 
table(salmon$origen, prediccionesLda$class, dnn = c("Origen real", "Origen predicho"))

error_ingenuo_lda<- mean(salmon$origen != prediccionesLda$class) * 100
paste(error_ingenuo_lda,"%", sep = "")#7%

prediccionesQda <- predict(object = salmon_qda0, newdata = salmon[,-1]) 
table(salmon$origen, prediccionesQda$class, dnn = c("Origen real", "Origen predicho"))

error_ingenuo_qda<- mean(salmon$origen != prediccionesQda$class) * 100
paste(error_ingenuo_qda,"%", sep = "")#7%


## clasificación cv-loo

salmon_lda <- lda(Origen ~ aguadulce+mar, data = salmon,CV=TRUE)
#salmon_lda
salmon_qda <- qda(Origen ~ aguadulce+mar, data = salmon,CV=TRUE)
#salmon_qda

table(salmon_lda$class,Origen)
TasaBuenaclasif_lda<-mean(salmon_lda$class==Origen)#0.93 # tasa de buena clasificación del discr lineal
(error_cvloo_lda<-mean(salmon_lda$class!=Origen))#0.07 # tasa de error

table(salmon_qda$class,Origen)
TasaBuenaclasif_qda<-mean(salmon_qda$class==Origen)#0.92 # tasa de buena clasificación del discr cuadrático
(error_cvloo_qda<-mean(salmon_qda$class!=Origen))#0.08 # tasa de error


## clasificación con entrenamiento y validación

set.seed(2019)
entrenamiento<-sample(nrow(salmon),nrow(salmon)*.7)
validación<-c(1:100)[-entrenamiento]
salmon_lda1 <- lda(origen ~ aguadulce+mar, data = salmon[entrenamiento,])
salmon_lda1
salmon_qda1 <- qda(origen ~ aguadulce+mar, data = salmon[entrenamiento,])
salmon_qda1
prediccionesLda_2 <- predict(object = salmon_lda1, newdata = salmon[validación,-1]) 
table(salmon$origen[validación], prediccionesLda_2$class, dnn = c("Origen real", "Origen predicho"))

error_test_lda<- mean(salmon$origen[validación] != prediccionesLda_2$class) * 100
error_test_lda#6.666667%

prediccionesQda_2<- predict(object = salmon_qda1, newdata = salmon[validación,-1]) 
table(salmon$origen[validación], prediccionesQda_2$class, dnn = c("Origen real", "Origen predicho"))

error_test_qda<- mean(salmon$origen[validación] != prediccionesQda_2$class) * 100
error_test_qda#6.666667%








### es razonable??
library(klaR) # Classification and visualization package
Origen=factor(salmon$origen)


partimat(Origen ~ mar+aguadulce, data = salmon, method = "qda", 
         imageplot = TRUE,col.mean=1,image.colors = c("lightgrey","red")) #QDA

partimat(Origen ~ mar+aguadulce, data = salmon, method = "lda", col.mean=1,
         imageplot = TRUE,image.colors = c("lightgrey","red")) #LDA





# Regresion Logistica -----------------------------------------------------


Alaska0_Canada1<-ifelse(Origen[entrenamiento]=="Alaska",0,1)
Alaska0_Canada1_test<-ifelse(Origen[validación]=="Alaska",0,1)
modelo_reg_logis<-glm(as.factor(Alaska0_Canada1) ~ mar+aguadulce, data = salmon[entrenamiento,],family="binomial")

#modelo_reg_logis<-glm(Origen[entrenamiento] ~ mar+aguadulce, data = salmon[entrenamiento,],family="binomial")
prediccionesRegLog<-ifelse(modelo_reg_logis$fitted.values>0.5,1,0) 
table(salmon$origen[entrenamiento], prediccionesRegLog, dnn = c("Origen real", "Origen predicho"))
error_RegLog_ingenuo<- mean(Alaska0_Canada1 != prediccionesRegLog) * 100
error_RegLog_ingenuo#7.142857%


pred_test_RegLog<-predict(object=modelo_reg_logis,newdata=salmon[validación,-1],type="response")
pred_test_RegLog_0_1<-ifelse(pred_test_RegLog>0.5,1,0)
table(salmon$origen[validación], pred_test_RegLog_0_1, dnn = c("Origen real", "Origen predicho"))


error_RegLog<- mean(Alaska0_Canada1_test!= pred_test_RegLog_0_1) * 100
error_RegLog#6.666667%


# SVM ---------------------------------------------------------------------

library(ggplot2)
library(e1071)
modelo_svm=svm(as.factor(Alaska0_Canada1)~mar+aguadulce,data=salmon[entrenamiento,],method="C-classification",kernel="radial",cost=10,gamma=.1)
pred_svm=predict(modelo_svm, salmon[validación,-1])
table(salmon$origen[validación], pred_svm, dnn = c("Origen real", "Origen predicho"))
error_svm<- mean(Alaska0_Canada1_test!= pred_svm) * 100
error_svm#10%

#plot(modelo_svm,salmon[entrenamiento,],symbolPalette=topo.colors(3),dataSymbol="o",color.palette=cm.colors)
plot(modelo_svm,salmon[entrenamiento,])

