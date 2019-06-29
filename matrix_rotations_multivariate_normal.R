library(MASS)
library(ggplot2)

set.seed(4779)
dist <- as.data.frame(mvrnorm(3000, mu=c(0,0), Sigma=rbind(c(1, 0),
                                  c(0, 1) )))
cov(dist)
means <- as.data.frame(t(as.matrix(colMeans(dist))))
max <- dist[which(dist[,1]==max(dist[,1])),]

(dist_chart <-
  ggplot(dist, mapping  = aes(x = V1, y = V2))+
  geom_point()+
  scale_x_continuous(limits = c(-8, 8))+
  scale_y_continuous(limits = c(-8, 8))+
  theme( 
    axis.line = element_line(color = "black", 
                             size = .11, linetype = "solid"))+
  geom_point(means, mapping = aes(x= V1, y = V2), color = "red", shape = 15, size = 3)+
  geom_point(max, mapping = aes(x= V1, y = V2), color = "blue", shape = 15, size = 3))

# Agrandar la varianza

ag_var <- as.matrix(rbind(c(2, 0),c(0, 1)))

dim(dist) 
dim(ag_var)

dist2 <- as.data.frame(as.matrix(dist)%*%ag_var)
max2 <- as.data.frame(as.matrix(max)%*%ag_var) 

ggplot(dist2, mapping  = aes(x = V1, y = V2))+
  geom_point()+
  scale_x_continuous(limits = c(-8, 8))+
  scale_y_continuous(limits = c(-8, 8))+
  theme( 
    axis.line = element_line(color = "black", 
                             size = .11, linetype = "solid"))+
  geom_point(means, mapping = aes(x= V1, y = V2), color = "red", shape = 15, size = 3)+
  geom_point(max2, mapping = aes(x= V1, y = V2), color = "blue", shape = 15, size = 3)+
  geom_point(subset(dist2, rownames(dist2)=="1265" ), mapping  = aes(x = V1, y = V2), color = "yellow", size = 3 )+
  geom_point(subset(dist2, rownames(dist2)=="1000" ), mapping  = aes(x = V1, y = V2), color = "green", size = 3 )




cov(dist2)


# Rotar
angle = 45
rot <- as.matrix(rbind(c(cos(angle), sin(angle)),c(-sin(angle), cos(angle))))


dim(dist2) 
dim(rot)

dist3 <- as.data.frame(as.matrix(dist2)%*%rot)
max3 <- as.data.frame(as.matrix(max2)%*%rot)

rot <- as.data.frame(rot)



  ggplot(dist3, mapping  = aes(x = V1, y = V2))+
  geom_point()+
  scale_x_continuous(limits = c(-8, 8))+
  scale_y_continuous(limits = c(-8, 8))+
  theme( 
    axis.line = element_line(color = "black", 
                             size = .11, linetype = "solid"))+
  geom_point(means, mapping = aes(x= V1, y = V2), color = "red", shape = 15, size = 3)+
  geom_point(max3, mapping = aes(x= V1, y = V2), color = "blue", shape = 15, size = 3)+
  geom_segment(aes(x = V1[1]*-5, y = V2[1]*-5, xend = V1[1]*5, yend = V2[1]*5), size = 1.5, data = rot, color = "red", arrow = arrow(length = unit(0.03, "npc")))+
  geom_segment(aes(x = V1[2]*-3, y = V2[2]*-3, xend = V1[2]*2.5, yend = V2[2]*2.5), size = 1.5, data = rot, color = "blue", arrow = arrow(length = unit(0.03, "npc")))
  
    
eigen(cov(dist3))


# Cambiar media

(shift <- matrix(c(2,1), nrow = nrow(dist3), ncol = 2, byrow = T))
dist4 <- dist3 + shift
max4 <- max3 + shift
means4 <- as.data.frame(t(as.matrix(colMeans(dist4))))

ggplot(dist4, mapping  = aes(x = V1, y = V2))+
  geom_point()+
  scale_x_continuous(limits = c(-8, 8))+
  scale_y_continuous(limits = c(-8, 8))+
  theme( 
    axis.line = element_line(color = "black", 
                             size = .11, linetype = "solid"))+
  geom_point(means4, mapping = aes(x= V1, y = V2), color = "red", shape = 15, size = 3)+
  geom_point(max4, mapping = aes(x= V1, y = V2), color = "blue", shape = 15, size = 3)

round(cov(dist4),2)
round(cov(dist3),2)
round(cov(dist2),2)
round(cov(dist),2)

round(cor(dist4),2)
round(cor(dist3),2) # Tras la rotacion la correlacion es .57 o 1 Radian
round(cor(dist2),2)
round(cor(dist),2)


eig <- eigen(cov(dist3))

# La transformación operada es una de rotacion, definida por los autovectores y otra de escalado, definida por la raiz de los autovalores

R = t(eig$vectors)
V = diag(eig$values)
S = sqrt(V)

trans = S%*%R

dist_bis <- as.data.frame(as.matrix(dist)%*%trans)

dist_bis_chart <-
  ggplot(dist_bis, mapping  = aes(x = V1, y = V2))+
  geom_point()+
  scale_x_continuous(limits = c(-8, 8))+
  scale_y_continuous(limits = c(-8, 8))+
  theme( 
    axis.line = element_line(color = "black", 
                             size = .11, linetype = "solid"))


# Las puedo aplicar por separado tambien y debe dar igual

a <- as.matrix(dist)%*%S
b <- as.data.frame(a%*%R)


b_chart <- ggplot(dist_bis, mapping  = aes(x = V1, y = V2))+
  geom_point()+
  scale_x_continuous(limits = c(-8, 8))+
  scale_y_continuous(limits = c(-8, 8))+
  theme( 
    axis.line = element_line(color = "black", 
                             size = .11, linetype = "solid"))




# Y La matriz de covarianza es esta transformación por su traspuesta

solve(R)%*%S%*%S%*%R # RSS(R**-1)
solve(R)%*%V%*%R # RV(R**-1)

cov(dist3) == cov(dist4) # La translacion por cambio de media no altera la matriz de covarianza

det(V)
det(V) == prod(diag(V)) # Determinante de los autovalores es igual a su traza y es el crecimiento del volumen al escalar

det(R) # Determinante de los autovectores es 1, pues solo rota, no escala


cov(dist)
cov(dist2)
eigen(cov(dist3))$values # Al rotar la varianza queda capturada en el autovalor, pues es la misma varianza de antes pero sobre el eje rotado por el autovector

# Veo qué da PCA
pca <- prcomp(dist4, center = T)

pca[[2]] # Los autovectores coinciden con la rotacion (el primer eje invertido de signo)
rot
R

pca[[1]] # El desvio estandar de los ejes es la matriz de escalado
diag(S) 

summary(pca) # La proporcion de la varianza es 1 a 4, el doble de lo que se escaló
round(V/sum(diag(V)),4)


# Verifico que los autovectores son ortogonales entre sí
# https://www.wikihow.com/Find-the-Angle-Between-Two-Vectors

rad2deg <- function(rad) {(rad * 180) / (pi)}

rad2deg(acos(sum(rot[1,]*rot[2,])/(sum(rot[1,]**2)*sum(rot[2,]**2))))

