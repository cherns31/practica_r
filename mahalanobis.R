library(ggplot2)
library(MASS)

set.seed(4775)

# Armo la data

data <- as.data.frame( mvrnorm(1000, mu=c(10,20), Sigma=rbind(c(10, 5),
                                  c(5, 3) )))

# La covarianza debe dar como la armada

cov(data)
cor(data)

# Calculo autovalores y autovectores

(eig <- eigen(cov(data)))

# Grafico la distribución

(a <- ggplot(data, mapping = aes(x = V1, y = V2))+
  geom_point())
  ##stat_ellipse(type = "norm", level = .99)
 nbhj
# Función para centrar datos por la media

center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}


# Centro los datos aplicando la función

data_centered <- center_colmeans(data)

# Grafico la data original y la centrada

(b <- ggplot(data_centered, mapping = aes(x = V1, y = V2))+
  geom_point(color = "blue"))

ggplot(data_centered, mapping = aes(x = V1, y = V2))+
  geom_point(color = "blue")+
  geom_point(data, mapping = aes(x = V1, y = V2))

# Aplico la inversa de la matriz de covarianza para quitar la rotación

data_centered_trans <- 
as.data.frame(as.matrix(data_centered)%*%solve(cov(data)))

# Y la grafico junto con las otras

ggplot(data_centered_trans, mapping = aes(x = V1, y = V2))+
  geom_point(color = "darkgreen")+
  geom_point(data, mapping = aes(x = V1, y = V2))+
  geom_point(data_centered, mapping = aes(x = V1, y = V2), color = "blue")

# Calculo mahalanobis como la data centrada rotada por la traspuesta de la data centrada

md <- diag(as.matrix(data_centered_trans)%*%as.matrix(t(data_centered)))

# Verifico que da igual que la función

all(md == mahalanobis(data, colMeans(data), cov(data))) # Calculada OK

# Calculo valores criticos y marco los que rechazo

nc = .95

cv<-qchisq(nc,df=2) #critical value
data$color<-as.factor(ifelse(md>cv,3,2)) #just sets the color
data_centered$color <- as.factor(ifelse(md>cv,3,2))

# Dibujo la data original con ellos

ggplot(data, mapping = aes(x = V1, y = V2))+
  geom_point(mapping = aes(color = color), size = 3)+
  stat_ellipse(type = "norm", level = nc)

# Comparo contra PCA (usando Correlacion)

library(FactoMineR)
pca.data <- princomp(data[1:2],cor = F)
summary(pca.data)

# Comparo matriz de correlacion vs la de autovalores
diag(cov(data[,1:2]))
pca.data$sdev**2

# Veo los autovectores
pca.data$loadings

names(pca.data$sdev)
av1 = 12
av2 = 1.5

b+
  geom_segment(data = as.data.frame(t(pca.data$loadings[,1])) ,
               aes(xend = V1*av1, yend = V2*av1
, colour = "curve", x = V1*-av1, y = V2*-av1 ), size = 3)+
  geom_segment(data = as.data.frame(t(pca.data$loadings[,2])) ,
               aes(xend = V1*av2, yend = V2*av2, colour = "curve", x = V1 * -av2, y = V2 *-av2), size = 3)
  

(eig <- eigen(cor(data[1:2])))
sqrt(eig$vectors[1,1]**2+eig$vectors[2,1]**2)


t(t(pca.data$scores %*% t(pca.data$loadings)) * pca.data$scale  + pca.data$center)[1,1:2]
data[1,]

data$pca_x = pca.data$scores[,1]
data$pca_y = pca.data$scores[,2]

ggplot(data, mapping = aes(x = pca_x , y = pca_y))+
  geom_point(mapping = aes(color = color), size = 3)+
  stat_ellipse(type = "norm", level = nc)


round(sum(md)/(dim(data)[1]*2))
diag(sqrt(round(cov(pca.data$scores),4)))
round(pca.data$sdev/diag(sqrt(round(cov(pca.data$scores),4))),3)


data$pca_x_std <- data$pca_x/
  sd(data$pca_x)

data$pca_y_std <- data$pca_y/
  sd(data$pca_y)

data[1,]$pca_x_std**2+data[1,]$pca_y_std**2
md[1]

top_md = data[order(md, decreasing = T)[1],]
second_top_md = data[order(md, decreasing = T)[2],]
third_top_md = data[order(md, decreasing = T)[3],]

ggplot(data, mapping = aes(x = pca_x_std , y = pca_y_std))+
  geom_point(mapping = aes(color = color), size = 3)+
  stat_ellipse(type = "norm", level = nc)+
  geom_point(data = top_md, aes(x= pca_x_std, y = pca_y_std), color = "black", size = 5)+
  geom_point(data = second_top_md, aes(x= pca_x_std, y = pca_y_std), color = "orange", size = 5)+
  geom_point(data = third_top_md, aes(x= pca_x_std, y = pca_y_std), color = "royalblue", size = 5)




ggplot(data, mapping = aes(x = V1, y = V2))+
  geom_point(mapping = aes(color = color), size = 3)+
  stat_ellipse(type = "norm", level = nc)+
  geom_point(data = top_md, aes(x= V1, y = V2), color = "black", size = 5)+
  geom_point(data = second_top_md, aes(x= V1, y = V2), color = "orange", size = 5)+
  geom_point(data = third_top_md, aes(x= V1, y = V2), color = "royalblue", size = 5)


top_md
second_top_md



