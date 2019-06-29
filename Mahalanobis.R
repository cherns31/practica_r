library(ggplot2)
library(MASS)

set.seed(4775)
data <- as.data.frame( mvrnorm(1000, mu=c(10,20), Sigma=rbind(c(10, 5),
                                  c(5, 3) )))

cov(data)
cor(data)

(eig <- eigen(cov(data)))

ggplot(data, mapping = aes(x = V1, y = V2))+
  geom_point()
  ##stat_ellipse(type = "norm", level = .99)


center_colmeans <- function(x) {
  xcenter = colMeans(x)
  x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}



data_centered <- center_colmeans(data)


ggplot(data_centered, mapping = aes(x = V1, y = V2))+
  geom_point()

data_centered_trans <- 
as.data.frame(as.matrix(data_centered)%*%solve(cov(data)))



# ggplot(data_centered_trans, mapping = aes(x = V1, y = V2))+
#   geom_point()

md <- diag(as.matrix(data_centered_trans)%*%as.matrix(t(data_centered)))
sum(md) == sum(mahalanobis(data, colMeans(data), cov(data))) # Calculada OK

nc = .95

cv<-qchisq(nc,df=2) #critical value
data$color<-as.factor(ifelse(md>cv,3,2)) #just sets the color
data_centered$color <- as.factor(ifelse(md>cv,3,2))

ggplot(data, mapping = aes(x = V1, y = V2))+
  geom_point(mapping = aes(color = color), size = 3)+
  stat_ellipse(type = "norm", level = nc)



library(FactoMineR)
pca.data <- princomp(data[1:2],cor = T)
summary(pca.data)


pca.data$loadings
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



