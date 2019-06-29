#### Distribución Normal Univariada ####

d1 <- rnorm(100000, mean = 10, sd = sqrt(20))
d2 <- rnorm(100000, mean = 10, sd = sqrt(10))
d3 <- rnorm(100000, mean = 10, sd = sqrt(5))

h1 <- dnorm(d1)
h2 <- dnorm(d2)
h3 <- dnorm(d3)

plot(d1, dnorm(d1))
plot(d1, dnorm(d2))

library(ggfortify)
library(ggplot2)


p <- ggfortify::ggdistribution(dnorm, seq(-5, 5, 0.1), mean = 0, sd = sqrt(1), colour = "blue")
p <- ggfortify::ggdistribution(dnorm, seq(-5, 5, 0.1), mean = 0, sd = sqrt(2), colour = "red", p = p)
p <- ggfortify::ggdistribution(dnorm, seq(-5, 5, 0.1), mean = 0, sd = sqrt(3),colour = 'orange', p = p)
ggfortify::ggdistribution(dnorm, seq(-5, 5, 0.1), mean = 0, sd = sqrt(4),colour = 'black', p = p)


1/(sqrt(2*pi)*sqrt(1)) # Maxima probabilidad
1/(sqrt(2*pi)*sqrt(2))
1/(sqrt(2*pi)*sqrt(3))
1/(sqrt(2*pi)*sqrt(4))



p <- ggfortify::ggdistribution(pnorm, seq(-5, 5, 0.1), mean = 0, sd = sqrt(1), colour = "blue")
p <- ggfortify::ggdistribution(pnorm, seq(-5, 5, 0.1), mean = 0, sd = sqrt(2), colour = "red", p = p)
p <- ggfortify::ggdistribution(pnorm, seq(-5, 5, 0.1), mean = 0, sd = sqrt(3),colour = 'orange', p = p)
ggfortify::ggdistribution(pnorm, seq(-5, 5, 0.1), mean = 0, sd = sqrt(4),colour = 'black', p = p)

# Todas alcanzan el 50% acumulado en la media
pnorm(0, mean = 0, sd = sqrt(1))
pnorm(0, mean = 0, sd = sqrt(2))
pnorm(0, mean = 0, sd = sqrt(3))
pnorm(0, mean = 0, sd = sqrt(4))
pnorm(50, mean = 50, sd = sqrt(1))

# Todas alcanzan el 68% acumulado +- 1 Desvio
pnorm(sqrt(1), mean = 10, sd = sqrt(1)) - pnorm(-sqrt(1), mean = 10, sd = sqrt(1))
pnorm(sqrt(2), mean = 0, sd = sqrt(2)) - pnorm(-sqrt(2), mean = 0, sd = sqrt(2))
pnorm(sqrt(3), mean = 0, sd = sqrt(3)) - pnorm(-sqrt(3), mean = 0, sd = sqrt(3))
pnorm(sqrt(4), mean = 0, sd = sqrt(4)) - pnorm(-sqrt(4), mean = 0, sd = sqrt(4))
pnorm(50+sqrt(4), mean = 50, sd = sqrt(4)) - pnorm(50-sqrt(4), mean = 50, sd = sqrt(4))

# Todas alcanzan el 95% acumulado +- 2 Desvio
pnorm(2*sqrt(1), mean = 0, sd = sqrt(1)) - pnorm(2*-sqrt(1), mean = 0, sd = sqrt(1))
pnorm(2*sqrt(2), mean = 0, sd = sqrt(2)) - pnorm(2*-sqrt(2), mean = 0, sd = sqrt(2))
pnorm(2*sqrt(3), mean = 0, sd = sqrt(3)) - pnorm(2*-sqrt(3), mean = 0, sd = sqrt(3))
pnorm(2*sqrt(4), mean = 0, sd = sqrt(4)) - pnorm(2*-sqrt(4), mean = 0, sd = sqrt(4))
pnorm(50+2*sqrt(4), mean = 50, sd = sqrt(4)) - pnorm(50-2*sqrt(4), mean = 50, sd = sqrt(4))

# O sea que la prob acumulada depende de cuantos desvios tome, y no del valor de los mismos


#### Distribución Normal Multivariada ####

library(MASS)

N <- 10000 # Number of random samples
set.seed(123)
# Target parameters for univariate normal distributions
rho <- 0
mu1 <- 1; s1 <- 2
mu2 <- 2; s2 <- 8

# Parameters for bivariate normal distribution
mu <- c(mu1,mu2) # Mean
(sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),
                2)) # Covariance matrix


bvn <- mvrnorm(N, mu = mu, Sigma = sigma ) # from MASS package
colnames(bvn) <- c("X1","X2")

plot(bvn)

# La variable se distribuye con media (1, 1) y varianza matriz sigma

ct1 = matrix(rep(c(1, 0), nrow(bvn)), ncol = 2, byrow = T)
c1 = (ct1 * bvn)[,1]
mean(c1)
var(c1)
sd(c1)

hist(c1)
qqnorm(c1)


ct2 = matrix(rep(c(0, 1), nrow(bvn)), ncol = 2, byrow = T)
c2 = (ct2 * bvn)[,2]
mean(c2)
var(c2)
sd(c2)


hist(c2)

p <- ggfortify::ggdistribution(dnorm, seq(-30, 30, 0.1), mean = mean(c1), sd = sd(c1),colour = 'orange')
ggfortify::ggdistribution(dnorm, seq(-30, 30, 0.1), mean = mean(c2), sd = sd(c2),colour = 'blue', p = p)


# Las nuevas variables, combinacion lineal usando los vectores canonicos, de la normal bivariada tiene media:

ct1[1,]%*%t(cbind(mu1, mu2))
ct2[1,]%*%t(cbind(mu1, mu2))

# Ahora probemos con otras combinaciones lineales, de a pares
# Primero con uno

ct3 = matrix(rep(c(2, 3), nrow(bvn)), ncol = 2, byrow = T)
c3 = (ct3 * bvn)

hist(c3, breaks = 100)

ct3[1,]%*%matrix(colMeans(bvn), nrow = 2) # Media

t(ct3[1,])%*%cov(bvn)%*%ct3[1,] # Varianza
sqrt(t(ct3[1,])%*%cov(bvn)%*%ct3[1,]) # Desvio

# Luego con otro

ct4 = matrix(rep(c(-3, 1), nrow(bvn)), ncol = 2, byrow = T)
c4 = (ct4 * bvn)

ct4[1,]%*%matrix(colMeans(bvn), nrow = 2) # Media

t(ct4[1,])%*%cov(bvn)%*%ct4[1,] # Varianza
sqrt(t(ct4[1,])%*%cov(bvn)%*%ct4[1,]) # Desvio

hist(c4, breaks = 100)

pnorm(ct4[1,]%*%matrix(colMeans(bvn), nrow = 2), mean = ct4[1,]%*%matrix(colMeans(bvn), nrow = 2) , sd = sqrt(t(ct4[1,])%*%cov(bvn)%*%ct4[1,])) # Media

# Y en ambos casos obtenemos una variable normal univariada.
# Ahora si los tomo juntos

ct34 = rbind(ct3[1,], ct4[1,])
c34 = (bvn %*% ct34)

ct34%*%matrix(colMeans(bvn), nrow = 2) # El vector de medias son las medias de las transformaciones anteriores por separado

t(ct34)%*%cov(bvn)%*%ct34 # La matriz de covarianza resultante


plot(c34[,1], c34[,2])
plot(bvn[,1], bvn[,2])

cor(bvn) # Antes tenia rho .3
cor(c34) # Ahora despues de la comb lineal tiene -.76

sum(diag(cov(c34)))
sum(diag(eigen(cov(c34))$values)) # Varianza total de la comb lineal, formando una nueva bvn



# Y los con los vectores canonicos?

# No cambio nada, lo cual es logico

(ct12 = rbind(ct1[1,], ct2[1,]))
c12 = (bvn %*% ct12) # Es el mismo vector

ct12%*%matrix(colMeans(bvn), nrow = 2) # El vector de medias son las medias de las transformaciones anteriores por separado

t(ct12)%*%cov(bvn)%*%ct12 # La matriz de covarianza resultante
cov(bvn)

plot(c12[,1], c12[,2])
plot(bvn[,1], bvn[,2])

cor(bvn) 
cor(c12) 

sum(diag(cov(c12)))
sum(diag(eigen(cov(c12))$values)) # Varianza total de la comb lineal, formando una nueva bvn



# Y los con un vector de puros 1?

(ct56 = matrix(rbind(c(1,1), c(1,1)), byrow = T, nrow = 2))
c56 = (bvn %*% ct56) # Es el mismo vector

ct56%*%matrix(colMeans(bvn), nrow = 2) # El vector de medias son las medias de las transformaciones anteriores por separado

t(ct56)%*%cov(bvn)%*%ct56 # La matriz de covarianza resultante
cov(bvn)

plot(c56[,1], c56[,2])
plot(bvn[,1], bvn[,2])

cor(bvn) 
cor(c56) # El rho se torna 1, es decir correlacion perfecta

sum(diag(cov(c56)))
sum(diag(eigen(cov(c56))$values)) # Varianza total de la comb lineal, formando una nueva bvn


# Veamos los graficos de contorno

bivn.kde <- kde2d(bvn[,1], bvn[,2], n = 50)   # from MASS package
image(bivn.kde)       # from base graphics package
contour(bivn.kde, add = TRUE)     # from base graphics package
