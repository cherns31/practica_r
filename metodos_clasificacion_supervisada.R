
# Analisis discriminante lineal --------------------------------------------------

avispas
avispas$Especie = as.factor(avispas$Especie)
especie.avispa = split(avispas, avispas$Especie)


prom.total = apply (avispas[,1:2],2,mean)
prom.esp1 = apply(especie.avispa[[1]][,1:2], 2, mean)
prom.esp2 = apply(especie.avispa[[2]][,1:2], 2, mean)



s1 = var(especie.avispa[[1]][,1:2])
s2 = var(especie.avispa[[2]][,1:2])

n1 = nrow(especie.avispa[[1]])
n2 = nrow(especie.avispa[[2]])
p = ncol(avispas[,-1])

s = ((n1-1)*s1 + (n2-1)*s2) / (n1 + n2 - 2)

w = s * (n1 + n2 - 2)

b = (prom.esp1 - prom.total) %*% t(prom.esp1 - prom.total) + 
  (prom.esp2 - prom.total) %*% t(prom.esp2 - prom.total)

mat.disc = solve(w) %*% b
round(mat.disc, 4)

avect1 = eigen(mat.disc)$vectors[,1] # Primer autovector de la matriz discriminante
coord.disc.1 = as.matrix(avispas[1:15,1:2])%*%avect1

# Uso funcion de R
z = lda(Especie ~  Antena + Pata, data = avispas, method = "mle")

centroide.1 = prom.esp1%*%z$scaling 
centroide.2 = prom.esp2%*%z$scaling 

corte = centroide.1 +  z$prior[1] * (centroide.2 - centroide.1)
coord.disc.2 = as.matrix(avispas[1:15,1:2])%*%z$scaling
cbind(avispas, coord.disc.2)

new_data = data.frame(Antena = 1.333, Pata = 1.9 )
predict(z, newdata = new_data)$class

new_data
as.matrix(new_data)%*%z$scaling


# Validacion de supuestos


library(mvtnormtest)
library(biotools)


# Normalidad multivariada

mshapiro.test(avispas[,1:2])
# No la rechazo

# Homocedasticidad

boxM(avispas[,1:2], avispas$Especie)
# No rechazo homocedasticidad



# Analisis discriminante cuadratico ---------------------------------------

# Cuando el supuesto de homocedasticidad no se cumple, se puede usar el discriminante cuadrático
# Aun cuando falte normalidad multivariada puede dar buenos resultados

data("banknote")

# Analizo si los vectores medios son diferentes

fit = hotelling.test(.~Status, data = data.frame(banknote))
fit$pval # Rechazo H0, igualdad de vectores medios y puedo seguir


# Analizo normalidad, da mal, pero igual sigo
C = t(banknote[,2:7])
mshapiro.test(C)

# Analizo homocedasticidad y rechazo tambien, aplico QDA pese a lo anterior

boxM(banknote[,2:7],  banknote[,1])

