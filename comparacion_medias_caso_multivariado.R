
# Test del vector de medias para 1 poblacion ------------------------------

# H0: vector medias poblacionales es igual a tanto
# H1: vector medias poblacionales no es igual a tanto


(V = matrix(c(4.288,1.244,1.244,0.428), nrow =  2, byrow = T))
x_prom = c(6.9,6.2)
x_planteado = c(sqrt(50),6)
n_observaciones = 40

(estadistico = t(x_prom-x_planteado) %*% solve(V) %*% (x_prom-x_planteado) * 
  ((n_observaciones - length(x_prom))/ length(x_prom)))

pf(estadistico, length(x_prom), n_observaciones-length(x_prom), lower.tail = F) # Rechazo H0
# No puedo sostener que el vector de medias es igual al valor planteado


# Test del vector de medias para 2 poblaciones ----------------------------

library(readxl)
avispas <- read_excel("avispas.xlsx")

ggplot(avispas, aes(Antena, Pata))+
  geom_point(aes(color = Especie))

especie.avispa = split(avispas, avispas$Especie)

prom.esp1 = apply(especie.avispa[[1]][,1:2], 2, mean)
prom.esp2 = apply(especie.avispa[[2]][,1:2], 2, mean)

s1 = var(especie.avispa[[1]][,1:2])
s2 = var(especie.avispa[[2]][,1:2])

n1 = nrow(especie.avispa[[1]])
n2 = nrow(especie.avispa[[2]])
p = ncol(avispas[,-1])

s = ((n1-1)*s1 + (n2-1)*s2) / (n1 + n2 - 2)

d2 = t(prom.esp1-prom.esp2)%*%solve(s)%*%(prom.esp1-prom.esp2)

t2 = d2*((n1*n2) / (n1+n2))

f = (n1+n2-p-1)/(
  (n1+n2-2)*p) * t2



alfa = 0.05

pf(f, p, n1+n2-p-1 ,lower.tail = F) < alfa # Rechazo H0 

f > qf(0.95,p, n1+n2-p-1) 




# Analisis de Perfiles ----------------------------------------------------

# H0: las diferencias de medias entre variables son todas iguales, ergo los perfiles son paralelos
# H1: alguna de las diferencias de medias entre variables es diferente al resto

iris.especie = split(iris, iris$Species)

setosa = data.frame(iris.especie[[1]])[,-c(4,5)]
versicolor = data.frame(iris.especie[[2]])[,-c(4,5)]

total = rbind(setosa, versicolor)

media.conjunta = apply(total, 2, mean)
media.setosa = apply(setosa, 2, mean)
media.versicolor = apply(versicolor, 2, mean)

var.setosa = var(setosa)
var.versicolor = var(versicolor)

n1 = nrow(setosa)
n2 = nrow(versicolor)
p = ncol(setosa)

var.amalgamada = ((n1-1)*var.setosa + (n2-1)*var.versicolor)/(n1+n2-2)

d2 = t(media.setosa-media.versicolor)%*%solve(var.amalgamada)%*%(media.setosa-media.versicolor)

t2 = d2*((n1*n2) / (n1+n2))

f = (n1+n2-p-1)/(
  (n1+n2-2)*p) * t2

f > qf(0.95,p, n1+n2-p-1) 

# Rechazo H0, los vectores medios no son iguales
# Pero quiero saber si los vectores medios son paralelos

c = rbind (c(1,-1,0),c(0,1,-1))
t = ((n1*n2)/(n1+n2)) * t(media.setosa-media.versicolor) %*% t(c) %*%
  solve(c%*%var.amalgamada %*% t(c)) %*% c %*%(media.setosa-media.versicolor)

v_critico = qf(0.95,p-1, n1+n2-p) 
t > v_critico # Rechazo vectores medios paralelos




