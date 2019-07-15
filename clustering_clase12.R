# Clustering --------------------------------------------------------------

data(USArrests) 
datos <- scale(USArrests)  
head(datos)

# Métodos Jerárquicos:

# Matriz de distancias euclídeas 
mat_dist <- dist(x = datos, method = "euclidean") # Distancia individuo con individuo 

# Podriamos usar otras como Manhattan

# Dendrogramas  
hc_complete <- hclust(d = mat_dist, method = "complete") # Distancias entre clusters
hc_average <- hclust(d = mat_dist, method = "average")
hc_single <- hclust(d = mat_dist, method = "single")
hc_ward <- hclust(d = mat_dist, method = "ward.D2")


# Para construir el objetivo dist a partir de matrices
(m = matrix(c(0,5,1,5,0,8,1,8,0), nrow = 3))
as.dist(m)

cor(x = mat_dist, cophenetic(hc_complete)) # Cuanto mas cercano a 1 mejor es la clusterizacion
cor(x = mat_dist, cophenetic(hc_average))
cor(x = mat_dist, cophenetic(hc_single))
cor(x = mat_dist, cophenetic(hc_ward))

plot(hc_ward)
rect.hclust(hc_ward, k=3, border="green")
grupos<-cutree(hc_ward,h=5)
length(split(rownames(datos),grupos))


# Otra forma
library(factoextra) 
library(dplyr)
datos2 <- USArrests 
set.seed(101) 

hc_completo <- datos2 %>% scale() %>% dist(method = "euclidean") %>% 
  hclust(method = "complete") # Todo en uno usando dplyr

fviz_dend(x = hc_completo, h = 1.5, cex = .5, horiz = T, repel = T, rect =T)
# Dendograma con numero de clusters y linea custom

fviz_cluster(object = list(data = datos2, cluster = cutree(hc_completo, k = 2)), ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE) + 
  theme_bw()

fviz_dend(x = hc_completo, k = 4, cex = 0.6, rect = T) 
--  geom_hline(yintercept = 3.5, linetype = "dashed")

fviz_cluster(object = list(data = datos2, cluster = cutree(hc_completo, k = 4)), ellipse.type = "convex", repel = TRUE, show.clust.cent = FALSE) + 
  theme_bw()


# K-means:

library(factoextra) 
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "wss", 
             diss = dist(datos, method = "euclidean")) + 
  geom_vline(xintercept = 4, linetype = 3)

set.seed(123) 
km_clusters <- kmeans(x = datos, centers = 4, nstart = 25)
names(km_clusters)
split(rownames(datos),km_clusters$cluster)

fviz_cluster(object = km_clusters, data = datos, show.clust.cent = TRUE, ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) + 
  theme_bw() + 
  theme(legend.position = "none")
