# <h1>Modelos no supervizados
# El set de datos datos clustering.csv contiene 1000
# observaciones simuladas, en la que se poseen 10 atributos numéricos
# llamadas V1 a V10, además, del atributo y que indica el
# grupo real de la observación

# Cargar los datos
datos <- read.csv("data/datos_clustering.csv", header = TRUE)
head(datos)

# drop columna y
df <- subset(datos, select = -c(y))
head(df)


# Pregunta 1
# Realice un análisis de componentes principales seleccione la cantidad
# de componentes que expliquen al menos el 70% de la variabilidad de los datos.

# Realizar PCA
pca <- prcomp(df, scale = TRUE)
summary(pca)

# Select number of components
plot(pca, type = "l")
abline(h = 0.7, col = "red")

# Suma de variabilidad de datos por componente
cumsum(pca$sdev^2 / sum(pca$sdev^2))

# Resultado: 
# 0.3908047 0.6120513 0.7509590 0.8425197 0.9086429 0.9409362 0.9585046

# Pregunta 2
# Detecte el número optimo de clustering utilizando la información de
# PCA con la metodología de Elbow y Average silhouette
install.packages("factoextra")
library(factoextra)

# Elbow
fviz_nbclust(df, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)

# Average silhouette
fviz_nbclust(df, kmeans, method = "silhouette") + geom_vline(xintercept = 4, linetype = 2)

# Pregunta 3
# Ajuste un clustering de K-means, utilizando las 2 principales
# componentes principales gráfique los grupos ajustados, qué puede concluir al respecto?

# Instalar ggplot2
install.packages("ggplot2")
library(ggplot2)

# K-means
kmeans <- kmeans(df, centers = 4, nstart = 25)
kmeans

# Gráfico de grupos
fviz_cluster(kmeans, data = df, geom = "point", ellipse.type = "norm", ellipse.level = 0.95)

# Graficos con centroides
fviz_cluster(kmeans, data = df, geom = "point", ellipse.type = "norm", ellipse.level = 0.95) + 
  geom_point(data = as.data.frame(pca$x[,1:2]), aes(x = PC1, y = PC2), color = "red", size = 3) +
  geom_text(data = as.data.frame(pca$x[,1:2]), aes(x = PC1, y = PC2, label = rownames(pca$x)), color = "red", size = 3)

install.packages("cluster")
library(cluster)

install.packages("fpc")
library(fpc)

# Metricas de evaluación de k-means
# Silhouette
fviz_silhouette(silhouette(kmeans$cluster, dist(df, method = "euclidean")), geom = "segment")

# Nueva agrupacion con 5 grupos
kmeans2 <- kmeans(df, centers = 5, nstart = 25)
kmeans2

#Grafico de kmeans2
fviz_cluster(kmeans2, data = df, geom = "point", ellipse.type = "norm", ellipse.level = 0.95) + 
  geom_point(data = as.data.frame(pca$x[,1:2]), aes(x = PC1, y = PC2), color = "red", size = 3) +
  geom_text(data = as.data.frame(pca$x[,1:2]), aes(x = PC1, y = PC2, label = rownames(pca$x)), color = "red", size = 3)


# En este caso el resultado muestra como óptimo 4 grupos, 
# sin embargo, al graficar los datos con las dos primeras componentes principales, 
# se observa que los grupos no son muy claros, por lo que se decide probar con 5 grupos, 
# en donde se observa que los grupos son más claros y se puede observar que 
# los grupos 1 y 2 se encuentran más cercanos entre sí, por lo que se decide 
# realizar un nuevo análisis con 5 grupos.

# Análisis de 5 grupos
# Silhouette
fviz_silhouette(silhouette(kmeans2$cluster, dist(df, method = "euclidean")), geom = "segment")

library(fpc)
kmeans_result <- kmeans(df, centers = 5, nstart = 25)

# Calcula el índice Davies-Bouldin (DB)
db_index <- cluster.stats(df, kmeans_result$cluster)$db

# Calcula la matriz de distancias
dist_matrix <- dist(df)
dist_matrix
# Calcula el índice Davies-Bouldin (DB)
db_index <- cluster.stats(dist_matrix, kmeans_result$cluster)$db
db_index

# Calcula el índice Calinski-Harabasz (CH)
ch_index <- cluster.stats(dist_matrix, kmeans_result$cluster)$ch
ch_index

# Calcula el índice Dunn (D)
dunn_index <- cluster.stats(dist_matrix, kmeans_result$cluster)$dunn
dunn_index

library(cluster)

# Vector para almacenar los valores de CH
ch_values <- numeric(7)

# Realiza el análisis de k-means y calcula CH para diferentes valores de k
for (k in 1:7) {
  kmeans_result <- kmeans(df, centers = k, nstart = 25)
  ch_values[k] <- cluster.stats(dist(df, method = "euclidean"), kmeans_result$cluster)$ch
}

# Imprime los valores de CH para cada valor de k
print(ch_values)
