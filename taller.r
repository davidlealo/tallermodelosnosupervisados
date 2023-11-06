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

# K-means
kmeans <- kmeans(df, centers = 4, nstart = 25)
kmeans

# Gráfico de grupos
fviz_cluster(kmeans, data = df, geom = "point", ellipse.type = "norm", ellipse.level = 0.95)
