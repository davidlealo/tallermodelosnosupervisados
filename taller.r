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

# 4 componentes
pca <- prcomp(df, scale = TRUE, nstart = 10)
summary(pca)
