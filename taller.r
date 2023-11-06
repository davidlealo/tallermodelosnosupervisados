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

