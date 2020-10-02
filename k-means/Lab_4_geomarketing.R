# Análisis de clusterización para el Gran Santiago utilizando k-means
#Autor: Pedro Pablo Silva Antilef
#Profesor: Ricardo Crespo Vergara
#Asignatura Geomarketing para la carrera de Ingeniería Civil en Geografía


#Se cargan las librerías
#factoextra nos permitirá plotear de mejor manera los clusters
library(factoextra)
#ggplot2 para la creación de gráficos
library(ggplot2)
#funcion pip
library(tidyverse)
#temas para gráficos
library(hrbrthemes)
#para hacer el count
library(plyr)
#libreria para calcular diversidad
library(vegan)
#libreria para cartografias
library(cartography)

#Se define la ruta del directorio de trabajo
workdir_path <- "C:/Users/ppsa_/Documents/Drope/u/Nivel 10/Geomarketing/lab_geomarketing/k-means"
setwd(workdir_path)

#Se carga la matrix del censo reducido
censo_gs_red = read.csv("censo_gs_red.csv", header = TRUE)


# ------------------- Parte 1 --------------------#


#Metodo elbow
set.seed(1234)
wcss = vector()
for(i in 1:15){
  wcss[i] <- sum(kmeans(censo_gs_red[,c(2,3)], i)$withinss)
}
#Se plotean los resultados del método elbow
ggplot() + geom_point(aes(x = 1:15, y = wcss), color = 'blue') + 
  geom_line(aes(x = 1:15, y = wcss), color = 'blue') + 
  ggtitle("Método del Codo") + 
  xlab('Cantidad de Centroides k') + 
  ylab('WCSS')

#Se realiza la clusterización con 5 centroides
set.seed(1234)
kmeans = kmeans(censo_gs_red[,c(2,3)], 5, iter.max = 1000, nstart = 10)
#se agrega el cluster al que pertenece cada persona
censo_gs_red$cluster = kmeans$cluster


#Se plotea la clusterización
ggplot() + geom_point(aes(x = EDAD, y = ESCOLARIDAD, color = cluster), data = censo_gs_red, size = 2) +
  scale_colour_gradientn(colours=rainbow(4)) +
  geom_point(aes(x = kmeans$centers[, 1], y = kmeans$centers[, 2]), color = 'black', size = 3) + 
  ggtitle('Clusters de Datos con k = 5 / K-Medios') + 
  xlab('Escolaridad') + ylab('Edad')

#Se plotea un histograma de personas pertenecientes a cada cluster
hist_clusters = censo_gs_red %>%
  ggplot( aes(x=cluster)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Histograma de personas pertenecientes a cada cluster") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
hist_clusters

#se calcula la proporcion de cluster1 dentro del total de la zona
poblacionPorZonaCensal = count(censo_gs_red, "GEOCODIGO")
cluster1 = censo_gs_red %>% filter(cluster == 1)
poblacionCluster1 = count(cluster1, "GEOCODIGO")

df_cluster1 = merge(x = poblacionPorZonaCensal, y = poblacionCluster1, by = "GEOCODIGO", all = TRUE)
df_cluster1 = df_cluster1 %>% 
  mutate(proporcion_1 = (freq.y/freq.x)*100)

#se calcula la proporcion de cluster2 dentro del total de la zona
poblacionPorZonaCensal = count(censo_gs_red, "GEOCODIGO")
cluster2 = censo_gs_red %>% filter(cluster == 2)
poblacionCluster2 = count(cluster2, "GEOCODIGO")


#se calcula la proporcion de cluster3 dentro del total de la zona
poblacionPorZonaCensal = count(censo_gs_red, "GEOCODIGO")
cluster3 = censo_gs_red %>% filter(cluster == 3)
poblacionCluster3 = count(cluster3, "GEOCODIGO")

#se calcula la proporcion de cluster4 dentro del total de la zona
poblacionPorZonaCensal = count(censo_gs_red, "GEOCODIGO")
cluster4 = censo_gs_red %>% filter(cluster == 4)
poblacionCluster4 = count(cluster1, "GEOCODIGO")

#se calcula la proporcion de cluster 5 dentro del total de la zona
cluster5 = censo_gs_red %>% filter(cluster == 5)
poblacionCluster5 = count(cluster5, "GEOCODIGO")

df_cluster5 = merge(x = poblacionPorZonaCensal, y = poblacionCluster5, by = "GEOCODIGO", all = TRUE)
df_cluster5 = df_cluster5 %>% 
  mutate(proporcion_5 = (freq.y/freq.x)*100)


#Se calcula la tabla con cantidad de personas de cada cluster por zona censal
zc_clusters = merge(x = poblacionCluster1, y = poblacionCluster2, by = "GEOCODIGO", all = TRUE)
zc_clusters = merge(x = zc_clusters, y = poblacionCluster3, by = "GEOCODIGO", all = TRUE)
zc_clusters = merge(x = zc_clusters, y = poblacionCluster4, by = "GEOCODIGO", all = TRUE)
zc_clusters = merge(x = zc_clusters, y = poblacionCluster5, by = "GEOCODIGO", all = TRUE)
#se renombran las columnas
names(zc_clusters)[2] = ("Cluster1")
names(zc_clusters)[3] = ("Cluster2")
names(zc_clusters)[4] = ("Cluster3")
names(zc_clusters)[5] = ("Cluster4")
names(zc_clusters)[6] = ("Cluster5")

#Calcular indice de Shannon
zc_clusters$SHANNON = diversity(zc_clusters[,c(2,3,4,5,6)],index = "shannon")



#Se realiza un join con una capa de información territorial para visualizar la distribución espacial de los resultados
#Se carga la capa de zonas censales para el gran santiago
gs_layer <- rgdal::readOGR("C:/Users/ppsa_/Documents/Drope/u/Nivel 10/Geomarketing/T2/zonas_cens_gs/zonas_gs.geojson")
gs_layer@data = join(gs_layer@data,df_cluster1[,c(1,4)], by="GEOCODIGO")
gs_layer@data = join(gs_layer@data,df_cluster5[,c(1,4)], by="GEOCODIGO")
gs_layer@data = join(gs_layer@data,zc_clusters[,c(1,7)], by="GEOCODIGO")


#Se generan los quiebres quantiles, se plotea y agregan preferencias del layout para Cluster 1
bks1 <- getBreaks(v = gs_layer$proporcion_1, nclass = 6, method = "quantile")
choroLayer(spdf = gs_layer, df = gs_layer@data, var = "proporcion_1",  breaks = bks1, legend.pos = n)
layoutLayer(title="Cluster número 1", author = "Pedro P. Silva",
            north =TRUE, col = "#3F0C7B")
legendChoro(pos = "topleft",
            title.txt = "Cluster número 1 ",
            breaks = bks1,
            col = carto.pal("blue.pal", 6),
            nodata = TRUE, nodata.txt = "No Data")

#Se generan los quiebres quantiles, se plotea y agregan preferencias del layout para Cluster 5
bks1 <- getBreaks(v = gs_layer$proporcion_5, nclass = 6, method = "quantile")
choroLayer(spdf = gs_layer, df = gs_layer@data, var = "proporcion_5",  breaks = bks1, legend.pos = n)
layoutLayer(title="Cluster número 5", author = "Pedro P. Silva",
            north =TRUE, col = "#3F0C7B")
legendChoro(pos = "topleft",
            title.txt = "Cluster número 5 ",
            breaks = bks1,
            col = carto.pal("blue.pal", 6),
            nodata = TRUE, nodata.txt = "No Data")

#Se generan los quiebres quantiles, se plotea y agregan preferencias del layout para el índice de shannon
bks1 <- getBreaks(v = gs_layer$SHANNON, nclass = 6, method = "quantile")
choroLayer(spdf = gs_layer, df = gs_layer@data, var = "SHANNON",  breaks = bks1, legend.pos = n)
layoutLayer(title="Índice de Shannon", author = "Pedro P. Silva",
            north =TRUE, col = "#3F0C7B")
legendChoro(pos = "topleft",
            title.txt = "Índice de Shannon",
            breaks = bks1,
            col = carto.pal("blue.pal", 6),
            nodata = TRUE, nodata.txt = "No Data")



# ------------------- Parte 2 --------------------#


casen_mod = read.csv("casen_mod.csv", header = TRUE)
ayuda_AM = read.csv("tabla_microsim.csv", sep = ";",header = TRUE)
names(ayuda_AM)[1]="GEOCODIGO"


#Se hace count de las personas mayores a 60 por ZC
edad_mas_60 = aggregate (EDAD ~ GEOCODIGO, data = censo_gs_red, FUN = function (a) {length(which(a>=60))})
#Se hace count de las personas con escolaridad menor a 12 años por ZC
esc_menos_12 = aggregate (ESCOLARIDAD ~ GEOCODIGO, data = censo_gs_red, FUN = function (a) {length(which(a<=12))})
#se juntan las tablas
tabla_f = merge(x = edad_mas_60, y = esc_menos_12, by = "GEOCODIGO", all = TRUE)
tabla_f = merge(x = tabla_f, y = ayuda_AM, by = "GEOCODIGO", all = TRUE)
tabla_f = merge(x = tabla_f, y = poblacionPorZonaCensal, by = "GEOCODIGO", all = TRUE)

#Se calculan las proporciones
tabla_f = tabla_f %>% 
  mutate(perc_esc = (ESCOLARIDAD/freq)*100)
tabla_f = tabla_f %>% 
  mutate(perc_edad = (EDAD/freq)*100)
tabla_f = tabla_f %>% 
  mutate(perc_ayuda = (ayuda/freq)*100)

tabla_f = na.omit(tabla_f)
#se define la cantidad optima de clusters
set.seed(1234)
wcss = vector()
for(i in 1:15){
  wcss[i] <- sum(kmeans(tabla_f[,c(6,7,8)], i)$withinss)
}

# se plotean ls resultados del número optimo de 
ggplot() + geom_point(aes(x = 1:15, y = wcss), color = 'blue') + 
  geom_line(aes(x = 1:15, y = wcss), color = 'blue') + 
  ggtitle("Método del Codo") + 
  xlab('Cantidad de Centroides k') + 
  ylab('WCSS')

#se aplica el algoritmo k-means
set.seed(1234)
kmeans2 = kmeans(tabla_f[,c(6,7,8)], 4, iter.max = 1000, nstart = 10)
#se agrega el cluster al que pertenece cada persona
tabla_f$cluster = kmeans2$cluster

#se plotean los gráficos de 
plot(tabla_f[,c(6,7,8)], col = tabla_f$cluster,pch=7)

#Se plotea un histograma de personas pertenecientes a cada cluster
hist_clusters = tabla_f %>%
  ggplot( aes(x=cluster)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Histograma de personas pertenecientes a cada cluster") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )
hist_clusters


#se plotean los resultados espacialmente
gs_layer@data = join(gs_layer@data,tabla_f[,c(1,9)], by="GEOCODIGO")

choroLayer(spdf = gs_layer, df = gs_layer@data, var = "cluster",  breaks = c(1,2,3,4,5), legend.pos = n, col = carto.pal("multi.pal", 5))
layoutLayer(title="Clusters de cada ZC con ayuda de pensiones estatales", author = "Pedro P. Silva",
            north =TRUE, col = "#3F0C7B")
legendChoro(pos = "topleft",
            title.txt = "Clusters",
            breaks = c(0,1,2,3,4),
            col = carto.pal("multi.pal", 4),
            nodata = TRUE, nodata.txt = "No Data")
  
