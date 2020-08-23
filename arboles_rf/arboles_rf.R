library(rpart)
library(rpart.plot)
library(readr)
library(randomForest)

#se define el directorio de trabajo, cambiar para cada caso
workdir_path <- "~/lab_geomarketing/arboles_rf"
setwd(workdir_path)

#se lee la base de datos en formato csv
casen_mod <- read.csv(file = "data/casen_mod.csv")


#transformar a factor
casen_mod$nac = as.factor(casen_mod$nac)
casen_mod$ing_nivel = as.factor(casen_mod$ing_nivel)
casen_mod$sexo = as.factor(casen_mod$sexo)

#Tablas para la rm y la región 11
casen_reg11 = casen_mod[(casen_mod$region == 11),]

casen_rm = casen_mod[(casen_mod$region == 13),]

#Histogramas
hist_ing_reg11 = ggplot(casen_reg11,aes(ingreso)) + 
  geom_histogram(fill="#005A32", color= "#D9F0A3",bins = 50)+
  ggtitle("Histograma Nivel de Ingresos Región de Aysén")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Ingreso", y= "Frecuencia") +
  xlim(c(0, 1500000))+
  geom_vline(data=casen_reg11, aes(xintercept=mean(ingreso), color="promedio"),
             linetype="dashed")+
  geom_vline(data=casen_reg11, aes(xintercept=median(ingreso), color="mediana"),
             linetype="dashed")+
  scale_color_manual(name = "Indicador", values = c(mediana="blue",promedio="red"))

hist_ing_rm = ggplot(casen_rm,aes(ingreso)) + 
  geom_histogram(fill="#005A32", color= "#D9F0A3",bins = 50)+
  ggtitle("Histograma Nivel de Ingresos Región Metropolitana")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Ingreso", y= "Frecuencia") +
  xlim(c(0, 1500000))+
  geom_vline(data=casen_rm, aes(xintercept=mean(ingreso), color="promedio"),
             linetype="dashed")+
  geom_vline(data=casen_rm, aes(xintercept=median(ingreso), color="mediana"),
             linetype="dashed")+
  scale_color_manual(name = "Indicador", values = c(mediana="blue",promedio="red"))


plot(hist_ing_reg11)
plot(hist_ing_rm)

#Generación Variables

#0 si el ingreso per cápita es bajo el promedio, 1 en caso contrario
casen_reg11$nivel_ing = ifelse(casen_reg11$ingreso <= mean(casen_reg11$ingreso),0,1)
casen_rm$nivel_ing = ifelse(casen_rm$ingreso <= mean(casen_rm$ingreso),0,1)


#0 si el ingreso per cápita es bajo la mediana, 1 en caso contrario
casen_reg11$ing_med = ifelse(casen_reg11$ingreso <= median(casen_reg11$ingreso),0,1)
casen_rm$ing_med = ifelse(casen_rm$ingreso <= median(casen_rm$ingreso),0,1)


##---------------- Decision trees  ----------------##

## Reg 11

### Modelos
arbol_11=rpart(nivel_ing~edad+esc+sexo+nac,casen_reg11,model=T,method = "class")

arbol_11_med=rpart(ing_med~edad+esc+sexo+nac,casen_reg11,model=T,method = "class")

arbol_11_3q=rpart(ing_3q~edad+esc+sexo+nac,casen_reg11,model=T,method = "class")


### Gráficos
rpart.plot(arbol_11,type=5,extra=104)
rpart.plot(arbol_11_med,type=5,extra=104)


## RM

### Modelos
arbol_rm=rpart(nivel_ing~edad+esc+sexo+nac,casen_rm,model=T,method = "class")

arbol_rm_med=rpart(ing_med~edad+esc+sexo+nac,casen_rm,model=T,method = "class")



### Gráficos
rpart.plot(arbol_rm,type=5,extra=104)
rpart.plot(arbol_rm_med,type=5,extra=104)

##-----------------           Parte dos              ----------------##


#----------- comparación con una iteración--------#

#se establece la semilla
set.seed(1)
# Se divide la poblacion en muestra de entrenamiento y evaluacion
dt = sort(sample(nrow(casen_mod), nrow(casen_mod)*.7))
casen_train<-casen_mod[dt,]
casen_test<-casen_mod[-dt,]


#--------Árbole de decisión---------#

#se genera la regresión, la predicción y finalmente se calculan los valores clasificados correctamente
arbol_casen = rpart(ing_nivel~edad+esc+sexo+nac,casen_train,model=T, method = "class")
pred_ad = predict(arbol_casen,casen_test,type = "class")
aciertos_ad = which(casen_test$ing_nivel==pred_ad)

#Se calcula el porcentaje de acierto
summary(pred_ad)
(length(aciertos_ad)/nrow(casen_test))*100
plot(arbol_casen, main = "Árbol para toda la encuesta CASEN")

#--------Random Forest---------#

set.seed(1)
start_time <- Sys.time()
ingresos_rf = randomForest(ing_nivel~edad+esc+sexo+nac, data = casen_train, ntree = 500)
pred_rf  = predict(ingresos_rf, casen_test)
end_time <- Sys.time()
time_rf = end_time - start_time
time_rf
aciertos_rf = which(casen_test$ing_nivel==pred_rf)

#Se calcula el porcentaje de acierto
summary(pred_rf)
(length(aciertos_rf)/nrow(casen_test))*100
#Se plotea el error vs los arboles y también se plotea la importancia de las variables
plot(ingresos_rf, main = "Relación error vs tamaño de bosque")
varImpPlot(ingresos_rf, main = "Importancia de variables")





#-------- Comparación de métodos con varias iteraciones--------#


#se instala la semilla
set.seed(2020)
#se inicia el contador de tiempo de ejecución
tiempos_ad <- list()
aciertos_ad_lst = list()
for (i in 1:100)
{
  dt = sort(sample(nrow(casen_mod), nrow(casen_mod)*.7))
  casen_train<-casen_mod[dt,]
  casen_test<-casen_mod[-dt,]
  start_time <- Sys.time()
  arbol_casen = rpart(ing_nivel~edad+esc+sexo+nac,casen_train,model=T, method = "class")
  pred_ad = predict(arbol_casen,casen_test,type = "class")
  end_time <- Sys.time()
  time_ad = end_time - start_time
  tiempos_ad = c(tiempos_ad, as.numeric(time_ad))
  aciertos_ad = which(casen_test$ing_nivel==pred_ad)
  aciertos_ad_lst = c(aciertos_ad_lst, length(aciertos_ad))
}

pruebas_ad = data.frame()
pruebas_ad = data.frame(matrix(unlist(tiempos_ad)))
names(pruebas_ad)[1] = "tiempos"
pruebas_ad = cbind(pruebas_ad, aciertos = unlist(aciertos_ad_lst))

summary(pruebas_ad)

#Tiempos de Árboles de decisión
hist_tiempo_ad = ggplot(pruebas_ad,aes(tiempos)) + 
  geom_histogram(fill="#005A32", color= "#D9F0A3",bins = 50)+
  ggtitle("Histograma de tiempos de computo para árboles de decisión")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Tiempo", y= "Frecuencia") +
  xlim(c(0.45, 0.6))+
  geom_vline(data=pruebas_ad, aes(xintercept=mean(tiempos), color="promedio"),
             linetype="dashed")+
  geom_vline(data=pruebas_ad, aes(xintercept=median(tiempos), color="mediana"),
             linetype="dashed")+
  scale_color_manual(name = "Indicador", values = c(mediana="blue",promedio="red"))

hist_tiempo_ad

#Aciertos de Árboles de decisión
hist_aciertos_ad = ggplot(pruebas_ad,aes(aciertos)) + 
  geom_histogram(fill="#005A32", color= "#D9F0A3",bins = 50)+
  ggtitle("Histograma de aciertos para árboles de decisión")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Tiempo", y= "Frecuencia") +
  xlim(c(23000, 23500))+
  geom_vline(data=pruebas_ad, aes(xintercept=mean(aciertos), color="promedio"),
             linetype="dashed")+
  geom_vline(data=pruebas_ad, aes(xintercept=median(aciertos), color="mediana"),
             linetype="dashed")+
  scale_color_manual(name = "Indicador", values = c(mediana="blue",promedio="red"))

hist_aciertos_ad


#Predicción Random Forest
#se instala la semilla
set.seed(2021)
#Se definen las listas vacias
tiempos_rf <- list()
aciertos_rf_lst = list()
#se inicia el loop para las regresiones
for(i in 1:50)
{
  dt = sort(sample(nrow(casen_mod), nrow(casen_mod)*.7))
  casen_train<-casen_mod[dt,]
  casen_test<-casen_mod[-dt,]
  start_time <- Sys.time()
  ingresos_rf = randomForest(ing_nivel~edad+esc+sexo+nac, data = casen_train, ntree = 100)
  pred_rf  = predict(ingresos_rf, casen_test)
  end_time <- Sys.time()
  time_rf = end_time - start_time
  tiempos_rf = c(tiempos_rf, as.numeric(time_rf))
  aciertos_rf = which(casen_test$ing_nivel==pred_rf)
  aciertos_rf_lst = c(aciertos_rf_lst, length(aciertos_rf))
}

pruebas_rf = data.frame()
pruebas_rf = data.frame(matrix(unlist(tiempos_rf)))
names(pruebas_rf)[1] = "tiempos"
pruebas_rf = cbind(pruebas_rf, aciertos = unlist(aciertos_rf_lst))
summary(pruebas_rf)

#Tiempos de randomForest
hist_tiempo_rf = ggplot(pruebas_rf,aes(tiempos)) + 
  geom_histogram(fill="#005A32", color= "#D9F0A3",bins = 50)+
  ggtitle("Histograma de tiempos de computo para Random Forest")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Tiempo", y= "Frecuencia") +
  xlim(c(2.5, 2.9))+
  geom_vline(data=pruebas_rf, aes(xintercept=mean(tiempos), color="promedio"),
             linetype="dashed")+
  geom_vline(data=pruebas_rf, aes(xintercept=median(tiempos), color="mediana"),
             linetype="dashed")+
  scale_color_manual(name = "Indicador", values = c(mediana="blue",promedio="red"))

hist_tiempo_rf



#Aciertos de randomForest
hist_aciertos_rf = ggplot(pruebas_rf,aes(aciertos)) + 
  geom_histogram(fill="#005A32", color= "#D9F0A3",bins = 50)+
  ggtitle("Histograma de aciertos para Random Forest")+
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x= "Tiempo", y= "Frecuencia") +
  xlim(c(23250, 23700))+
  geom_vline(data=pruebas_rf, aes(xintercept=mean(aciertos), color="promedio"),
             linetype="dashed")+
  geom_vline(data=pruebas_rf, aes(xintercept=median(aciertos), color="mediana"),
             linetype="dashed")+
  scale_color_manual(name = "Indicador", values = c(mediana="blue",promedio="red"))

hist_aciertos_rf


#tabla de resultados final
resultados_f = data.frame(stringsAsFactors=FALSE )
test_ad = c(as.character("arbolesDecision"),mean(pruebas_ad$tiempos), mean(pruebas_ad$aciertos), (mean(pruebas_ad$aciertos)/nrow(casen_test))*100)
test_rf = c(as.character("randomForest"), mean(pruebas_rf$tiempos), mean(pruebas_rf$aciertos), (mean(pruebas_rf$aciertos)/nrow(casen_test))*100)
resultados_f = rbind(resultados_f, test_ad, stringsAsFactors=FALSE )
resultados_f = rbind(resultados_f, test_rf)
colnames(resultados_f) <- c("metodo","tiempos","aciertos","porc promedio")
resultados_f
