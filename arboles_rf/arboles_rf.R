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

##----------------- Random Forest ----------------##

# Se divide la poblacion en muestra de entrenamiento y evaluacion
dt = sort(sample(nrow(casen_mod), nrow(casen_mod)*.7))
casen_train<-casen_mod[dt,]
casen_test<-casen_mod[-dt,]


#Predicción Random Forest
ingresos_rf = randomForest(ing_nivel~edad+esc+sexo+nac, data = casen_train, ntree = 500)
pred_rf  = predict(ingresos_rf, casen_test)
aciertos_rf = which(casen_test$ing_nivel==pred_rf)

#Predicción Árboles de Decisión
arbol_casen = rpart(ing_nivel~edad+esc+sexo+nac,casen_train,model=T, method = "class")
pred_ad = predict(arbol_casen,casen_test,type = "class")
aciertos_ad = which(casen_test$ing_nivel==pred_ad)
