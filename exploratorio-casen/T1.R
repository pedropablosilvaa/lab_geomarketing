library(caret)
library(psych)
library(RPostgreSQL)
library(dplyr)
library(ggplot2)
library(GGally)
library(randomForest)
library(rmarkdown)

#Conectar con dbpsql
#clave de acceso
pw = {
  "admin"
}
drv = dbDriver("PostgreSQL")

#Crear conexión a base de datos
con = dbConnect(drv, dbname = "postgres",
                host = "localhost", port = 5432,
                user = "postgres", password = "admin")

#Solicitud SQL data "general"
q = "SELECT ytotcor as ingreso, edad, esc, sexo, expc FROM public.casen;"
data = dbSendQuery(con, q)
data = dbFetch(data)

#analisis exploratorio
head(data)
summary(data) 

#Histograma ingresos sin factor de expansion
data$ingreso = as.numeric(data$ingreso)
hist(data$ingreso, breaks = "FD",main = "histograma Ingreso sin factor de expansion", xlab = "Ingreso total corregido" ,xlim = c(0,4000000))
#Histograma ingresos con factor de expansión
#q = "SELECT ytotcor, COUNT(ytotcor), SUM(CAST(expc AS DOUBLE PRECISION)) as ToTingr FROM public.casen GROUP BY ytotcor ORDER BY LENGTH(ytotcor),ytotcor;"
#data_ytotcor = dbSendQuery(con, q)
#data_ytotcor = dbFetch(data_ytotcor)
#ata_ytotcor = data_ytotcor[-2,]
#data_ytotcor$ytotcor = as.numeric(data_ytotcor$ytotcor)
#arplot(data_ytotcor$totingr, names.arg=data_ytotcor$ytotcor ,xlab="Ingresos",ylab="Frecuencia",col="blue",
#       main="Histograma ingresos GS con factor de expansion", border="black")


#Histograma Edad sin factor de expansion
data$edad = as.numeric(data$edad)
hist(data$edad, breaks = "FD",main = "histograma edad sin factor de expansion", xlab = "Edad" )
#Histograma Edad con factor de expansion
q = "SELECT edad, COUNT(edad), SUM(CAST(expc AS DOUBLE PRECISION)) as Totedad FROM public.casen GROUP BY edad ORDER BY LENGTH(edad),edad;"
data_edad = dbSendQuery(con, q)
data_edad = dbFetch(data_edad)
barplot(data_edad$totedad,names.arg=data_edad$edad ,xlab="Edad",ylab="Frecuencia",col="blue",
        main="Histograma Edad GS con Factor de expansion",border="black")


#Histograma escolaridad sin factor de expansion
data$esc = as.numeric(data$esc)
hist(data$esc, breaks = "FD",main = "histograma de años de escolaridad sin factor de expansion", xlab = "Años de escolaridad")
#Histograma Escolaridad con factor de expansion
q = "SELECT esc, COUNT(esc), SUM(CAST(expc AS DOUBLE PRECISION)) as TotEsc FROM public.casen GROUP BY esc ORDER BY LENGTH(esc),esc;"
data_esc = dbSendQuery(con, q)
data_esc = dbFetch(data_esc)
data_esc = data_esc[-24,]
barplot(data_esc$totesc,names.arg=data_esc$esc  ,xlab="Años de escolaridad",ylab="Frecuencia",col="blue",
        main="Histograma Escolaridad GS con Factor de expansion",border="black")


#Regresiones


#respaldar data
data_resp = data
#data = data_resp
# Separar datos en 70% de entrenamiento y 30% de prueba
data = data[c(1,2,3,4)]
data = data[complete.cases(data), ]
data = data[!(data$ingreso >10000000 | data$edad > 65 | data$esc <8 ),]
ggpairs(data, title="Correlacion de variables")
ggplot(data, aes(x=log(ingreso), y=esc)) + geom_point()
set.seed(5)
ind = sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train = data[ind==1,]
test = data[ind==2,]
# Definir entrenamiento
param = trainControl(method = "cv",
                     number = 20,
                     repeats = 50)



#Regresiones esc + edad


# Entrenar modelo lineal
set.seed(1239)
lm = train(log(ingreso) ~ esc + edad,
           train,
           method = 'lm',
           trControl = param)
lm
summary(lm)

set.seed(1237)
en = train(ingreso ~ esc + edad ,
           train,
           method = 'glmnet',
           tuneGrid = expand.grid(alpha = seq(0, 1, length = 10),
                                  lambda = seq(0.0001, 0.1, length = 5)),
           trControl = param)
en
summary(en)

param = trainControl(method = "adaptive_cv",
                     number = 500)
rf = randomForest(log(ingreso) ~ esc + edad, data = train, trControl = param, ntree = 1000)
rf$rsq
rf
plot(rf)
summary(rf)

#Regresiones esc + edad + sexo
param = trainControl(method = "cv",
                     number = 20,
                     repeats = 50)
# Entrenar modelo lineal
set.seed(1239)
lm = train(log(ingreso) ~ esc + edad + sexo,
           train,
           method = 'lm',
           trControl = param)
lm
summary(lm)

set.seed(1237)
en = train(ingreso ~ esc + edad + sexo,
           train,
           method = 'glmnet',
           tuneGrid = expand.grid(alpha = seq(0, 1, length = 10),
                                  lambda = seq(0.0001, 0.1, length = 5)),
           trControl = param)
en
summary(en)

param = trainControl(method = "adaptive_cv",
                     number = 500)
rf = randomForest(ingreso ~ esc + edad + sexo, data = train, trControl = param, ntree = 1000)
rf$rsq
rf
plot(rf)
summary(rf)
#Elastic net es la mejor regresión
#analizar betas, solo eso, igual poner un grafico
#beta negativo (valor de referencia), contabilizar el sexo.