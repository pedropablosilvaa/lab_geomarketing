library(rpart)
library(rpart.plot)
library(readr)


#se define el directorio de trabajo, cambiar para cada caso
workdir_path <- "~/lab_geomarketing"
setwd(workdir_path)

#se lee la base de datos en formato csv
casen <- read.csv(file = "data/casen_mod.csv")


#trabsformar a factor
casen_mod$nac = as.factor(casen_mod$nac)
casen_mod$ing_nivel = as.factor(casen_mod$ing_nivel)
casen_mod$sexo = as.factor(casen_mod$sexo)

#Tablas para la rm y la región 11
casen_reg11 = casen_mod[(casen_mod$region == 11),]

casen_rm = casen_mod[(casen_mod$region == 13),]

#Arboles

## Reg 11

### Sin nac
arbol_11=rpart(ing_nivel~edad+esc+sexo,casen_reg11,model=T,method = "class")
rpart.plot(arbol_11,type=5,extra=104)

### Con nac
arbol_11_nac=rpart(ing_nivel~edad+esc+sexo+nac,casen_reg11,model=T,method = "class")
rpart.plot(arbol_11_nac,type=5,extra=104)

## RM
### Sin nac
arbol_rm=rpart(ing_nivel~edad+esc+sexo,casen_rm,model=T, method = "class")
rpart.plot(arbol_rm,type=5,extra=104)

### Con nac
arbol_rm_nac=rpart(ing_nivel~edad+esc+sexo+nac,casen_rm,model=T, method = "class")
rpart.plot(arbol_rm_nac,type=5,extra=104)