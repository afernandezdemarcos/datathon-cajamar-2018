######################### LIMPIEZA DE ENTORNO Y CARGA DE LIBRERIAS #################################
rm(list=ls())
library(readr)
library(plotly)
library(readr)
library(randomForest)
##################################### CARGA DE DATASET TRAINING ####################################
setwd("D:/Datathon18")
train <- read_csv("./DatosOriginales/Dataset_Salesforce_Predictive_Modelling_TRAIN.txt")
train = as.data.frame(train)
# En primer lugar realizamos una división del mercado, para ver las franjas del poder adquisitivo
datosimporte = train[,c(2:18,89)]
datosimporte = as.data.frame(datosimporte)
mercados = kmeans(datosimporte, 3, nstart = 4)
cluster = mercados$cluster
train$Mercado = cluster
train$Mercado = as.factor(train$Mercado)
##################################### ANÁLISIS DE VARIABLES #########################################
#
# Tras haber realizado un análisis exploratorio de las variables y siguiendo la metodología descrita en cuerpo.txt
# se procede a la modificación y limpieza de las variables para entrenar el modelo
#
# Ind_Cons_06
# Socio_Demo_02 
# Total_Cons
# Total_Sal
# Indice2
# Poder_Adquisitivo
# Modificamos la tenencia a 0 - No tiene y 1 - Tiene (El 2 no es de importancia)
for (i in 40:63){
  train[train[,i]==2,i]=1
}
# Creamos un indice que representa el numero de productos "ricos" (que tienen los ricos pero apenas los pobres)
indice = 0
for (i in c(39+1,39+4,39+13,39+15,39+17,39+22,39+23)){
  indice = indice+train[,i]
}
train$indice = indice
# Este otro indice representa el numero de productos "medios" (que tienen los medios, pero apenas pobres)
indice = 0
for (i in c(39+2,39+10,39+11,39+14,39+16,39+21)){
  indice = indice+train[,i]
}
train$indice2 = indice

train$indice2[train$indice==5]=8
train$indice2[train$indice==4]=7
train$indice2[train$indice==3]=6
train$indice2[train$indice==2]=5
train$indice2[train$indice==1]=4

# Variable suma de consumos
suma=0
VectorConsumo = c(1,2,3,4,6,9,12,15,16)
for (i in (1+VectorConsumo)){suma=suma+train[,i]}
train$total_cons = suma
# Variable suma de saldos clave (los saldos 8,9,19,21)
suma=0
VectorSaldo = c(8,9,16,19,20,21)
for (i in (1+VectorSaldo)){suma=suma+train[,i]}
train$total_sal = suma
train = train[,c(7,85,92:94,90)]

write.csv(train, file="./trainTotal.csv",row.names = F)

###############################################################################################
###########                                                                         ###########
###########               MODELO DE CLASIFICACIÓN EN TRES GRUPOS                    ###########
###########                                                                         ###########
###############################################################################################

###############################################################################################
##############################  DIVISIÓN DE DATASET  ##########################################
###############################################################################################
rm(list=ls())
library(readr)
library(DMwR)
# Leemos el dataset previamente preparado
train = read_csv("D:/Datathon18/Universidad Politécnica de Madrid#SCAhaters/trainTotal.csv", 
                 col_types = cols(Mercado = col_factor(levels = c("1","2", "3"))))
train$Socio_Demo_02 = as.factor(train$Socio_Demo_02)
train$indice2 = as.factor(train$indice2)
train = as.data.frame(train)

# Dividir test(10%) y train(90%)
muestrastrain = sample(nrow(train),0.9*nrow(train))
train = train[muestrastrain,]
test = train[-muestrastrain,]

write.csv(train, file="./train.csv",row.names = F)
write.csv(test, file="./test.csv",row.names = F)

# Aplicamos el algoritmo SMOTE con el objetivo de equilibrar las muestras de los distintos grupos.
train12 = train[train$Mercado==1 | train$Mercado==2,]
train12$Mercado = as.factor(as.numeric(train12$Mercado))
train23 = train[train$Mercado==2 | train$Mercado==3,]
train23$Mercado = as.factor(as.numeric(train23$Mercado))

trainBalanced12 = SMOTE(Mercado~.,data = train12, perc.over = 5000,porc.under = 0)
trainBalanced23 = SMOTE(Mercado~.,data = train23, perc.over = 100000,porc.under = 0)

trainFinal = rbind(trainBalanced12[trainBalanced12$Mercado == 1,], trainBalanced23[trainBalanced23$Mercado == 3,])
trainmajority = train[train$Mercado == 2,]
muestras = sample(nrow(trainmajority),200000)
trainmajority = trainmajority[muestras,]
trainFinal = rbind(trainmajority,trainFinal)
write.csv(trainFinal, file="./trainBalanced.csv",row.names = F)

###############################################################################################
##################### ENTRENAMIENTO DEL MODELO DE CLASIFICACIÓN 1 2 3 #########################
###############################################################################################
rm(list=ls())
train = read_csv("D:/Datathon18/Universidad Politécnica de Madrid#SCAhaters/trainBalanced.csv")
train = as.data.frame(train)
train[,2] = as.factor(train[,2])
train[,3] = as.factor(train[,3])
train[,6] = as.factor(train[,6])
# Debido al gran número de muestras para el entrenamiento y a la posibilidad de paralelización 
# del algoritmo randomForest, dividimos en tres modelos de clasificación aleatoriamente.
muestrastrain12 = sample(nrow(train), 0.66*(nrow(train)))
train3 = train[-muestrastrain12,]
train12 = train[muestrastrain12,]
muestrastrain1 = sample(nrow(train12), 0.5*(nrow(train12)))
train1 = train12[muestrastrain1,]
train2 = train12[-muestrastrain1,]
write.csv(train1,file = "./train1.csv",row.names = F)
write.csv(train2,file = "./train2.csv",row.names = F)
write.csv(train3,file = "./train3.csv",row.names = F)

# ENTRENAMIENTO DEL MODELO
rm(list=ls())
setwd("D:/Datathon18/")
train <- read_csv("./Universidad Politécnica de Madrid#SCAhaters/train3.csv", 
                  col_types = cols(Mercado = col_factor(levels = c("1", "2", "3"))))
train$indice2 = as.factor(train$indice2)
train$Socio_Demo_02 = as.factor(train$Socio_Demo_02)
# library(randomForest)
# for (numberfeatures in 2:3){
#   for (numbertrees in c(500,1000,1500,2000)){
#     modelo = randomForest(data = train, Mercado~., ntrees = numbertrees, mtry = numberfeatures)
#     save(modelo,file=paste0("./modelotrain2_",numbertrees,"trees",numberfeatures,"_fea.RData"))
#   }
# }
#valorOptimo
numbertrees = 500
numberfeatures = 3
modelo = randomForest(data = train, Mercado~., ntrees = numbertrees, mtry = numberfeatures)
save(modelo,file=paste0("./modelotrain3_",numbertrees,"trees_",numberfeatures,"fea.RData"))

# TESTEO DEL MODELO
rm(list=ls())
library(readr)
library(randomForest)
library(pROC)
library(caret)
library(e1071)
setwd("D:/Datathon18/")
test = read_csv("./Universidad Politécnica de Madrid#SCAhaters/test.csv",
                col_types = cols(Mercado = col_factor(levels = c("1", "2", "3"))))
test$indice2 = as.factor(test$indice2)
test$Socio_Demo_02 = as.factor(test$Socio_Demo_02)

load("./modelotrain1_500trees_3fea.RData")
pred1 = predict(modelo, newdata = test)

load("./modelotrain2_500trees_3fea.RData")
pred2 = predict(modelo, newdata = test)

load("./modelotrain3_500trees_3fea.RData")
pred3 = predict(modelo, newdata = test)

pred = (as.numeric(pred1) + as.numeric(pred2) + as.numeric(pred3))/3
pred = round(pred)

confusionMatrix(pred, test$Mercado)
###############################################################################################
###########                                                                         ###########
##########    MODELO DE CLASIFICACIÓN EN TRES SUBGRUPOS DE MENOR PODER ADQUISITIVO  ###########
###########                                                                         ###########
###############################################################################################
##################################### CARGA DE DATASET ############################################
setwd("D:/Datathon18")
train <- read_csv("./DatosOriginales/Dataset_Salesforce_Predictive_Modelling_TRAIN.txt")
train = as.data.frame(train)
# En primer lugar realizamos una división del mercado, para ver las franjas del poder adquisitivo
datosimporte = train[,c(2:18,89)]
datosimporte = as.data.frame(datosimporte)
mercados = kmeans(datosimporte, 3, nstart = 4)
cluster = mercados$cluster
train$Mercado = cluster
train$Mercado = as.factor(train$Mercado)
pobres = train[train$Mercado==1,]
# Subdividimos los "pobres" en tres grupos
datosimporte = pobres[,c(2:18,89)]
datosimporte = as.data.frame(datosimporte)
mercados = kmeans(datosimporte, 3, nstart = 4)
cluster = mercados$cluster
pobres$Mercado = cluster
pobres$Mercado = as.factor(pobres$Mercado)

mercado1 = pobres[pobres$Mercado==1,]
mercado2 = pobres[pobres$Mercado==2,]
mercado3 = pobres[pobres$Mercado==3,]
summary(mercado1$Poder_Adquisitivo)
summary(mercado2$Poder_Adquisitivo)
summary(mercado3$Poder_Adquisitivo)

pobres$Mercado = as.numeric(pobres$Mercado)

pobres$Mercado[pobres$Mercado==2] = "2A"
pobres$Mercado[pobres$Mercado==3] = "2B"
pobres$Mercado[pobres$Mercado==1] = "2C"

# Agrupamos el dataset de train con las siguientes variables
# Ind_Cons_06
# Socio_Demo_02 
# Total_Cons
# Total_Sal
# Indice2
# Poder_Adquisitivo
# Modificamos la tenencia a 0 - No tiene y 1 - Tiene (El 2 no afecta demasiado)
for (i in 40:63){
  pobres[pobres[,i]==2,i]=1
}
# Creamos un indice que representa el numero de productos "ricos" (que tienen los ricos pero apenas los pobres)
indice = 0
for (i in c(39+1,39+4,39+13,39+15,39+17,39+22,39+23)){
  indice = indice+pobres[,i]
}
pobres$indice = indice
# Este otro indice representa el numero de productos "medios" (que tienen los medios, pero apenas pobres)
indice = 0
for (i in c(39+2,39+10,39+11,39+14,39+16,39+21)){
  indice = indice+pobres[,i]
}
pobres$indice2 = indice

pobres$indice2[pobres$indice==5]=8
pobres$indice2[pobres$indice==4]=7
pobres$indice2[pobres$indice==3]=6
pobres$indice2[pobres$indice==2]=5
pobres$indice2[pobres$indice==1]=4

# Variable suma de consumos
suma=0
VectorConsumo = c(1,2,3,4,6,9,12,15,16)
for (i in (1+VectorConsumo)){suma=suma+pobres[,i]}
pobres$total_cons = suma
# Variable suma de saldos clave (los saldos 8,9,19,21)
suma=0
VectorSaldo = c(8,9,16,19,20,21)
for (i in (1+VectorSaldo)){suma=suma+pobres[,i]}
pobres$total_sal = suma
pobres = pobres[,c(7,85,92:94,90)]

write.csv(pobres, file="./Universidad Politécnica de Madrid#SCAhaters/ModeloClasificacion1ABC/trainABCTotal.csv",row.names = F)

###############################################################################################
##############################  DIVISIÓN DE DATASET  ##########################################
###############################################################################################
rm(list=ls())
library(readr)
library(DMwR)
train = read_csv("./Universidad Politécnica de Madrid#SCAhaters/ModeloClasificacion1ABC/trainABCTotal.csv", 
                 col_types = cols(Mercado = col_factor(levels = c("2A","2B", "2C"))))
train$Socio_Demo_02 = as.factor(train$Socio_Demo_02)
train$indice2 = as.factor(train$indice2)
train = as.data.frame(train)

# Dividir test(10%) y train(90%)
muestrastrain = sample(nrow(train),0.9*nrow(train))
train = train[muestrastrain,]
test = train[-muestrastrain,]

write.csv(train, file="./Universidad Politécnica de Madrid#SCAhaters/ModeloClasificacion1ABC/train.csv",row.names = F)
write.csv(test, file="./Universidad Politécnica de Madrid#SCAhaters/ModeloClasificacion1ABC/test.csv",row.names = F)

trainAB = train[train$Mercado=="2A" | train$Mercado=="2B",]
trainAB$Mercado = as.factor(as.character(trainAB$Mercado))
trainAC = train[train$Mercado=="2A" | train$Mercado=="2C",]
trainAC$Mercado = as.factor(as.character(trainAC$Mercado))

trainBalancedAC = SMOTE(Mercado~.,data = trainAC, perc.over = 500,porc.under = 0)

trainFinal = rbind(trainBalancedAC[trainBalancedAC$Mercado == "2C",], train[train$Mercado =="2B",])
trainmajority = train[train$Mercado == "2A",]
trainFinal = rbind(trainmajority,trainFinal)
write.csv(trainFinal, file="./Universidad Politécnica de Madrid#SCAhaters/ModeloClasificacion1ABC/trainBalanced.csv",row.names = F)

###############################################################################################
##################### ENTRENAMIENTO DEL MODELO DE CLASIFICACIÓN A B C #########################
###############################################################################################
# Debido al gran número de muestras para el entrenamiento y a la posibilidad de paralelización 
# del algoritmo randomForest, dividimos en tres modelos de clasificación aleatoriamente.
rm(list=ls())
train = read_csv("./Universidad Politécnica de Madrid#SCAhaters/ModeloClasificacion1ABC/trainBalanced.csv")
train = as.data.frame(train)
train[,2] = as.factor(train[,2])
train[,3] = as.factor(train[,3])
train[,6] = as.factor(train[,6])

muestrastrain12 = sample(nrow(train), 0.66*(nrow(train)))
train3 = train[-muestrastrain12,]
train12 = train[muestrastrain12,]
muestrastrain1 = sample(nrow(train12), 0.5*(nrow(train12)))
train1 = train12[muestrastrain1,]
train2 = train12[-muestrastrain1,]
write.csv(train1,file = "./Universidad Politécnica de Madrid#SCAhaters/ModeloClasificacion1ABC/train1.csv",row.names = F)
write.csv(train2,file = "./Universidad Politécnica de Madrid#SCAhaters/ModeloClasificacion1ABC/train2.csv",row.names = F)
write.csv(train3,file = "./Universidad Politécnica de Madrid#SCAhaters/ModeloClasificacion1ABC/train3.csv",row.names = F)

# ENTRENAMIENTO DEL MODELO
rm(list=ls())
setwd("D:/Datathon18/")
train <- read_csv("./Universidad Politécnica de Madrid#SCAhaters/ModeloClasificacion1ABC/train3.csv", 
                  col_types = cols(Mercado = col_factor(levels = c("2A", "2B", "2C"))))
train$indice2 = as.factor(train$indice2)
train$Socio_Demo_02 = as.factor(train$Socio_Demo_02)
# library(randomForest)
# for (numberfeatures in 2:3){
#   for (numbertrees in c(500,1000,1500,2000)){
#     modelo = randomForest(data = train, Mercado~., ntrees = numbertrees, mtry = numberfeatures)
#     save(modelo,file=paste0("./Universidad Politécnica de Madrid#SCAhaters/ModeloClasificacion1ABC/modelotrain2_",numbertrees,"trees",numberfeatures,"_fea.RData"))
#   }
# }
#valorOptimo
numbertrees = 500
numberfeatures = 3
modelo = randomForest(data = train, Mercado~., ntrees = numbertrees, mtry = numberfeatures)
save(modelo,file=paste0("./Universidad Politécnica de Madrid#SCAhaters/ModeloClasificacion1ABC/modelotrain3_",numbertrees,"trees_",numberfeatures,"fea.RData"))

# TESTEO DEL MODELO
rm(list=ls())
library(readr)
library(randomForest)
library(pROC)
library(caret)
library(e1071)
setwd("D:/Datathon18/")
test = read_csv("./Universidad Politécnica de Madrid#SCAhaters/ModeloClasificacion1ABC/test.csv",
                col_types = cols(Mercado = col_factor(levels = c("2A", "2B", "2C"))))
test$indice2 = as.factor(test$indice2)
test$Socio_Demo_02 = as.factor(test$Socio_Demo_02)

load("./Universidad Politécnica de Madrid#SCAhaters/ModeloClasificacion1ABC/modelotrain1_500trees_3fea.RData")
pred1 = predict(modelo, newdata = test)

load("./Universidad Politécnica de Madrid#SCAhaters/ModeloClasificacion1ABC/modelotrain2_500trees_3fea.RData")
pred2 = predict(modelo, newdata = test)

load("./Universidad Politécnica de Madrid#SCAhaters/ModeloClasificacion1ABC/modelotrain3_500trees_3fea.RData")
pred3 = predict(modelo, newdata = test)

pred = (as.numeric(pred1) + as.numeric(pred2) + as.numeric(pred3))/3
pred = round(pred)
pred = as.factor(pred)
levels(pred) = c("2A","2B","2C")
confusionMatrix(pred, test$Mercado)

###############################################################################################
###########                                                                         ###########
###########  MODELO DE REGRESIÓN PARA CADA UNO DE LOS GRUPOS PREVIAMENTE DIVIDIDOS  ###########
###########                                                                         ###########
###########   En este caso se muestra el modelo para un sólo grupo, pero el proceso ###########
###########                 es iterativo en cada uno de los grupos                  ###########
###########                                                                         ###########
###############################################################################################
######################### LIMPIEZA DE ENTORNO Y CARGA DE LIBRERIAS #################################
rm(list=ls())
##################################### CARGA DE DATASET ############################################
setwd("D:/Datathon18/Universidad Politécnica de Madrid#SCAhaters/ModeloRegresion/1/")
Grupo1 = read_csv("./Grupo1.csv")
Grupo1 = as.data.frame(Grupo1)
Grupo1$Mercado = NULL

# Dividir test(10%) y train(90%)
muestrastrain = sample(nrow(Grupo1),0.9*nrow(Grupo1))
train = Grupo1[muestrastrain,]
test = Grupo1[-muestrastrain,]

write.csv(train, file="./train.csv",row.names = F)
write.csv(test, file="./test.csv",row.names = F)

train = read_csv("./train.csv")
test = read_csv("./test.csv")
train = as.data.frame(train)
test = as.data.frame(test)
load("./modeloreg_500trees_3fea.RData")
#
# TRAIN PREPARATION
#
# Agrupamos el dataset de train con las siguientes variables
# Ind_Cons_06
# Socio_Demo_02 
# Total_Cons
# Total_Sal
# Indice2
# Poder_Adquisitivo
# Modificamos la tenencia a 0 - No tiene y 1 - Tiene (El 2 no afecta demasiado)
for (i in 40:63){
  train[train[,i]==2,i]=1
}
# Creamos un indice que representa el numero de productos "ricos" (que tienen los ricos pero apenas los pobres)
indice = 0
for (i in c(39+1,39+4,39+13,39+15,39+17,39+22,39+23)){
  indice = indice+train[,i]
}
train$indice = indice
# Este otro indice representa el numero de productos "medios" (que tienen los medios, pero apenas pobres)
indice = 0
for (i in c(39+2,39+10,39+11,39+14,39+16,39+21)){
  indice = indice+train[,i]
}
train$indice2 = indice

train$indice2[train$indice==5]=8
train$indice2[train$indice==4]=7
train$indice2[train$indice==3]=6
train$indice2[train$indice==2]=5
train$indice2[train$indice==1]=4

# Variable suma de consumos
suma=0
VectorConsumo = c(1,2,3,4,6,9,12,15,16)
for (i in (1+VectorConsumo)){suma=suma+train[,i]}
train$total_cons = suma
# Variable suma de saldos clave (los saldos 8,9,19,21)
suma=0
VectorSaldo = c(8,9,16,19,20,21)
for (i in (1+VectorSaldo)){suma=suma+train[,i]}
train$total_sal = suma
train = train[,c(7,85,91:93,89)]
train$indice2 = as.factor(train$indice2)
train$Socio_Demo_02 = as.factor(train$Socio_Demo_02)
#
# TEST PREPARATION
#
# Agrupamos el dataset de train con las siguientes variables
# Ind_Cons_06
# Socio_Demo_02 
# Total_Cons
# Total_Sal
# Indice2
# Poder_Adquisitivo
# Modificamos la tenencia a 0 - No tiene y 1 - Tiene (El 2 no afecta demasiado)
for (i in 40:63){
  test[test[,i]==2,i]=1
}
# Creamos un indice que representa el numero de productos "ricos" (que tienen los ricos pero apenas los pobres)
indice = 0
for (i in c(39+1,39+4,39+13,39+15,39+17,39+22,39+23)){
  indice = indice+test[,i]
}
test$indice = indice
# Este otro indice representa el numero de productos "medios" (que tienen los medios, pero apenas pobres)
indice = 0
for (i in c(39+2,39+10,39+11,39+14,39+16,39+21)){
  indice = indice+test[,i]
}
test$indice2 = indice

test$indice2[test$indice==5]=8
test$indice2[test$indice==4]=7
test$indice2[test$indice==3]=6
test$indice2[test$indice==2]=5
test$indice2[test$indice==1]=4

# Variable suma de consumos
suma=0
VectorConsumo = c(1,2,3,4,6,9,12,15,16)
for (i in (1+VectorConsumo)){suma=suma+test[,i]}
test$total_cons = suma
# Variable suma de saldos clave (los saldos 8,9,19,21)
suma=0
VectorSaldo = c(8,9,16,19,20,21)
for (i in (1+VectorSaldo)){suma=suma+test[,i]}
test$total_sal = suma
test = test[,c(7,85,91:93,89)]
test$indice2 = as.factor(test$indice2)
test$Socio_Demo_02 = as.factor(test$Socio_Demo_02)

#valorOptimo
numbertrees = 500
numberfeatures = 3
modelo = randomForest(data = train, Poder_Adquisitivo~., ntrees = numbertrees, mtry = numberfeatures)
# save(modelo,file=paste0("./modeloreg_",numbertrees,"trees_",numberfeatures,"fea.RData"))

pred = predict(modelo, newdata = test)
plot(test$Poder_Adquisitivo,pred)


###############################################################################################
###########                                                                         ###########
##########           EVALUACIÓN Y LANZAMIENTO DEL MODELO EN PRODUCCIÓN (TEST)       ###########
###########                                                                         ###########
###############################################################################################
########################## LIMPIEZA DE ENTORNO Y CARGA DE LIBRERIAS ###########################
rm(list=ls())
library(readr)
library(plotly)
library(readr)
library(randomForest)
library(caret)
library(e1071)

################################## CLASIFICACIÓN 123 ##########################################
load("~/cajamar/Code/ModeloClasificacion123/modelotrain1_500trees_3fea.RData")
modelo1 = modelo
load("~/cajamar/Code/ModeloClasificacion123/modelotrain2_500trees_3fea.RData")
modelo2 = modelo
load("~/cajamar/Code/ModeloClasificacion123/modelotrain3_500trees_3fea.RData")
modelo3 = modelo

test = read_delim("~/cajamar/DatosOriginales/Dataset_Salesforce_Predictive_Modelling_TEST.txt",delim=",")
test=as.data.frame(test)

# Agrupamos el dataset de train con las siguientes variables
# Ind_Cons_06
# Socio_Demo_02 
# Total_Cons
# Total_Sal
# Indice2
# Poder_Adquisitivo
# Modificamos la tenencia a 0 - No tiene y 1 - Tiene (El 2 no afecta demasiado)
for (i in 40:63){
  test[test[,i]==2,i]=1
}
# Creamos un indice que representa el numero de productos "ricos" (que tienen los ricos pero apenas los pobres)
indice = 0
for (i in c(39+1,39+4,39+13,39+15,39+17,39+22,39+23)){
  indice = indice+test[,i]
}
test$indice = indice
# Este otro indice representa el numero de productos "medios" (que tienen los medios, pero apenas pobres)
indice = 0
for (i in c(39+2,39+10,39+11,39+14,39+16,39+21)){
  indice = indice+test[,i]
}
test$indice2 = indice

test$indice2[test$indice==5]=8
test$indice2[test$indice==4]=7
test$indice2[test$indice==3]=6
test$indice2[test$indice==2]=5
test$indice2[test$indice==1]=4

# Variable suma de consumos
suma=0
VectorConsumo = c(1,2,3,4,6,9,12,15,16)
for (i in (1+VectorConsumo)){suma=suma+test[,i]}
test$total_cons = suma
# Variable suma de saldos clave (los saldos 8,9,19,21)
suma=0
VectorSaldo = c(8,9,16,19,20,21)
for (i in (1+VectorSaldo)){suma=suma+test[,i]}
test$total_sal = suma
test$indice2 = as.factor(test$indice2)
test$Socio_Demo_02 = as.factor(test$Socio_Demo_02)

test$indice2 = as.factor(test$indice2)
test$Socio_Demo_02 = as.factor(test$Socio_Demo_02)
pred1 = predict(modelo1, newdata = test)
pred2 = predict(modelo2, newdata = test)
pred3 = predict(modelo3, newdata = test)
pred = (as.numeric(pred1) + as.numeric(pred2) + as.numeric(pred3))/3
pred = round(pred)
pred = as.factor(pred)
test$prediccion123 = pred
grupo2 = test[test$prediccion123 == 2,]
################################## CLASIFICACIÓN 2ABC ##########################################
load("~/cajamar/Code/ModeloClasificacion1ABC/modelotrain1_500trees_3fea.RData")
modelo1ABC = modelo
load("~/cajamar/Code/ModeloClasificacion1ABC/modelotrain2_500trees_3fea.RData")
modelo2ABC = modelo
load("~/cajamar/Code/ModeloClasificacion1ABC/modelotrain3_500trees_3fea.RData")
modelo3ABC = modelo
pred1ABC = predict(modelo1ABC, newdata = grupo2)
pred2ABC = predict(modelo2ABC, newdata = grupo2)
pred3ABC = predict(modelo3ABC, newdata = grupo2)
predABC = (as.numeric(pred1ABC) + as.numeric(pred2ABC) + as.numeric(pred3ABC))/3
predABC = round(predABC)
predABC = as.factor(predABC)
levels(predABC) = c("2A","2B","2C")
grupo2$prediccionABC = predABC
test$prediccionABC = as.character(test$prediccion123)
test$prediccionABC[test$prediccionABC == 2] = as.character(predABC)
test$prediccionABC = as.factor(test$prediccionABC)
################################## REGRESION 1 ##########################################
load("~/cajamar/Code/ModeloRegresion/1/modeloreg_500trees_3fea.RData")
pred1 = predict(modelo, newdata = test[test$prediccion123 == 1,])
test$prediccionPA = 0
test$prediccionPA[test$prediccion123 == 1] = pred1
################################## REGRESION 2A ##########################################
load("~/cajamar/Code/ModeloRegresion/2A/modelo1reg_500trees_3fea.RData")
pred2A1 = predict(modelo1, newdata = test[test$prediccionABC == "2A",])
load("~/cajamar/Code/ModeloRegresion/2A/modelo2reg_500trees_3fea.RData")
pred2A2 = predict(modelo2, newdata = test[test$prediccionABC == "2A",])
pred2A = (pred2A1 + pred2A2)/2
test$prediccionPA[test$prediccionABC == "2A"] = pred2A
################################## REGRESION 2B ##########################################
load("~/cajamar/Code/ModeloRegresion/2B/modeloreg_500trees_3fea.RData")
pred2B = predict(modelo, newdata = test[test$prediccionABC == "2B",])
test$prediccionPA[test$prediccionABC == "2B"] = pred2B
################################## REGRESION 2C ##########################################
load("~/cajamar/Code/ModeloRegresion/2C/modeloreg_500trees_3fea.RData")
pred2C = predict(modelo, newdata = test[test$prediccionABC == "2C",])
test$prediccionPA[test$prediccionABC == "2C"] = pred2C
################################## REGRESION 3 ##########################################
load("~/cajamar/Code/ModeloRegresion/3/modeloreg_500trees_3fea.RData")
pred3 = predict(modelo, newdata = test[test$prediccion123 == 3,])
test$prediccionPA[test$prediccion123 == 3] = pred3

# Preparacion del Test_Mission.txt
# test = test[,c(1,95)]
# names(test) = c("ID_Customer","PA_Est")
# test$ID_Customer = as.character(test$ID_Customer)
# test$PA_Est = as.numeric(test$PA_Est)
# write_delim(test,"./cajamar/Test_Mission.txt",delim=",")

################################################## Métricas y evaluación del modelo ##################################################
confusionMatrix(test$prediccionABC, test$Mercado)
test$MercadoSimple = as.character(test$Mercado)
test$MercadoSimple[test$Mercado=="2A" | test$Mercado=="2B" | test$Mercado=="2C"] = "2"
test$MercadoSimple = as.factor(test$MercadoSimple)
confusionMatrix(test$prediccion123, test$MercadoSimple)
testeo = plot_ly(x = test$Poder_Adquisitivo,y = test$prediccionPA, type="scatter", mode="markers")

Error1 = sum(abs(test$prediccionPA[test$prediccionABC == "1"]-test$Poder_Adquisitivo[test$prediccionABC == "1"]))/length(test$prediccionPA[test$prediccionABC == "1"])
Error2A = sum(abs(test$prediccionPA[test$prediccionABC == "2A"]-test$Poder_Adquisitivo[test$prediccionABC == "2A"]))/length(test$prediccionPA[test$prediccionABC == "2A"])
Error2B = sum(abs(test$prediccionPA[test$prediccionABC == "2B"]-test$Poder_Adquisitivo[test$prediccionABC == "2B"]))/length(test$prediccionPA[test$prediccionABC == "2B"])
Error2C = sum(abs(test$prediccionPA[test$prediccionABC == "2C"]-test$Poder_Adquisitivo[test$prediccionABC == "2C"]))/length(test$prediccionPA[test$prediccionABC == "2C"])
Error3 = sum(abs(test$prediccionPA[test$prediccionABC == "3"]-test$Poder_Adquisitivo[test$prediccionABC == "3"]))/length(test$prediccionPA[test$prediccionABC == "3"])
err1 = Error1/mean(test$Poder_Adquisitivo[test$Mercado == "1"])*100
err2A = Error2A/mean(test$Poder_Adquisitivo[test$Mercado == "2A"])*100
err2B = Error2B/mean(test$Poder_Adquisitivo[test$Mercado == "2B"])*100
err2C = Error2C/mean(test$Poder_Adquisitivo[test$Mercado == "2C"])*100
err3 = Error3/mean(test$Poder_Adquisitivo[test$Mercado == "3"])*100

print(paste("error 2A =",err2A))
print(paste("error 2B =",err2B))
print(paste("error 2C =",err2C))
print(paste("error 1 =",err1))
print(paste("error 3 =",err3))

