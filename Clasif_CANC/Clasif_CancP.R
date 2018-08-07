
###################### LECTOR DE ARCHIVO ####################
ABA_NivelPoliza <- ("C:\\Users\\V Tibs\\Desktop\\R\\R\\Clasif_CANC\\ABA_NivelPoliza.csv")
ABA_NP <- read.csv(ABA_NivelPoliza,header = TRUE)

############ ENTRENAMIENTO DE RED  Y TESTEO DE #############
##################TEOREMA DE BAYES #########################
library(e1071)
# Selección de una submuestra el 70% de los datos
set.seed(101)
ABA.indices <- sample(1:nrow(ABA_NP), nrow(ABA_NP) * 0.7)
ABA.entrenamiento <- ABA_NP[ABA.indices,]
ABA.test <- ABA_NP[-ABA.indices,]
model <- naiveBayes(EstPoliza ~ ., data = ABA.entrenamiento)

####################### PREDICCION ###############################
results <- predict(object = model, newdata = ABA.test, type = "raw")
#results2 <- predict(object = model, ABA.test, type = "class")
#mc <- table(results,ABA.test$EstPoliza)
#mc
Porcentaje <- round(results*100)
# Correctamente clasificados

x <- 100 * sum(diag(mc)) / sum(mc)
#Agregar Columna de Prediccion a la tabla ABA.test
ABA.test$PrediClas <- Porcentaje
#ABA.test$PClas <- results2
#write.csv(ABA.test, "Clasif_Canc2.csv")

########### PRUEBA DE ARCHIVO RECIBIDO csv #####################
Prueba <- ("C:\\Users\\V Tibs\\Desktop\\Prueba.csv")
PruebaR <- read.csv(Prueba, header = TRUE)
PruebaP <- predict(object= model, newdata = PruebaR, type =  "raw")
prue <- PruebaP * 100

############## JSON ###########################################

library(rjson)
data = fromJSON(file ="Pruebajson.json")
Pruebajson <- as.data.frame(data)
print(Pruebajson)
Prueba2<- predict(object = model, newdata = Pruebajson, type = "raw")
PorcentajeP <- Prueba2 * 100
################ VECTOR #######################################





