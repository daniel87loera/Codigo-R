#############################################################
########### CONECTOR A BD    ################################
#############################################################

#library("RODBC")
#odbcChannel <- odbcConnect("DataClasificacion", uid = "sa", pwd = "Tibs2016")
#ABA_NivelPoliza <- sqlFetch(odbcChannel, "Info_Aba_NivelPoliza")


#############################################################
###################### LECTOR DE ARCHIVO ####################
#############################################################
ABA_NivelPoliza <- ("C:\\Users\\V Tibs\\Desktop\\R\\R\\Clasif_CANC\\ABA_NivelPoliza.csv")
ABA_NP <- read.csv(ABA_NivelPoliza,header = TRUE)
#names(ABA_NivelPoliza)<- c("CvePoliza","CvePolizaPrevia","InVigPoliza", "FinVigPoliza","Producto","TipoEmision",
#                           "TiendaDanos","SucursalDanos","OfVntDanos", "TipoProducto","PrimaNetaPesos","SumaAsegurada", "EstPoliza")

############################################################
############ ENTRENAMIENTO DE RED  Y TESTEO DE #############
##################TEOREMA DE BAYES #########################
############################################################

library(e1071)
# Selección de una submuestra de 105 (el 70% de los datos)
set.seed(101)
ABA.indices <- sample(1:nrow(ABA_NP), nrow(ABA_NP) * 0.7)
ABA.entrenamiento <- ABA_NP[ABA.indices,]
ABA.test <- ABA_NP[-ABA.indices,]

model <- naiveBayes(EstPoliza ~ ., data = ABA.entrenamiento)

# Importancia de cada variable
#model$importance

##################################################################
####################### PREDICCION ###############################
##################################################################
# predict necesita el parámetro newdata
results <- predict(object = model, newdata = ABA.test, type = "raw")
results2 <- predict(object = model, ABA.test, type = "class")

mc <- table(results2, ABA.test$EstPoliza)
df<- data.frame(mc)
Porcentaje <- round(results*100)

# Correctamente clasificados
x <- 100 * sum(diag(mc)) / sum(mc)

#Agregar Columna de Prediccion a la tabla ABA.test
ABA.test$PrediClas <- Porcentaje
ABA.test$PClas <- results2
#write.csv(ABA.test, "Clasif_Canc.csv")