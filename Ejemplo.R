# LIBRERIAS Y DATOS
# -----------------------------------------------------
library(MASS); library(neuralnet); library(ggplot2)
set.seed(65)
datos2    <- ("C://Users//V Tibs//Desktop//R//R/Pronostico Vta//Joyeria.csv ")
datos<- read.csv(datos2, header = TRUE)
names(datos) <- c("s1","s2","s3","s4","s5","s6","s7")
n        <- nrow(datos)
muestra  <- sample(n, n * .70)
train    <- datos[muestra, ]
test     <- datos[-muestra, ]


# NORMALIZACION DE VARIABLES
# -----------------------------------------------------
maxs      <- apply(train, 2, max)
mins      <- apply(train, 2, min)
datos_nrm <- as.data.frame(scale(datos, center = mins, scale = maxs - mins))
train_nrm <- datos_nrm[muestra, ]
test_nrm  <- datos_nrm[-muestra, ]


# FORMULA
# -----------------------------------------------------
nms  <- names(train_nrm)
frml <- as.formula(paste("s7 ~", paste(nms[!nms %in% "s7"], collapse = " + ")))


# MODELO
# -----------------------------------------------------
modelo.nn <- neuralnet(frml,
                       data          = train_nrm,
                       hidden        = c(8,7), # ver Notas para detalle 
                       threshold     = 0.05,   # ver Notas para detalle
                       algorithm     = "rprop+" 
)


# PREDICCION
# -----------------------------------------------------
pr.nn   <- compute(modelo.nn,within(test_nrm,rm(s7)))

# se transoforma el valor escalar al valor nominal original
s7.predict <- pr.nn$net.result*(max(datos$s7)-min(datos$s7))+min(datos$s7)
s7.real    <- (test_nrm$s7)*(max(datos$s7)-min(datos$s7))+min(datos$s7)



# SUMA DE ERROR CUADRATICO
# -----------------------------------------------------
se.nn <- sum((s7.real - s7.predict)^2)/nrow(test_nrm)


#GRAFICOS
# -----------------------------------------------------
# Errores
qplot(x=s7.real, y=s7.predict, geom=c("point","smooth"),method="lm")
# Red
plot(modelo.nn)