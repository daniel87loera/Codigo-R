## ENTRADA DE DATOS 
#RN <- Funcion(Giro){
instancia<- "C://Users//V Tibs//Desktop//R//R/Pronostico Vta//Joyeria.csv "  
inputTarget.grid <- read.csv(instancia,header= TRUE)
names(inputTarget.grid) <- c("m1","m2","m3","m4","m5","m6","m7")
##Encuentro el valor maximo en la instancia para normalizar los datos
maxValue=max(inputTarget.grid[,1:length(inputTarget.grid)-1])
##Se toman todas las columnas menos la ultima como entrada. 
##Se Toma  como objetivo la ultima columna.
##En ambas divido entre maxValue para normalizar.
input = inputTarget.grid[,1:length(inputTarget.grid)-1]/maxValue
entrada= inputTarget.grid[,length(inputTarget.grid)]
target = inputTarget.grid[,length(inputTarget.grid)]/maxValue

#Se toma el 70% de los datos para entrenamiento. 
f <- round(nrow(inputTarget.grid)*0.7)

library(nnet)
## la capa oculta es dinamicamente.
#for(cao in 1:8)
#{

  # Se realiza  el pronostico con 6 entredas  y la caoa oculta de 8
  producto.nnet = nnet(input[1:f,], target[1:f], size= 8,rang=1,maxit=10000,trace=FALSE)
  #Calculo el error y se realiza sumatoria
  diferencia=(target[1:f]-producto.nnet$fitted.values)^2
  i=0;error_ind=c()
  for(i in 1:f)
  {  
    error_ind[i]=sum(diferencia[i,])/8
  }
  error = sum(error_ind)/f
  x <- c(error)
  # se calcula  lo efectivo del pronostico
  CoErr <- 100 - (error* 100)
  
  
  #obtener el pronostico 
  output = predict(producto.nnet, input)
  # se multiplica el output por el valor maximo de normalizacion.
  salida<- (output * maxValue)
  
   # En esta grafica mostramos la salida en $$$$
   plot(salida,type = "o", col = "green")
 
  
#}  
