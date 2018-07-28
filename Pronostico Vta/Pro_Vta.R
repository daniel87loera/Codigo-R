## ENTRADA DE DATOS 
instancia<- "C://Users//V Tibs//Desktop//R//R/Pronostico Vta//Joyeria.csv "  
inputTarget.grid <- read.csv(instancia,header= TRUE)
names(inputTarget.grid) <- c("m1","m2","m3","m4","m5","m6","m7")
##Encuentro el valor maximo en la instancia
maxValue=max(inputTarget.grid[,1:length(inputTarget.grid)-1])
##Se toman todas las columnas menos la ultima como entrada. 
##Se Toma  como objetivo la ultima columna.
##En ambas divido entre maxValue para normalizar.
input = inputTarget.grid[,1:length(inputTarget.grid)-1]/maxValue
target = inputTarget.grid[,length(inputTarget.grid)]/maxValue

#Se toma el 70% de los datos para entrenamiento. 
f <- round(nrow(inputTarget.grid)*0.7)

library(nnet)
## la capa oculta es dinamicamente.
#for(cao in 1:8)
#{
  producto.nnet = nnet(input[1:f,], target[1:f], size= 8,rang=1,maxit=10000,trace=FALSE)
  #Calculo el error
  diferencia=(target[1:f]-producto.nnet$fitted.values)^2
  i=0;error_ind=c()
  for(i in 1:f)
  {  
    error_ind[i]=sum(diferencia[i,])/8
  }
  error = sum(error_ind)/f
  x <- c(error)
  CoErr <- 100 - (error* 100)
  
  
  #La funcion predict utiliza los pesos obtenidos para calcular el pronostico
  #En este caso, enviamos todo input lo cual nos darÃ???a 
  #como resultado una tabla de 15 renglones (los primeros 12 son iguales a 
  #la variable productos.nnet$fitted.values, y los 3 rengones restantes 
  #serÃ???an el pronostico obtenido)
  output = predict(producto.nnet, input)
  
  #Esta grafica muestra compara la primer neurona target con predict
  
  plot(target,type="o", col= "blue")
  lines(output,type="o",col="red")
  
  View(output)
#}  
