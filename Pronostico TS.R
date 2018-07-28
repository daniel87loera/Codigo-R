#### PRONOSTICO TS

Fts <-  ts(FerrTS,start = c(2016,1),end= c(2018,26),frequency = 52)
str(decompose(Fts))
plot(decompose(Fts))
l