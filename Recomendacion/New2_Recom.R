library(tidyverse)
library(recommenderlab)
#data("MovieLense")
# Se convierte la matriz a dataframe
# valoraciones de 10 a 1
ww<- ("C:\\Users\\V Tibs\\Desktop\\Valoracion_Prod2.xlsx")
Valoracion_Prod <- read_excel(ww)
valoraciones <- as.data.frame(Valoracion_Prod)
class(valoraciones)
valoraciones <- valoraciones %>% rownames_to_column(var = "Giro")
# Datos descriptivos de las películas
# atributos 0 y 1 
atri <-("C:\\Users\\V Tibs\\Desktop\\Recom_Prod2.xlsx")
at <- read_excel(atri)

atributos <- as.data.frame(at)
#atributos <- atributos %>% rownames_to_column(var = "Producto")

class(atributos)

#############################################################
############ BASADO EN ITEMS ###############################

# Se reestructuran los datos para que tengan un formato tidy
valoraciones_tidy <- valoraciones %>% gather(key = "Producto",
                                             value = "valoracion",
                                             -Giro) %>%
  group_by(Giro) %>% 
  mutate(valoracion = scale(valoracion)) %>%
  ungroup()
Productos_excluidos <- valoraciones_tidy %>% filter(!is.na(valoracion)) %>%
  group_by(Producto) %>% count() %>% filter(n < 5) %>%
  pull(Producto)
valoraciones_tidy <- valoraciones_tidy %>%
  filter(!Producto %in% Productos_excluidos)


# Identificación de las películas vistas y no vistas por el usuario 329.
# Se asume que si la película no ha sido valorada es que no ha sido vista.
Productos_Contratados <- valoraciones_tidy %>%
  filter(Giro == 329 & !is.na(valoracion)) %>%
  pull(Producto)

Productos_no_Contratados <- valoraciones_tidy %>%
  filter(Giro == 329 & is.na(valoracion)) %>%
  pull(Producto)

# Se genera un grid con todas las comparaciones que se tienen que realizar
comparaciones <- expand.grid(Productos_no_Contratados, Productos_Contratados,
                             stringsAsFactors = FALSE)
colnames(comparaciones) <- c("Productos_no_Contratados", "Productos_Contratados")

# Se crea un dataframe en el que cada columna es una película
valoraciones <- valoraciones_tidy %>%
  spread(key = Producto, value = valoracion, fill = NA)

# Se define la función que calcula la similitud
correlacion <- function(Producto1, Producto2, datos) {
  # Esta función calcula la correlación entre dos columnas de un dataframe.
  similitud <- cor(x = datos[, Producto1], y = datos[, Producto2],
                   method = "pearson", use = "na.or.complete")
  return(similitud)
}

# Con la función map2 del paquete purrr, se aplica la función correlación empleando
# las columnas del grid comparaciones como valores de los argumentos pelicula1 y
# pelicula2.

comparaciones <- comparaciones %>%
  mutate(similitud = map2_dbl(.x = Productos_no_Contratados,
                              .y = Productos_Contratados,
                              .f = correlacion,
                              datos = valoraciones))

# Para cada película no vista, se filtran las 15 películas más parecidas y cuyo 
# valor de similitud es mayor o igual a cero.
comparaciones <- comparaciones %>% filter(similitud >= 0) %>%
  group_by(Productos_no_Contratados) %>%
  top_n(n = 15, wt = similitud) %>%
  arrange(Productos_no_Contratados, desc(similitud))

# Se eliminan aquellas películas para las que no haya un mínimo de películas 
# similares con valores positivos.
exclusion <- comparaciones %>%
  group_by(Productos_no_Contratados) %>%
  count() %>%
  filter(n < 10) %>%
  pull(Productos_no_Contratados)
comparaciones <- comparaciones %>% filter(!Productos_no_Contratados %in% exclusion)

# Se añade la valoración que el usuario 329 ha hecho de cada una de las películas.
valoraciones_u329 <- valoraciones_tidy %>%
  filter(Giro == 329 & !is.na(valoracion))
comparaciones <-  comparaciones %>%
  left_join(y = valoraciones_u329,
            by = c("Productos_Contratados"  = "Producto"))

# Media ponderada de las valoraciones por película
media_ponderada <- function(df){
  resultado <- sum(df$valoracion * df$similitud) / sum(df$similitud)
  return(resultado)
}

top10_recomendaciones <- comparaciones %>% group_by(Productos_Contratados) %>%
  nest() %>%
  mutate(prediccion = map_dbl(.x = data,
                              .f = media_ponderada)) %>%
  select(-data) %>% arrange(desc(prediccion)) %>% head(10)
top10_recomendaciones

  