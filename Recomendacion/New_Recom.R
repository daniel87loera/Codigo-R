library(tidyverse)
library(recommenderlab)
data("MovieLense")

## No es posible pasar de realRatingMatrix a dataframe directamente.
# Se extraen las valoraciones de las películas y se almacenan en formato matriz.
# Cada fila de la matriz contiene la información de un usuario y cada columna la 
# información de una película.
valoraciones <- as(MovieLense, "matrix")

# Se convierte la matriz a dataframe
valoraciones <- as.data.frame(valoraciones)
valoraciones <- valoraciones %>% rownames_to_column(var = "usuario")

# Datos descriptivos de las películas
atributos <- MovieLenseMeta

# Se reestructuran los datos para que tengan un formato tidy
valoraciones_tidy <- valoraciones %>% gather(key = "pelicula",
                                             value = "valoracion",
                                             -usuario)
head(valoraciones_tidy)

valoraciones_tidy %>% filter(!is.na(valoracion)) %>%
  ggplot(aes(x = usuario, y = pelicula, fill = valoracion)) +
  geom_tile(color = "black") +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")

# Se cuenta el número de NA por columna del dataframe y con la función reduce
# se suman todos los resultados. La columna usuario se excluye del contaje.
total_NA <- valoraciones %>% select(-usuario) %>%
  map_dbl(.f = function(x){ sum(is.na(x))}) %>%
  reduce(.f = sum)
total_elementos <- (ncol(valoraciones) - 1) * (nrow(valoraciones))
porcentaje_NA   <- 100 * (total_NA / total_elementos)
porcentaje_NA

valoraciones_tidy %>% filter(!is.na(valoracion)) %>%
  group_by(usuario) %>% count() %>%
  pull(n) %>% median()

valoraciones_tidy %>% filter(!is.na(valoracion)) %>% select(valoracion) %>% group_by(valoracion) %>% count() %>%
  ggplot(aes(x = valoracion, y = n)) +
  geom_col(color = "black") +
  theme_bw()

glimpse(atributos)

atributos %>% select(year) %>% group_by(year) %>% count() %>% 
  ggplot(aes(x =  as.factor(year), y = n)) +
  geom_col() +
  theme_bw() +
  labs(x = "year") +
  theme(axis.text.x = element_text(angle = 90, size = 6))

atributos %>% select(-title, -url, -year) %>%
  gather(key = "variable", value = "valor") %>%
  filter(valor != 0) %>% group_by(variable) %>%
  count() %>% 
  ggplot(aes(x =  reorder(variable, desc(n)), y = n)) +
  geom_col() +
  theme_bw() +
  labs(x = "temática") +
  theme(axis.text.x = element_text(angle = 90))

##########################################################################################################################

# Identificación de las películas vistas y no vistas por el usuario 329.
# Se asume que si la película no ha sido valorada es que no ha sido vista.
peliculas_vistas <- valoraciones_tidy %>%
  filter(usuario == 329 & !is.na(valoracion)) %>%
  pull(pelicula)

peliculas_no_vistas <- valoraciones_tidy %>%
  filter(usuario == 329 & is.na(valoracion)) %>%
  pull(pelicula)

# Se calcula la similitud entre cada película no valorada y las sí valoradas.
# Se genera un grid con todas las comparaciones que se tienen que realizar
comparaciones <- expand.grid(peliculas_no_vistas, peliculas_vistas,
                             stringsAsFactors = FALSE)
colnames(comparaciones) <- c("pelicula_no_vista", "pelicula_vista")

# Cuando un cálculo implica múltiples pares de vectores, suele ser práctico
# almacenar los datos en forma de matriz o dataframe donde cada vector es una
# columna. Se crea un dataframe en el que cada columna es una película.

atributos <- atributos %>% gather(key = "atributo", value = "valor", -title) %>%
  spread(key = title, value = valor)

# Se define la función que calcula la similitud
indice_jaccard <- function(pelicula1, pelicula2, datos) {
  # Esta función calcula el índice jaccard entre dos columnas de un dataframe.
  # El valor 1 indica presencia y el valor 0 ausencia.
  m11 <- sum(datos[, pelicula1] == 1 & datos[, pelicula2] == 1)
  m10 <- sum(datos[, pelicula1] == 1 & datos[, pelicula2] == 0)
  m01 <- sum(datos[, pelicula1] == 0 & datos[, pelicula2] == 1)
  indice <- m11 / sum(m01 + m10 + m11)
  return(indice)
}

# Con la función map2 del paquete purrr, se aplica la función indice_jaccard
# empleando las columnas del grid comparaciones como valores de los argumentos
# pelicula1 y pelicula2.
recomendaciones <- comparaciones[] %>%
  mutate(similitud = map2_dbl(.x = pelicula_no_vista,
                              .y = pelicula_vista,
                              .f = indice_jaccard,
                              datos = atributos))

# Para cada película no vista, se filtran las 15 películas más parecidas
recomendaciones <- recomendaciones %>% group_by(pelicula_no_vista) %>%
  top_n(n = 15, wt = similitud) %>%
  arrange(pelicula_no_vista, desc(similitud))


# Se añade la valoración que el usuario 329 ha hecho de cada una de las películas
valoraciones_u329 <- valoraciones_tidy %>%
  filter(usuario == 329 & !is.na(valoracion))
recomendaciones <- recomendaciones %>%
  left_join(y = valoraciones_u329,
            by = c("pelicula_vista"  = "pelicula"))

# Media ponderada de las valoraciones por película
media_ponderada <- function(df){
  resultado <- sum(df$valoracion * df$similitud) / sum(df$similitud)
  return(resultado)
}

top10_recomendaciones <- recomendaciones %>% group_by(pelicula_no_vista) %>%
  nest() %>%
  mutate(prediccion = map_dbl(.x = data,
                              .f = media_ponderada)) %>%
  select(-data) %>% arrange(desc(prediccion)) %>% head(10)
top10_recomendaciones
######################################################################################################
########################## FILTRADO COLABORATIVO #####################################################
#####################################################################################################
usuarios_excluidos <- valoraciones_tidy %>% filter(!is.na(valoracion)) %>%
  group_by(usuario) %>% count() %>% filter(n < 30) %>%
  pull(usuario)
valoraciones_tidy <- valoraciones_tidy %>% filter(!usuario %in% usuarios_excluidos)

# Se crea un dataframe en el que cada columna representa las valoraciones de 
# un usuario.
valoraciones_usuarios <- valoraciones_tidy %>%
  spread(key = usuario, value = valoracion, fill = NA)

# Función que calcula la similitud entre dos columnas
funcion_correlacion <- function(x, y){
  correlacion <- cor(x, y, use = "na.or.complete", method = "pearson")
  return(correlacion)
}

# Se aplica la función de correlación a cada columna de valoraciones_usuarios,
# empelando como argumento "y" la columna del usuario "329"
similitud_usuarios <- map_dbl(.x = valoraciones_usuarios[, -1],
                              .f = funcion_correlacion,
                              y = valoraciones_usuarios[, "329"])
similitud_usuarios <- data_frame(usuario   = names(similitud_usuarios),
                                 similitud = similitud_usuarios) %>%
  arrange(desc(similitud))
head(similitud_usuarios)
#######################################################################################################3
# Identificación de las películas vistas y no vistas por el usuario 329.
# Se asume que si la película no ha sido valorada por el usuario 329 es que no
# ha sido vista.
peliculas_vistas <- valoraciones_tidy %>%
  filter(usuario == 329 & !is.na(valoracion)) %>%
  pull(pelicula)

peliculas_no_vistas <- valoraciones_tidy %>%
  filter(usuario == 329 & is.na(valoracion)) %>%
  pull(pelicula)

# Se inicia un bucle para predecir la valoración que el usuario 329 hará de cada
# una de las películas no vistas.

prediccion <- rep(NA, length(peliculas_no_vistas))
pelicula   <- rep(NA, length(peliculas_no_vistas))
n_obs_prediccion <- rep(NA, length(peliculas_no_vistas))

for(i in seq_along(peliculas_no_vistas)){
  # Usuarios que han visto la película i
  usuarios_pelicula_i <- valoraciones_tidy %>%
    filter(pelicula == peliculas_no_vistas[i] & 
             !is.na(valoracion)) %>% pull(usuario)
  # Si no hay un mínimo de usuarios que han visto la película, no se considera una
  # estimación suficientemente buena por lo que se pasa a la siguiente película.
  if (length(usuarios_pelicula_i) < 10){
    next()
  }
  # Los 15 usuarios más parecidos de entre los que han visto la película i, cuya
  # similitud es >= 0.
  top_15_usuarios <- similitud_usuarios %>%
    filter(similitud >= 0 & (usuario %in% usuarios_pelicula_i)) %>%
    arrange(desc(similitud)) %>% 
    head(15) 
  # Si no hay un mínimo de usuarios con valoraciones válidas, no se considera una
  # estimación suficientemente buena por lo que se pasa a la siguiente película.
  if (nrow(top_15_usuarios) < 10){
    next()
  }
  
  # Valoraciones de esos 15 usuarios sobre la película i
  valoraciones_top_15 <- valoraciones_tidy %>%
    filter(pelicula == peliculas_no_vistas[i] &
             usuario %in% top_15_usuarios$usuario)
  
  # Media ponderada de las valoraciones de los top_15_usuarios
  top_15_usuarios <- top_15_usuarios %>% left_join(valoraciones_top_15,
                                                   by = "usuario")
  prediccion[i] <- sum(top_15_usuarios$similitud * top_15_usuarios$valoracion) /
    sum(top_15_usuarios$similitud)
  pelicula[i] <- peliculas_no_vistas[i]
  n_obs_prediccion[i] <- nrow(top_15_usuarios)
}

top10_recomendaciones <- data.frame(pelicula, prediccion, n_obs_prediccion) %>% 
  arrange(desc(prediccion)) %>%
  head(10)
top10_recomendaciones

###################################################################################
############## FILTRADO COLABORATIVO BASADO EN ITEMS ##############################
###################################################################################
valoraciones_tidy <- valoraciones %>% gather(key = "pelicula",
                                             value = "valoracion",
                                             -usuario) %>%
  group_by(usuario) %>% 
  mutate(valoracion = scale(valoracion)) %>%
  ungroup()
peliculas_excluidas <- valoraciones_tidy %>% filter(!is.na(valoracion)) %>%
  group_by(pelicula) %>% count() %>% filter(n < 5) %>%
  pull(pelicula)
valoraciones_tidy <- valoraciones_tidy %>%
  filter(!pelicula %in% peliculas_excluidas)

# Identificación de las películas vistas y no vistas por el usuario 329.
# Se asume que si la película no ha sido valorada es que no ha sido vista.
peliculas_vistas <- valoraciones_tidy %>%
  filter(usuario == 329 & !is.na(valoracion)) %>%
  pull(pelicula)

peliculas_no_vistas <- valoraciones_tidy %>%
  filter(usuario == 329 & is.na(valoracion)) %>%
  pull(pelicula)

# Se genera un grid con todas las comparaciones que se tienen que realizar
comparaciones <- expand.grid(peliculas_no_vistas, peliculas_vistas,
                             stringsAsFactors = FALSE)
colnames(comparaciones) <- c("pelicula_no_vista", "pelicula_vista")

# Se crea un dataframe en el que cada columna es una película
valoraciones <- valoraciones_tidy %>%
  spread(key = pelicula, value = valoracion, fill = NA)

# Se define la función que calcula la similitud
correlacion <- function(pelicula1, pelicula2, datos) {
  # Esta función calcula la correlación entre dos columnas de un dataframe.
  similitud <- cor(x = datos[, pelicula1], y = datos[, pelicula2],
                   method = "pearson", use = "na.or.complete")
  return(similitud)
}

# Con la función map2 del paquete purrr, se aplica la función correlación empleando
# las columnas del grid comparaciones como valores de los argumentos pelicula1 y
# pelicula2.

comparaciones <- comparaciones %>%
  mutate(similitud = map2_dbl(.x = pelicula_no_vista,
                              .y = pelicula_vista,
                              .f = correlacion,
                              datos = valoraciones))

# Para cada película no vista, se filtran las 15 películas más parecidas y cuyo 
# valor de similitud es mayor o igual a cero.
comparaciones <- comparaciones %>% filter(similitud >= 0) %>%
  group_by(pelicula_no_vista) %>%
  top_n(n = 15, wt = similitud) %>%
  arrange(pelicula_no_vista, desc(similitud))

# Se eliminan aquellas películas para las que no haya un mínimo de películas 
# similares con valores positivos.
exclusion <- comparaciones %>%
  group_by(pelicula_no_vista) %>%
  count() %>%
  filter(n < 10) %>%
  pull(pelicula_no_vista)
comparaciones <- comparaciones %>% filter(!pelicula_no_vista %in% exclusion)

# Se añade la valoración que el usuario 329 ha hecho de cada una de las películas.
valoraciones_u329 <- valoraciones_tidy %>%
  filter(usuario == 329 & !is.na(valoracion))
comparaciones <-  comparaciones %>%
  left_join(y = valoraciones_u329,
            by = c("pelicula_vista"  = "pelicula"))

# Media ponderada de las valoraciones por película
media_ponderada <- function(df){
  resultado <- sum(df$valoracion * df$similitud) / sum(df$similitud)
  return(resultado)
}

top10_recomendaciones <- comparaciones %>% group_by(pelicula_no_vista) %>%
  nest() %>%
  mutate(prediccion = map_dbl(.x = data,
                              .f = media_ponderada)) %>%
  select(-data) %>% arrange(desc(prediccion)) %>% head(10)
top10_recomendaciones


ggplot(data = top10_recomendaciones,
                            aes(x = reorder(pelicula_no_vista, prediccion), y = prediccion)) +
  geom_col() +
  coord_flip() +
  labs(x = "película recomendada") +
  theme_bw()



