library(tidyverse)
library(recommenderlab)
data("MovieLense")

## No es posible pasar de realRatingMatrix a dataframe directamente.
# Se extraen las valoraciones de las pel�culas y se almacenan en formato matriz.
# Cada fila de la matriz contiene la informaci�n de un usuario y cada columna la 
# informaci�n de una pel�cula.
valoraciones <- as(MovieLense, "matrix")

# Se convierte la matriz a dataframe
valoraciones <- as.data.frame(valoraciones)
valoraciones <- valoraciones %>% rownames_to_column(var = "usuario")

# Datos descriptivos de las pel�culas
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

# Se cuenta el n�mero de NA por columna del dataframe y con la funci�n reduce
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
  labs(x = "tem�tica") +
  theme(axis.text.x = element_text(angle = 90))

##########################################################################################################################

# Identificaci�n de las pel�culas vistas y no vistas por el usuario 329.
# Se asume que si la pel�cula no ha sido valorada es que no ha sido vista.
peliculas_vistas <- valoraciones_tidy %>%
  filter(usuario == 329 & !is.na(valoracion)) %>%
  pull(pelicula)

peliculas_no_vistas <- valoraciones_tidy %>%
  filter(usuario == 329 & is.na(valoracion)) %>%
  pull(pelicula)

# Se calcula la similitud entre cada pel�cula no valorada y las s� valoradas.
# Se genera un grid con todas las comparaciones que se tienen que realizar
comparaciones <- expand.grid(peliculas_no_vistas, peliculas_vistas,
                             stringsAsFactors = FALSE)
colnames(comparaciones) <- c("pelicula_no_vista", "pelicula_vista")

# Cuando un c�lculo implica m�ltiples pares de vectores, suele ser pr�ctico
# almacenar los datos en forma de matriz o dataframe donde cada vector es una
# columna. Se crea un dataframe en el que cada columna es una pel�cula.

atributos <- atributos %>% gather(key = "atributo", value = "valor", -title) %>%
  spread(key = title, value = valor)

# Se define la funci�n que calcula la similitud
indice_jaccard <- function(pelicula1, pelicula2, datos) {
  # Esta funci�n calcula el �ndice jaccard entre dos columnas de un dataframe.
  # El valor 1 indica presencia y el valor 0 ausencia.
  m11 <- sum(datos[, pelicula1] == 1 & datos[, pelicula2] == 1)
  m10 <- sum(datos[, pelicula1] == 1 & datos[, pelicula2] == 0)
  m01 <- sum(datos[, pelicula1] == 0 & datos[, pelicula2] == 1)
  indice <- m11 / sum(m01 + m10 + m11)
  return(indice)
}

# Con la funci�n map2 del paquete purrr, se aplica la funci�n indice_jaccard
# empleando las columnas del grid comparaciones como valores de los argumentos
# pelicula1 y pelicula2.
recomendaciones <- comparaciones[] %>%
  mutate(similitud = map2_dbl(.x = pelicula_no_vista,
                              .y = pelicula_vista,
                              .f = indice_jaccard,
                              datos = atributos))

# Para cada pel�cula no vista, se filtran las 15 pel�culas m�s parecidas
recomendaciones <- recomendaciones %>% group_by(pelicula_no_vista) %>%
  top_n(n = 15, wt = similitud) %>%
  arrange(pelicula_no_vista, desc(similitud))


# Se a�ade la valoraci�n que el usuario 329 ha hecho de cada una de las pel�culas
valoraciones_u329 <- valoraciones_tidy %>%
  filter(usuario == 329 & !is.na(valoracion))
recomendaciones <- recomendaciones %>%
  left_join(y = valoraciones_u329,
            by = c("pelicula_vista"  = "pelicula"))

# Media ponderada de las valoraciones por pel�cula
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

# Funci�n que calcula la similitud entre dos columnas
funcion_correlacion <- function(x, y){
  correlacion <- cor(x, y, use = "na.or.complete", method = "pearson")
  return(correlacion)
}

# Se aplica la funci�n de correlaci�n a cada columna de valoraciones_usuarios,
# empelando como argumento "y" la columna del usuario "329"
similitud_usuarios <- map_dbl(.x = valoraciones_usuarios[, -1],
                              .f = funcion_correlacion,
                              y = valoraciones_usuarios[, "329"])
similitud_usuarios <- data_frame(usuario   = names(similitud_usuarios),
                                 similitud = similitud_usuarios) %>%
  arrange(desc(similitud))
head(similitud_usuarios)
#######################################################################################################3
# Identificaci�n de las pel�culas vistas y no vistas por el usuario 329.
# Se asume que si la pel�cula no ha sido valorada por el usuario 329 es que no
# ha sido vista.
peliculas_vistas <- valoraciones_tidy %>%
  filter(usuario == 329 & !is.na(valoracion)) %>%
  pull(pelicula)

peliculas_no_vistas <- valoraciones_tidy %>%
  filter(usuario == 329 & is.na(valoracion)) %>%
  pull(pelicula)

# Se inicia un bucle para predecir la valoraci�n que el usuario 329 har� de cada
# una de las pel�culas no vistas.

prediccion <- rep(NA, length(peliculas_no_vistas))
pelicula   <- rep(NA, length(peliculas_no_vistas))
n_obs_prediccion <- rep(NA, length(peliculas_no_vistas))

for(i in seq_along(peliculas_no_vistas)){
  # Usuarios que han visto la pel�cula i
  usuarios_pelicula_i <- valoraciones_tidy %>%
    filter(pelicula == peliculas_no_vistas[i] & 
             !is.na(valoracion)) %>% pull(usuario)
  # Si no hay un m�nimo de usuarios que han visto la pel�cula, no se considera una
  # estimaci�n suficientemente buena por lo que se pasa a la siguiente pel�cula.
  if (length(usuarios_pelicula_i) < 10){
    next()
  }
  # Los 15 usuarios m�s parecidos de entre los que han visto la pel�cula i, cuya
  # similitud es >= 0.
  top_15_usuarios <- similitud_usuarios %>%
    filter(similitud >= 0 & (usuario %in% usuarios_pelicula_i)) %>%
    arrange(desc(similitud)) %>% 
    head(15) 
  # Si no hay un m�nimo de usuarios con valoraciones v�lidas, no se considera una
  # estimaci�n suficientemente buena por lo que se pasa a la siguiente pel�cula.
  if (nrow(top_15_usuarios) < 10){
    next()
  }
  
  # Valoraciones de esos 15 usuarios sobre la pel�cula i
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

# Identificaci�n de las pel�culas vistas y no vistas por el usuario 329.
# Se asume que si la pel�cula no ha sido valorada es que no ha sido vista.
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

# Se crea un dataframe en el que cada columna es una pel�cula
valoraciones <- valoraciones_tidy %>%
  spread(key = pelicula, value = valoracion, fill = NA)

# Se define la funci�n que calcula la similitud
correlacion <- function(pelicula1, pelicula2, datos) {
  # Esta funci�n calcula la correlaci�n entre dos columnas de un dataframe.
  similitud <- cor(x = datos[, pelicula1], y = datos[, pelicula2],
                   method = "pearson", use = "na.or.complete")
  return(similitud)
}

# Con la funci�n map2 del paquete purrr, se aplica la funci�n correlaci�n empleando
# las columnas del grid comparaciones como valores de los argumentos pelicula1 y
# pelicula2.

comparaciones <- comparaciones %>%
  mutate(similitud = map2_dbl(.x = pelicula_no_vista,
                              .y = pelicula_vista,
                              .f = correlacion,
                              datos = valoraciones))

# Para cada pel�cula no vista, se filtran las 15 pel�culas m�s parecidas y cuyo 
# valor de similitud es mayor o igual a cero.
comparaciones <- comparaciones %>% filter(similitud >= 0) %>%
  group_by(pelicula_no_vista) %>%
  top_n(n = 15, wt = similitud) %>%
  arrange(pelicula_no_vista, desc(similitud))

# Se eliminan aquellas pel�culas para las que no haya un m�nimo de pel�culas 
# similares con valores positivos.
exclusion <- comparaciones %>%
  group_by(pelicula_no_vista) %>%
  count() %>%
  filter(n < 10) %>%
  pull(pelicula_no_vista)
comparaciones <- comparaciones %>% filter(!pelicula_no_vista %in% exclusion)

# Se a�ade la valoraci�n que el usuario 329 ha hecho de cada una de las pel�culas.
valoraciones_u329 <- valoraciones_tidy %>%
  filter(usuario == 329 & !is.na(valoracion))
comparaciones <-  comparaciones %>%
  left_join(y = valoraciones_u329,
            by = c("pelicula_vista"  = "pelicula"))

# Media ponderada de las valoraciones por pel�cula
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
  labs(x = "pel�cula recomendada") +
  theme_bw()



