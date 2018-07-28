#Eighth code snippet
installations <- Recom

head(installations)
library('reshape')
user.package.matrix <- cast(installations, User~ Package, value = 'installed')
user.package.matrix[, 1]
# [1] 1 3 4 5 6 7 8 9 11 13 14 15 16 19 21 23 25 26 27 28 29 30 31 33 34
#[26] 35 36 37 40 41 42 43 44 45 46 47 48 49 50 51 54 55 56 57 58 59 60 61 62 63
#[51] 64 65

user.package.matrix[, 2]
# [1] 1 1 0 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 0 0 1 1 1 1 1 1 1 0 1 0 1 0 1 1 1 1 1 1
#[39] 1 1 1 1 1 1 1 1 0 1 1 1 1 1

row.names(user.package.matrix) <- user.package.matrix[, 1]

user.package.matrix <- user.package.matrix[, -1]

# Tenth code snippet
similarities <- cor(user.package.matrix)

nrow(similarities)
#[1] 2487
ncol(similarities)
#[1] 2487
similarities[1, 1]
#[1] 1
similarities[1, 2]
#[1] -0.04822428

# Eleventh code snippet
distances <- -log((similarities / 2) + 0.5)

# Twelfth code snippet
k.nearest.neighbors <- function(i, distances, k = 25) {
  return(order(distances[i,])[2:(k + 1)])
}

# Thirteenth code snippet
installation.probability <- function(user, package, user.package.matrix, distances, k = 25) {
  neighbors <- k.nearest.neighbors(package, distances, k = k)
  
  return(mean(sapply(neighbors, function(neighbor) { user.package.matrix[user, neighbor] })))
}

installation.probability(1, 1, user.package.matrix, distances)
#[1] 0.76

# Fourteenth code snippet
most.probable.packages <- function(user, user.package.matrix, distances, k = 25) {
  return(order(sapply(1:ncol(user.package.matrix),
                      function(package) {
                        installation.probability(user,
                                                 package,
                                                 user.package.matrix,
                                                 distances,
                                                 k = k)
                      }),
               decreasing = TRUE))
}

user <- 3

listing <- most.probable.packages(user, user.package.matrix, distances)

colnames(user.package.matrix)[listing[1:5]]