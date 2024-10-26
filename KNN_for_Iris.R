# Cargar librerías
library(ggplot2)
library(plotly)

# Cargar datos iris y establecer semilla para reproducibilidad
data(iris)
set.seed(123)  
training_indices <- sample(1:nrow(iris), size = 0.7 * nrow(iris))  # 70% para entrenamiento

# Dividir datos en entrenamiento y prueba
iris_training <- iris[training_indices, ]
iris_test <- iris[-training_indices, ]

# Filtrar solo las clases "versicolor" y "virginica"
iris_training <- subset(iris_training, Species %in% c("versicolor", "virginica"))
iris_test <- subset(iris_test, Species %in% c("versicolor", "virginica"))

# Calcular distancias entre datos
combined_data <- rbind(iris_training[, 1:4], iris_test[, 1:4])
distance_matrix <- as.matrix(dist(combined_data))

# Definir colores para las especies
species_colors <- c("versicolor" = "red", "virginica" = "green")

# Función k-NN para clasificar
k_nearest_neighbors <- function(k) {
  knn_indices <- apply(distance_matrix[1:nrow(iris_training), (nrow(iris_training) + 1):nrow(combined_data)], 2, order)  # Índices de k vecinos más cercanos
  
  # Predecir clases según mayoría
  predictions <- sapply(1:ncol(knn_indices), function(i) {
    neighbor_classes <- iris_training[knn_indices[1:k, i], 5]
    unique_classes <- unique(neighbor_classes)
    unique_classes[which.max(tabulate(match(neighbor_classes, unique_classes)))]  # Clase mayoritaria
  })
  
  # Calcular y mostrar TP y TN
  true_positives <- sum(predictions == "virginica" & iris_test[, 5] == "virginica")
  true_negatives <- sum(predictions == "versicolor" & iris_test[, 5] == "versicolor")
  cat("Predicciones:", predictions, "\n", "Reales:", iris_test[, 5], "\n", "Accuracy:", (true_positives + true_negatives) / nrow(iris_test), "\n")
  
  return(predictions)  # Retornar predicciones
}

# Función para graficar datos originales
plot_original_data <- function() {
  p1 <- plot_ly(data = iris[iris$Species %in% c("versicolor", "virginica"), ],
                x = ~Sepal.Length, y = ~Sepal.Width, z = ~Petal.Length,
                color = ~Species, colors = species_colors, type = "scatter3d", mode = "markers") %>%
    layout(title = "Datos Originales (Iris)",
           scene = list(xaxis = list(title = "Sepal Length"),
                        yaxis = list(title = "Sepal Width"),
                        zaxis = list(title = "Petal Length")))
  print(p1)  # Mostrar gráfico
}

# Función para graficar predicciones
plot_predictions <- function(predictions, k) {
  results <- data.frame(Sepal.Length = iris_test$Sepal.Length,
                        Sepal.Width = iris_test$Sepal.Width,
                        Petal.Length = iris_test$Petal.Length,
                        Predicciones = predictions,
                        Reales = iris_test$Species)
  
  p2 <- plot_ly(data = results,
                x = ~Sepal.Length, y = ~Sepal.Width, z = ~Petal.Length,
                color = ~Predicciones, colors = species_colors, type = "scatter3d", mode = "markers") %>%
    layout(title = paste("Predicciones k-NN (k =", k, ")"),
           scene = list(xaxis = list(title = "Sepal Length"),
                        yaxis = list(title = "Sepal Width"),
                        zaxis = list(title = "Petal Length"))) %>%
    add_markers(data = results, x = ~Sepal.Length, y = ~Sepal.Width, z = ~Petal.Length,
                marker = list(size = 3, opacity = 1, line = list(width = 3, color = 'black')),
                name = "Reales", color = ~Reales, colors = species_colors, showlegend = TRUE)
  
  print(p2)  # Mostrar gráfico de predicciones
}

# Establecer valor de k
k_value <- 5

# Ejecutar k-NN y graficar resultados
predicciones <- k_nearest_neighbors(k_value)
plot_original_data()
plot_predictions(predicciones, k_value)
