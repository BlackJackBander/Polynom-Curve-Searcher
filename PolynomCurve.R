# Установка seed для воспроизводимости
set.seed(123)

# Функция для создания случайных точек
generate_random_points <- function(n = 5, x_range = c(-10, 10), y_range = c(-10, 10)) {
  x <- runif(n, x_range[1], x_range[2])
  y <- runif(n, y_range[1], y_range[2])
  data.frame(x = x, y = y)
}

# Функция для подбора полинома степени (n-1) для n точек
fit_polynomial <- function(points) {
  n <- nrow(points)
  degree <- n - 1  # Степень полинома на единицу меньше количества точек
  
  # Создаем матрицу признаков для полиномиальной регрессии
  poly_formula <- as.formula(paste("y ~ poly(x,", degree, ", raw = TRUE)"))
  model <- lm(poly_formula, data = points)
  
  return(model)
}

# Функция для вычисления ошибки (RMSE)
calculate_rmse <- function(model, points) {
  predicted <- predict(model, points)
  sqrt(mean((predicted - points$y)^2))
}

# Функция для получения уравнения полинома
get_polynomial_equation <- function(model) {
  coefs <- coef(model)
  degree <- length(coefs) - 1
  
  equation <- paste0("y = ", round(coefs[1], 3))
  
  for (i in 1:degree) {
    sign <- ifelse(coefs[i+1] >= 0, " + ", " - ")
    value <- abs(round(coefs[i+1], 3))
    if (i == 1) {
      equation <- paste0(equation, sign, value, "x")
    } else {
      equation <- paste0(equation, sign, value, "x^", i)
    }
  }
  
  return(equation)
}

# Функция для визуализации
visualize_fit <- function(points, model) {
  # Создаем данные для гладкой кривой
  x_curve <- seq(min(points$x) - 2, max(points$x) + 2, length.out = 100)
  curve_data <- data.frame(x = x_curve)
  curve_data$y <- predict(model, curve_data)
  
  # Вычисляем RMSE
  rmse <- calculate_rmse(model, points)
  
  # Визуализация
  library(ggplot2)
  
  ggplot() +
    geom_point(data = points, aes(x, y), color = "red", size = 3) +
    geom_line(data = curve_data, aes(x, y), color = "blue", size = 1) +
    labs(title = paste("Полиномиальная регрессия степени", length(coef(model)) - 1),
         subtitle = paste(get_polynomial_equation(model), "\nRMSE:", round(rmse, 4))) +
    theme_minimal()
}

# Основная программа
n_points <- 6  # Количество точек (можно изменить)
points <- generate_random_points(n_points)

# Подбираем полином
model <- fit_polynomial(points)

# Визуализируем результат
visualize_fit(points, model)

# Вывод информации
cat("Количество точек:", n_points, "\n")
cat("Степень полинома:", length(coef(model)) - 1, "\n")
cat("Уравнение:", get_polynomial_equation(model), "\n")
cat("RMSE:", calculate_rmse(model, points), "\n")
cat("\nКоэффициенты полинома:\n")
print(coef(model))
