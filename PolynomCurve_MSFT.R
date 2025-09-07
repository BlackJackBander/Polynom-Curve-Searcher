library(quantmod)
library(ggplot2)
library(zoo)

# Получаем данные по акциям MSFT с начала 2025 года
getSymbols("MSFT", from = "2025-01-01", to = Sys.Date(), src = "yahoo")

# Извлекаем цены закрытия
close_prices <- Cl(MSFT)
colnames(close_prices) <- "Close"

# Находим локальные экстремумы (максимумы и минимумы)
find_peaks <- function(x, n = 3) {
  # Убедимся, что x является числовым вектором
  x <- as.numeric(x)
  x_max <- rollapply(x, n, function(x) which.max(x) == ceiling(n/2), fill = NA)
  x_min <- rollapply(x, n, function(x) which.min(x) == ceiling(n/2), fill = NA)
  return(list(maxima = which(x_max), minima = which(x_min)))
}



extrema <- find_peaks(close_prices$Close, n = 5)
# Создаем отдельные наборы данных для максимумов и минимумов
maxima_df <- data.frame(
  x = as.numeric(index(close_prices[extrema$maxima, ])),
  y = as.numeric(close_prices[extrema$maxima, ]$Close),
  type = "Maximum"
)

minima_df <- data.frame(
  x = as.numeric(index(close_prices[extrema$minima, ])),
  y = as.numeric(close_prices[extrema$minima, ]$Close),
  type = "Minimum"
)

# Объединяем в один датафрейм
points_df <- rbind(maxima_df, minima_df)

# Сортируем по x (времени) для правильного порядка точек
points_df <- points_df[order(points_df$x), ]



# Подбираем полином степени (n-1) для n точек
fit_polynomial <- function(points) {
  n <- nrow(points)
  degree <- min(n - 1, 10)  # Ограничиваем степень полинома максимум 10
  model <- lm(y ~ poly(x, degree, raw = TRUE), data = points)
  return(model)
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

# Подбираем модель
model <- fit_polynomial(points_df)

# Создаем данные для предсказания
x_curve <- seq(min(points_df$x), max(points_df$x), length.out = 1000)
predicted <- predict(model, newdata = data.frame(x = x_curve))
curve_data <- data.frame(x = x_curve, y = predicted)

# Визуализация
ggplot() +
  geom_line(data = data.frame(x = as.numeric(index(close_prices)), 
                              y = as.numeric(close_prices$Close)), 
            aes(x = x, y = y), color = "gray50", alpha = 0.7) +
  geom_point(data = points_df, aes(x = x, y = y, color = type), size = 3) +
  geom_line(data = curve_data, aes(x = x, y = y), color = "blue", size = 1) +
  labs(title = "Полиномиальная регрессия для экстремумов цен MSFT (2025)",
       subtitle = paste(get_polynomial_equation(model),
                        "\nТочек:", nrow(points_df), 
                        "Степень полинома:", length(coef(model))-1),
       x = "Время (дни с начала периода)",
       y = "Цена закрытия (USD)",
       color = "Тип точки") +
  theme_minimal() +
  scale_color_manual(values = c("Maximum" = "red", "Minimum" = "green"))

# Выводим информацию
cat("Количество экстремумов:", nrow(points_df), "\n")
cat("Степень полинома:", length(coef(model))-1, "\n")
cat("Уравнение:", get_polynomial_equation(model), "\n")
cat("RMSE:", sqrt(mean((predict(model) - points_df$y)^2)), "\n")

# Показываем первые несколько точек
cat("\nПервые 5 экстремумов:\n")
print(head(points_df, 5))
