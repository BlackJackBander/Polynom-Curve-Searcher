library(quantmod)
library(ggplot2)
library(zoo)
library(splines)  # Добавляем библиотеку для сплайнов

# Получаем данные по акциям GOOG с начала 2024 года
getSymbols("GOOG", from = "2024-01-01", to = Sys.Date(), src = "yahoo")

# Извлекаем цены закрытия
close_prices <- Cl(GOOG)
colnames(close_prices) <- "Close"

# Находим локальные экстремумы (максимумы и минимумы)
find_peaks <- function(x, n) {
  # Убедимся, что x является числовым вектором
  x <- as.numeric(x)
  x_max <- rollapply(x, n, function(x) which.max(x) == ceiling(n/2), fill = NA)
  x_min <- rollapply(x, n, function(x) which.min(x) == ceiling(n/2), fill = NA)
  return(list(maxima = which(x_max), minima = which(x_min)))
}

extrema <- find_peaks(close_prices$Close, n = 15)

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

# Функция для подбора сплайна
fit_spline <- function(points, df = NULL, knots = NULL) {
  if (!is.null(df)) {
    # Натуральный сплайн с заданным количеством степеней свободы
    model <- lm(y ~ ns(x, df = df), data = points)
  } else if (!is.null(knots)) {
    # Сплайн с заданными узлами
    model <- lm(y ~ ns(x, knots = knots), data = points)
  } else {
    # Автоматический выбор степени свободы на основе количества точек
    n <- nrow(points)
    df_value <- min(floor(n/2), 6)  # Ограничиваем максимальную сложность
    model <- lm(y ~ ns(x, df = df_value), data = points)
  }
  return(model)
}

# Подбираем несколько моделей сплайнов для сравнения
model_spline_4 <- fit_spline(points_df, df = 4)  # Натуральный сплайн с 4 степенями свободы
model_spline_auto <- fit_spline(points_df)       # Автоматический подбор

# Создаем данные для предсказания
x_curve <- seq(min(points_df$x), max(points_df$x), length.out = 1000)

# Предсказания для разных моделей
predicted_spline_4 <- predict(model_spline_4, newdata = data.frame(x = x_curve))
predicted_spline_auto <- predict(model_spline_auto, newdata = data.frame(x = x_curve))

curve_data_spline_4 <- data.frame(x = x_curve, y = predicted_spline_4, Model = "Сплайн (df=4)")
curve_data_spline_auto <- data.frame(x = x_curve, y = predicted_spline_auto, Model = "Сплайн (авто)")

# Объединяем данные для визуализации
curve_data <- rbind(curve_data_spline_4, curve_data_spline_auto)

# Визуализация с сравнением сплайнов
ggplot() +
  geom_line(data = data.frame(x = as.numeric(index(close_prices)), 
                              y = as.numeric(close_prices$Close)), 
            aes(x = x, y = y), color = "gray50", alpha = 0.7, linewidth = 0.5) +
  geom_point(data = points_df, aes(x = x, y = y, color = type), size = 3, alpha = 0.8) +
  geom_line(data = curve_data, aes(x = x, y = y, color = Model), linewidth = 1.2, alpha = 0.8) +
  labs(title = "Сплайновая регрессия для экстремумов цен GOOG",
       subtitle = paste("Точек экстремумов:", nrow(points_df)),
       x = "Время (числовое представление)",
       y = "Цена закрытия (USD)",
       color = "Легенда") +
  theme_minimal() +
  scale_color_manual(values = c("Maximum" = "red", "Minimum" = "green", 
                                "Сплайн (df=4)" = "blue", "Сплайн (авто)" = "purple")) +
  theme(legend.position = "bottom")

# Выводим информацию о моделях
cat("=== ИНФОРМАЦИЯ О МОДЕЛЯХ СПЛАЙНОВ ===\n\n")

cat("Модель 1: Натуральный сплайн с 4 степенями свободы\n")
cat("Количество точек:", nrow(points_df), "\n")
cat("Степеней свободы:", 4, "\n")
cat("R-squared:", summary(model_spline_4)$r.squared, "\n")
cat("RMSE:", sqrt(mean((predict(model_spline_4) - points_df$y)^2)), "\n\n")

cat("Модель 2: Автоматический подбор сплайна\n")
cat("Количество точек:", nrow(points_df), "\n")
cat("Фактическое количество степеней свободы:", length(coefficients(model_spline_auto)) - 1, "\n")
cat("R-squared:", summary(model_spline_auto)$r.squared, "\n")
cat("RMSE:", sqrt(mean((predict(model_spline_auto) - points_df$y)^2)), "\n\n")

# Сравнительная таблица
comparison_df <- data.frame(
  Модель = c("Сплайн (df=4)", "Сплайн (авто)"),
  Точки = nrow(points_df),
  Степени_свободы = c(4, length(coefficients(model_spline_auto)) - 1),
  R2 = c(summary(model_spline_4)$r.squared, summary(model_spline_auto)$r.squared),
  RMSE = c(sqrt(mean((predict(model_spline_4) - points_df$y)^2)), 
           sqrt(mean((predict(model_spline_auto) - points_df$y)^2))
  )
)

cat("Сравнительная таблица:\n")
print(comparison_df)

# Показываем первые несколько точек
cat("\nПервые 5 экстремумов:\n")
print(head(points_df, 5))

# Дополнительная диагностика
cat("\n=== ДИАГНОСТИКА ЛУЧШЕЙ МОДЕЛИ ===\n")
if (comparison_df$RMSE[1] < comparison_df$RMSE[2]) {
  best_model <- model_spline_4
  cat("Лучшая модель: Сплайн с 4 степенями свободы\n")
} else {
  best_model <- model_spline_auto
  cat("Лучшая модель: Автоматический сплайн\n")
}

cat("Коэффициенты лучшей модели:\n")
print(coef(best_model))
cat("\nУзлы сплайна (если применимо):\n")
# Для натуральных сплайнов можно получить информацию об узлах
if ("ns" %in% attr(terms(best_model), "term.labels")) {
  spline_info <- attributes(ns(points_df$x, df = length(coef(best_model)) - 1))
  print(spline_info$knots)
}
