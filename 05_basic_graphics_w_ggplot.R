library(ggplot2)
library(gridExtra)  # для объединения графиков
df <- mtcars
mtcars$cyl <- as.factor(mtcars$cyl)

# Задание 1.1 - Гистограмма qsec
p1 <- ggplot(mtcars, aes(x = qsec)) +
  geom_histogram(fill = "#FFC0CB", color = "#CD919E", bins = 15) +
  labs(title = "Распределение qsec",
       subtitle = "Данные mtcars",
       x = "Время четверти мили (сек)",
       y = "Частота") +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

# Задание 1.2 - Плотность qsec
p2 <- ggplot(mtcars, aes(x = qsec)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Плотность распределения qsec",
       x = "qsec",
       y = "Плотность") +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))


# Задание 2 - Точечный график mpg от disp
p3 <- ggplot(mtcars, aes(x = disp, y = mpg, color = cyl, shape = cyl)) +
  geom_point(size = 2) +
  labs(title = "Зависимость расхода от объема",
       x = "Объем двигателя (куб. дюймы)",
       y = "Расход топлива (миль/галлон)") +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

# Задание 3 - Боксплоты + джиттер
p4 <- ggplot(mtcars, aes(x = cyl, y = mpg)) +
  geom_boxplot(fill = "lightgray", alpha = 0.7) +
  geom_jitter(width = 0.2, size = 2, color = "red", alpha = 0.7) +
  labs(title = "Расход топлива по количеству цилиндров",
       x = "Количество цилиндров",
       y = "Расход топлива (миль/галлон)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14))

# Задание 4 - Панель из 4 графиков
p5 <- ggplot(mtcars, aes(x = qsec)) +
  geom_histogram(fill = "#FFC0CB", color = "#CD919E", bins = 15) +
  labs(x = "Время четверти мили (сек)", y = "Частота") +
  theme_minimal() +
  annotate("text", x = min(mtcars$qsec), y = Inf, label = "A", 
           hjust = 0, vjust = 1.5, size = 6, fontface = "bold") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

p6 <- ggplot(mtcars, aes(x = qsec)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(x = "qsec", y = "Плотность") +
  theme_minimal() +
  annotate("text", x = min(mtcars$qsec), y = Inf, label = "B", 
           hjust = 0, vjust = 1.5, size = 6, fontface = "bold") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

p7 <- ggplot(mtcars, aes(x = disp, y = mpg, color = cyl, shape = cyl)) +
  geom_point(size = 2) +
  labs(x = "Объем двигателя (куб. дюймы)", 
       y = "Расход топлива (миль/галлон)") +
  theme_minimal() +
  annotate("text", x = min(mtcars$disp), y = max(mtcars$mpg), label = "C", 
           hjust = 0, vjust = 1.5, size = 6, fontface = "bold") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12))

p8 <- ggplot(mtcars, aes(x = cyl, y = mpg)) +
  geom_boxplot(fill = "lightgray", alpha = 0.7) +
  geom_jitter(width = 0.2, size = 2, color = "red", alpha = 0.7) +
  labs(x = "Количество цилиндров", y = "Расход топлива (миль/галлон)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  annotate("text", x = 1, y = max(mtcars$mpg), label = "D", 
           hjust = 0, vjust = 1.5, size = 6, fontface = "bold")

# Объединяем все графики
combined_plot <- grid.arrange(p5, p6, p7, p8, ncol = 2, nrow = 2)