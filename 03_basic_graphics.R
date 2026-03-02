# Задание 1
# Для датасета mtcars визуализировать:
#  • распределение qsec в виде гладкой линии и гистограммы (hist, plot-density-polygon)
#• для гистограммы: подписать имена оси x, заголовок, подзаголовок, заполнить цветом 
# гистограмму, задать другим цветом границы столбцов гистограммы. Увеличить размер шрифтов 
# для осей в 2 раза (подписи и тики). 
#• точечный график, на котором будет отображаться зависимость mpg от disp (plot). 
# Задать свой цвет, форму элементов для каждого значения cyl. Размер элементов должен быть равен 2. 
#• боксплоты+джиттерплоты mpg для каждого значения cyl(три боксплота на 1 графике, 
# джиттерплоты наложены на боксплоты). Надписи по оси абсцисс развернуть под углом 90 градусов.
df <- mtcars

# 1. Распределение qsec
# Гистограмма
png("iogen_course/гистограмма_qsec.png",
    width = 1000, 
    height = 500,
    units = "px"
    )
hist(mtcars$qsec, 
     col = "#FFC0CB",
     border = "#CD919E",
     main = "Распределение qsec",
     sub = "Данные mtcars",
     xlab = "Время четверти мили (сек)",
     cex.axis = 2, # размер шрифта тиков оси (x2)
     cex.lab = 2)# размер подписей осей (x2)
dev.off()

# Плотность распределения
png("iogen_course/плотность_qsec.png",
    width = 1000, 
    height = 500,
    units = "px"
)
plot(density(mtcars$qsec), 
     main = "Плотность распределения qsec",
     xlab = "qsec")
dev.off()

# 2. Точечный график mpg от disp с цветом по cyl

png("iogen_course/2.png",
    width = 1000, 
    height = 500,
    units = "px"
)
plot(mtcars$disp, mtcars$mpg,
     col = mtcars$cyl,
     pch = mtcars$cyl,
     cex = 2,
     xlab = "Объем двигателя (куб. дюймы)",
     ylab = "Расход топлива (миль/галлон)",
     main = "Зависимость расхода от объема")
dev.off()

# 3. Боксплоты + джиттерплоты mpg по cyl
png("iogen_course/3.png",
    width = 1000, 
    height = 500,
    units = "px"
)

# Боксплот с повернутыми подписями
boxplot(mpg ~ cyl, data = mtcars,
        xlab = "Количество цилиндров",
        ylab = "Расход топлива (миль/галлон)",
        main = "Расход топлива по количеству цилиндров",
        las = 2)

# Джиттер поверх
stripchart(mpg ~ cyl, data = mtcars,
           method = "jitter",
           vertical = TRUE,
           add = TRUE,
           pch = 16,
           col = "red",
           cex = 1.5)

dev.off()


# Задание 2. Панель с графиками выше
png("iogen_course/4panel.png",
    width = 1200, 
    height = 1000,
    units = "px"
)

par(mfrow = c(2, 2),
    oma = c(0, 0, 0, 0),
    mar = c(4, 4, 3, 1))

# Панель A: Гистограмма qsec
hist(mtcars$qsec, 
     col = "#FFC0CB",
     border = "#CD919E",
     main = "",
     xlab = "Время четверти мили (сек)",
     ylab = "Частота",
     cex.axis = 1.5, 
     cex.lab = 1.5)
mtext("A", side = 3, adj = 0, line = 1, cex = 1.5, font = 2)

# Панель B: Плотность qsec
plot(density(mtcars$qsec), 
     main = "",
     xlab = "qsec",
     ylab = "Плотность",
     cex.axis = 1.5,
     cex.lab = 1.5)
mtext("B", side = 3, adj = 0, line = 1, cex = 1.5, font = 2)

# Панель C: Точечный график mpg от disp
plot(mtcars$disp, mtcars$mpg,
     col = mtcars$cyl,
     pch = mtcars$cyl,
     cex = 2,
     xlab = "Объем двигателя (куб. дюймы)",
     ylab = "Расход топлива (миль/галлон)",
     main = "",
     cex.axis = 1.5,
     cex.lab = 1.5)
mtext("C", side = 3, adj = 0, line = 1, cex = 1.5, font = 2)

# Панель D: Боксплоты
boxplot(mpg ~ cyl, data = mtcars,
        xlab = "Количество цилиндров",
        ylab = "Расход топлива (миль/галлон)",
        main = "",
        las = 2,
        cex.axis = 1.5,
        cex.lab = 1.5)

# Джиттер
stripchart(mpg ~ cyl, data = mtcars,
           method = "jitter",
           vertical = TRUE,
           add = TRUE,
           pch = 16,
           col = "red",
           cex = 1.5)
mtext("D", side = 3, adj = 0, line = 1, cex = 1.5, font = 2)

dev.off()