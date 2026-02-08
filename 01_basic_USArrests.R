# Задание 1
# Для встроенного датасета USArrests определить:
df <- USArrests

# 1. количество строк и столбцов(dim, ncol, nrow)
dim(df) # размер
ncol(df)  # столбцы
nrow(df) # строки

# 2. вывести на консоль содержание первых строк (head, tail, обращение по индексам строк)
head(df)
tail(df)
df[1,] # первая строка

my_rows <- c(2, 5, 8, 1)
df[my_rows, ] # набор строк

# 3. вывести на консоль 16-20 элементы 3 столбца, обратившись к нему по имени 
# (обращение по именам столбцов, индексам строк)

df$UrbanPop[16:20]

# 4. тип/структуру/подтип данных для датасета в целом и каждого столбца 
# (mode, class, typeof, цикл for, print)
for (i in 1:ncol(df)) {
  print(mode(df[[i]])) # с каким типом хранится объект в памяти
}

for (i in 1:ncol(df)) {
  print(class(df[[i]])) # класс объекта (датафрейм, матрица, фактор и т.д.)
}

for (i in 1:ncol(df)) {
  print(typeof(df[[i]])) # определяет внутренние для R типы объектов
}

print(mode(df))
print(class(df))
print(typeof(df))

# 5. имена столбцов и строк (colnames, rownames, dimnames)

colnames(df)
rownames(df)
dimnames(df) # список с двумя элементами: имена строк и имена столбцов

# 6. сумму, среднее, дисперсию, среднеквадратическое 
# отклонение всего датасета (sum, mean, sd, var)
summary(df) # статистика по датафрейму - разные средние
mean(df)
sum(df)
var(df) # ковариационная матрица по столбцам (дисперсия на диагонали)
# sd(df) ошибка: объект 'list' не может быть преобразован в тип 'double', class data.frame, но typeof list

# 7. сумму, среднее, дисперсию, среднеквадратическое отклонение каждого столбца 
for (i in 1:ncol(df)) {
  print(sum(df[[i]]))
}

for (i in 1:ncol(df)) {
  print(mean(df[[i]]))
}

for (i in 1:ncol(df)) {
  print(var(df[[i]]))
}

for (i in 1:ncol(df)) {
  print(sd(df[[i]]))
}

# 8. сумму, среднее, дисперсию, среднеквадратическое отклонение каждой строки
rowSums(df)
rowMeans(df)
apply(df, 1, var)
apply(df, 1, sd)

# 9. вывести с 10 по 14 элементы для каждого столбца (обращение по индексам строк, for)

for (i in 1:ncol(df)) {
  print(df[10:14, i])
}

# 10. сколько в датасете элементов меньше 10 (sum, условие)
sum(df<10)

# 11. сколько в каждом столбце элементов меньше 10 (sum, условие)
colSums(df < 10)

# 12. какие штаты содержат в названии “Miss” (which, rownames, grepl)
states_with_miss <- rownames(df)[which(grepl("Miss", rownames(df)))]
states_with_miss

# 13. вывести на консоль криминальную статистику для штатов, содержащих в названии “New”
df[rownames(df)[which(grepl("New", rownames(df)))],]

# 14. записать файл .csv, содержащий 1-20 строки и 1 и 3 столбцы
write.csv(df[1:20, c(1, 3)], file = "data.csv")

# 15. записать файл .xlsx, содержащий листы M и N со статистикой для штатов, начинающихся на букву M и N 
install.packages("writexl")
library(writexl)
library(readxl)
states_M <- df[grep("^M", rownames(USArrests)), ]
states_N <- df[grep("^N", rownames(USArrests)), ]
data_list <- list(M = states_M, N = states_N)
write_xlsx(data_list, "states_MN.xlsx")

# 16. прочитать записанные файлы
data1 <- read.csv('data.csv')
data_M <- read_excel("states_MN.xlsx", sheet = "M")
data_N <- read_excel("states_MN.xlsx", sheet = "N")