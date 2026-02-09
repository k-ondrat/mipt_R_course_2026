# Задание 2
# Блок A задание 4:
# Напишите функцию для обращения строки.
reverse_string <- function(str) {
  chars <- strsplit(str, "")[[1]]
  
  n <- length(chars)
  reversed <- character(n)
  
  for(i in 1:n) {
    reversed[i] <- chars[n - i + 1]
  }
  
  return(paste(reversed, collapse = ""))
}

reverse_string("1234abcd")  #-> "dcba4321"


# Блок B
# 1. Написать функцию с отложенным вычислением, когда будет использоваться значение аргумента, отличное от заданного пользователем.
lazy <- function(x, y = x**2) {
  print(x)
  x = 10
  print(x)
  print(y)
}
lazy(3)
lazy(3, 5)

# 2. Написать функцию с отложенным вычислением, когда будет использоваться значение аргумента, заданное пользователем.
lazy2 <- function(a, b = a * 2) {
  a + b
}

lazy2(10)
lazy2(10, 5)