# Пример 1. shiny+ggplot2
library(shiny)
library(ggplot2)
library(dplyr) # Для удобной фильтрации

data("diamonds")

ui <- fluidPage(
  titlePanel("Анализ алмазов"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var_x", "Выберите переменную для оси X:",
                  choices = names(diamonds)[sapply(diamonds, is.numeric)], # Только числовые
                  selected = "carat"),
      selectInput("var_color", "Выберите цвет по переменной:",
                  choices = names(diamonds)[sapply(diamonds, is.factor)], # Только факторы (категории)
                  selected = "cut")
    ),
    mainPanel(
      plotOutput("scatter_plot")
    )
  ),
  actionButton("stop_btn", "Закрыть приложение")
)

server <- function(input, output, session) {
  
  output$scatter_plot <- renderPlot({
    # ВАЖНО: используем aes_string() вместо aes(), чтобы передать имена переменных как строки
    p <- ggplot(diamonds, aes_string(x = input$var_x, 
                                     y = "price", 
                                     color = input$var_color)) +
      geom_point(alpha = 0.3, size = 1) +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
      labs(title = paste("Зависимость цены от", input$var_x),
           x = input$var_x,
           y = "Цена",
           color = input$var_color) +
      theme_minimal()
    
    print(p)
  })
  observeEvent(input$stop_btn, {
    stopApp()  # Функция для программного завершения приложения
  })
}

shinyApp(ui = ui, server = server)

# Пример 2: Динамическая фильтрация данных (Slider + Checkbox)
# Пользователь не только выбирает, что рисовать, но и какие данные использовать (интерактивный дашборд).
# 
# Возможность фильтровать данные внутри реактивного выражения (reactive({})), чтобы не переписывать код фильтрации в каждом графике.
install.packages("shiny")
library(shiny)
library(ggplot2)
library(dplyr)

data("mpg") # Данные о машинах

ui <- fluidPage(
  titlePanel("Данные о расходе топлива (MPG)"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Диапазон годов выпуска:",
                  min = min(mpg$year), max = max(mpg$year),
                  value = c(1999, 2008), step = 1, sep = ""),
      checkboxGroupInput("drives", "Тип привода:",
                         choices = unique(mpg$drv),
                         selected = unique(mpg$drv)),
      tags$hr(),  # горизонтальная линия для разделения
      actionButton("stop_btn", "Закрыть приложение", 
                   icon = icon("power-off"),  # Добавляем иконку
                   class = "btn-danger")      # Красный цвет кнопки
    ),
    mainPanel(
      plotOutput("histogram"),
      plotOutput("scatter")
    )
  )
)

server <- function(input, output) {
  
  # Реактивный датасет: фильтруется при изменении слайдера или чекбоксов
  filtered_data <- reactive({
    mpg %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2],
             drv %in% input$drives)
  })
  
  output$histogram <- renderPlot({
    req(nrow(filtered_data()) > 0) # Проверка, что данные не пустые
    
    ggplot(filtered_data(), aes(x = hwy)) +
      geom_histogram(bins = 15, fill = "skyblue", color = "white") +
      labs(title = "Расход на шоссе (Highway MPG)") +
      theme_minimal()
  })
  
  output$scatter <- renderPlot({
    req(nrow(filtered_data()) > 0)
    
    ggplot(filtered_data(), aes(x = displ, y = hwy, color = class)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = "Зависимость расхода от объема двигателя") +
      theme_minimal()
  })
  observeEvent(input$stop_btn, {
    stopApp()
  })
}

shinyApp(ui = ui, server = server)


# Пример 3: Интерактивность через "клик" 
# работа с nearPoints() и plotOutput(click = ...).

library(shiny)
library(ggplot2)
library(dplyr)

# ПОДГОТОВКА ДАННЫХ
# ------------------------------------------------------------------------------
data("mtcars")
# Преобразуем названия моделей в отдельную переменную и добавляем идентификатор
mtcars$model <- rownames(mtcars)
# Преобразуем cyl в фактор для корректной цветовой легенды
mtcars$cyl <- as.factor(mtcars$cyl)

# ИНТЕРФЕЙС ПОЛЬЗОВАТЕЛЯ (UI)
# ------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Интерактивный анализ автомобилей: вес vs расход топлива"),
  
  # Краткое описание для пользователя
  h5("Кликните на любую точку, чтобы увидеть информацию об автомобиле.
     Наведите курсор, чтобы увидеть ближайшие модели."),
  
  fluidRow(
    # Область графика (занимает 7 частей из 12)
    column(7,
           plotOutput("car_plot", 
                      click = "plot_click",      # Обработка клика
                      hover = hoverOpts(         # Обработка наведения с задержкой
                        id = "plot_hover",
                        delay = 100,              # Задержка 100ms перед показом
                        delayType = "debounce"
                      ))
    ),
    
    # Панель с информацией (занимает 5 частей из 12)
    column(5,
           # Карточка с информацией о клике
           wellPanel(
             h4("🔍 Информация о выбранной модели:"),
             verbatimTextOutput("click_info"),
             tags$hr(),  # Горизонтальная линия
             h4("👆 Модели под курсором:"),
             verbatimTextOutput("hover_info")
           ),
           
           # Дополнительная информация
           wellPanel(
             h4("📊 Статистика по клику:"),
             textOutput("click_stats")
           ),
           
           # Кнопка завершения работы
           wellPanel(
             style = "background-color: #f8f9fa; text-align: center;",
             actionButton("stop_btn", "Завершить работу", 
                          icon = icon("power-off"),
                          class = "btn-danger btn-lg btn-block",
                          style = "font-weight: bold;")
           )
    )
  )
)

# СЕРВЕРНАЯ ЛОГИКА
# ------------------------------------------------------------------------------
server <- function(input, output) {
  
  # ОСНОВНОЙ ГРАФИК
  # ----------------------------------------------------------------------------
  output$car_plot <- renderPlot({
    ggplot(mtcars, aes(x = wt, y = mpg, color = cyl, label = model)) +
      geom_point(size = 5, alpha = 0.8) +
      # Добавляем подписи только для точек, чтобы не перегружать график
      # Используем check_overlap для избежания наложений
      geom_text(aes(label = model), 
                size = 3.5,
                check_overlap = TRUE, 
                hjust = -0.1, 
                vjust = 0.5) +
      # Добавляем линии регрессии для каждой группы цилиндров
      geom_smooth(aes(group = cyl), 
                  method = "lm", 
                  se = FALSE, 
                  alpha = 0.5, 
                  linetype = "dashed") +
      # Настройка цветовой палитры
      scale_color_brewer(palette = "Set1") +
      labs(title = "Зависимость расхода топлива от веса автомобиля",
           subtitle = "По количеству цилиндров",
           x = "Вес (1000 фунтов)",
           y = "Расход топлива (миль на галлон)",
           color = "Цилиндры",
           caption = "Данные: mtcars") +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(face = "bold"))
  })
  
  # ИНФОРМАЦИЯ О КЛИКЕ
  # ----------------------------------------------------------------------------
  output$click_info <- renderPrint({
    # Проверяем, был ли совершен клик
    req(input$plot_click)
    
    # Находим ближайшую точку к месту клика
    clicked_point <- nearPoints(mtcars, 
                                input$plot_click, 
                                xvar = "wt", 
                                yvar = "mpg",
                                threshold = 10,      # Радиус поиска
                                maxpoints = 1,       # Только ближайшая точка
                                addDist = TRUE)      # Добавляем расстояние
    
    # Если точка найдена, выводим детальную информацию
    if (nrow(clicked_point) > 0) {
      cat("Модель:", clicked_point$model, "\n")
      cat("━━━━━━━━━━━━━━━━━━━━━━\n")
      cat("Вес (wt):", round(clicked_point$wt, 2), "1000 lbs\n")
      cat("Расход (mpg):", round(clicked_point$mpg, 1), "mpg\n")
      cat("Цилиндры:", as.character(clicked_point$cyl), "\n")
      cat("Лошадиные силы (hp):", clicked_point$hp, "\n")
      cat("Объем двигателя (disp):", round(clicked_point$disp, 1), "cu.in.\n")
      cat("Передач (gear):", clicked_point$gear, "\n")
      cat("━━━━━━━━━━━━━━━━━━━━━━\n")
      cat("Расстояние до клика:", round(clicked_point$dist_, 2), "(пиксели)")
    } else {
      cat("Кликните на точку для получения информации.\n",
          "Клик мимо данных: нет точки в радиусе 10 пикселей.")
    }
  })
  
  # ИНФОРМАЦИЯ ПРИ НАВЕДЕНИИ
  # ----------------------------------------------------------------------------
  output$hover_info <- renderPrint({
    # Проверяем наведение
    req(input$plot_hover)
    
    # Находим все точки в радиусе курсора
    hover_points <- nearPoints(mtcars, 
                               input$plot_hover, 
                               xvar = "wt", 
                               yvar = "mpg",
                               threshold = 15,      # Больший радиус для наведения
                               maxpoints = 5,       # Показываем до 5 точек
                               addDist = TRUE)
    
    # Сортируем по расстоянию и выводим
    if (nrow(hover_points) > 0) {
      hover_points <- hover_points[order(hover_points$dist_), ]
      cat("Найдено моделей:", nrow(hover_points), "\n")
      cat("━━━━━━━━━━━━━━━━━━━━━━\n")
      for (i in 1:nrow(hover_points)) {
        cat(sprintf("%d. %s (wt: %.2f, mpg: %.1f) [dist: %.1f]\n", 
                    i,
                    hover_points$model[i],
                    hover_points$wt[i],
                    hover_points$mpg[i],
                    hover_points$dist_[i]))
      }
    } else {
      cat("Нет точек в радиусе 15 пикселей от курсора")
    }
  })
  
  # СТАТИСТИКА ПО ВЫБРАННОЙ ТОЧКЕ
  # ----------------------------------------------------------------------------
  output$click_stats <- renderText({
    req(input$plot_click)
    
    clicked <- nearPoints(mtcars, 
                          input$plot_click, 
                          xvar = "wt", 
                          yvar = "mpg",
                          threshold = 10, 
                          maxpoints = 1)
    
    if (nrow(clicked) > 0) {
      # Сравниваем со средними показателями
      avg_mpg <- mean(mtcars$mpg)
      avg_wt <- mean(mtcars$wt)
      
      mpg_diff <- clicked$mpg - avg_mpg
      wt_diff <- clicked$wt - avg_wt
      
      paste0(
        "Сравнение со средним:\n",
        "Расход: ", 
        ifelse(mpg_diff > 0, "выше", "ниже"), 
        " среднего на ", abs(round(mpg_diff, 1)), " mpg\n",
        "Вес: ",
        ifelse(wt_diff > 0, "тяжелее", "легче"),
        " среднего на ", abs(round(wt_diff, 2)), " 1000 lbs"
      )
    } else {
      "Кликните на точку для сравнения со средними показателями"
    }
  })
  # Обработчик кнопки завершения работы
  observeEvent(input$stop_btn, {
    stopApp()
  })
}

# ЗАПУСК ПРИЛОЖЕНИЯ
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
