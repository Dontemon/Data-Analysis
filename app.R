#Пакеты
#install.packages("shiny")
#install.packages("corrplot")
#install.packages("ggplot2")
#install.packages("randomForest")
#install.packages("xgboost")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("psych")
#install.packages("writexl")
#install.packages("xlsx", dep = T) 
#install.packages("caret")
#install.packages("class")

#Подключение библиотек
library(caret)
library(shiny)
library(corrplot)
library(ggplot2)
library(randomForest)
library(xgboost)
library(readr)
library(dplyr)
library(psych)
library(writexl)
library(xlsx)
library(class)
rsconnect::setAccountInfo(name='dontemon', token='B5AB2AC67AC54C9D8295BC9B12EFDEF5', secret='X+Vi02W5gQjBPNJHqQSc2u+cSS9mqyAJW2JaWZoQ') 
library(rsconnect) 
 
first  <- 0
second <- 0
third <- 0
fourth <- 0

# Define UI for application that draws a histogram 
ui <- fluidPage( id = "body",
                 includeCSS("style2.css"),
                 titlePanel(h1(id = "title","Предсказание цены бриллианта")), 
                 mainPanel( id ="main",
                            selectInput("CutValue", "Выберите огранку:", 
                                        choices = c("Fair", "Good", "Very Good", "Premium", "Ideal")), 
                            selectInput("ClarityValue", "Выберите прозрачность:", 
                                        choices = c("I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF")), 
                            selectInput("ColorValue", "Выберите цвет:", 
                                        choices = c("D", "E", "F", "G", "H", "I", "J")), 
                            numericInput("CaratInput", "Введите количество карат:", value = 0), 
                            numericInput("DepthInput", "Введите глубина:", value = 0), 
                            numericInput("TableInput", "Введите площадка:", value = 0), 
                            numericInput("XInput", "Введите ширину:", value = 0), 
                            numericInput("YInput", "Введите высоту:", value = 0), 
                            numericInput("ZInput", "Введите длину:", value = 0), 
                            actionButton("myButton", "Random forest "), 
                            actionButton("myButton2", "XGBoost"), 
                            actionButton("myButton3", "Linear Regression"),
                            actionButton("myButton4", "K-nn"),
                            actionButton("myButton5", "Mean of alghorithms"),
                            textOutput("output")),
                 sidebarPanel( id = "sidebar",
                               h4("1) Цена - цена в долларах США"), 
                               h4("2) Карат - вес бриллианта"), 
                               h4("3) Огранка - качество реза"), 
                               h4("4) Цвет - цвет бриллианта"), 
                               h4("5) Чистота – показатель того, насколько чист алмаз"), 
                               h4("6) Глубина - общий процент глубины"), 
                               h4("7) Площадка - самая большая грань бриллианта"), 
                               h4("8) х - длина в мм"), 
                               h4("9) у - ширина в мм"),
                               h4("10) z - высота в мм"), 
                 ),
                 
                 
) 

# Define server logic required to draw a histogram 
server <- function(input, output) { 
  
  output$output <- renderText({ 
    paste("Цена бриллианта: ") 
  }) 
  
  
  observeEvent(input$myButton,  { 
    means = read.xlsx("means.xlsx", sheetIndex = 1) 
    sds = read.xlsx("sds.xlsx", sheetIndex = 1) 
    diamonds = read.csv("diamonds.csv") 
    selectedCarat <- input$CaratInput 
    selectedDepth <- input$DepthInput 
    selectedTable <- input$TableInput 
    selectedX <- input$XInput 
    selectedY <- input$YInput 
    selectedZ <- input$ZInput 
    selectedCut <- input$CutValue 
    selectedClarity <- input$ClarityValue 
    selectedColor <- input$ColorValue 
    
    if(selectedCarat > 0 && selectedDepth > 0 && selectedTable > 0 && selectedX > 0 && selectedY > 0 && selectedZ > 0){
      diamonds$cut = as.factor(diamonds$cut) 
      diamonds$color = as.factor(diamonds$color) 
      diamonds$clarity = as.factor(diamonds$clarity) 
      
      loadedModel <- readRDS("RandomForest.rds") 
      new_data = data.frame(carat = selectedCarat, cut = selectedCut, color = selectedColor,
                            clarity = selectedClarity,	depth=selectedDepth,	table=selectedTable,
                            x=selectedX, y=selectedY,	z=selectedZ) 
      
      new_data$carat = (new_data$carat - means$carat) / sds$carat 
      new_data$depth = (new_data$depth - means$depth) / sds$depth 
      new_data$table = (new_data$table - means$table) / sds$table 
      new_data$x = (new_data$x - means$x) / sds$x 
      new_data$y = (new_data$y - means$y) / sds$y 
      new_data$z = (new_data$z - means$z) / sds$z 
      new_data$color = factor(new_data$color, levels = levels(diamonds$color)) 
      new_data$clarity = factor(new_data$clarity, levels = levels(diamonds$clarity)) 
      new_data$cut = factor(new_data$cut, levels = levels(diamonds$cut)) 
      prediction = predict(loadedModel, new_data) 
      
      first <<- prediction
      
      output$output <- renderText({ 
        paste("Цена бриллианта: ", round(prediction), "$") 
      }) 
    }
    else{
      output$output <- renderText({ 
        paste("Ошибка.Неверно введены входные данные!") 
      }) 
    }
  }) 
  
  observeEvent(input$myButton2,  { 
    selectedCarat <- input$CaratInput 
    selectedDepth <- input$DepthInput 
    selectedTable <- input$TableInput 
    selectedX <- input$XInput 
    selectedY <- input$YInput 
    selectedZ <- input$ZInput 
    selectedCut <- input$CutValue 
    selectedClarity <- input$ClarityValue 
    selectedColor <- input$ColorValue 
    
    if(selectedCarat > 0 && selectedDepth > 0 && selectedTable > 0 && selectedX > 0 && selectedY > 0 && selectedZ > 0){
      means = read.xlsx("means.xlsx", sheetIndex = 1) 
      sds = read.xlsx("sds.xlsx", sheetIndex = 1) 
      diamonds = read.csv("diamonds.csv") 
      diamonds$cut = as.factor(diamonds$cut) 
      diamonds$color = as.factor(diamonds$color) 
      diamonds$clarity = as.factor(diamonds$clarity) 
      
      loadedModel <- readRDS("XGBoost.rds") 
      new_data = data.frame(carat = selectedCarat, cut = selectedCut, color = selectedColor,
                            clarity = selectedClarity,	depth=selectedDepth,	table=selectedTable,
                            x=selectedX, y=selectedY,	z=selectedZ) 
      
      new_data$carat = (new_data$carat - means$carat) / sds$carat 
      new_data$depth = (new_data$depth - means$depth) / sds$depth 
      new_data$table = (new_data$table - means$table) / sds$table 
      new_data$x = (new_data$x - means$x) / sds$x 
      new_data$y = (new_data$y - means$y) / sds$y 
      new_data$z = (new_data$z - means$z) / sds$z 
      new_data$cut <- as.numeric(as.factor(new_data$cut))
      new_data$color <- as.numeric(as.factor(new_data$color))
      new_data$clarity <- as.numeric(as.factor(new_data$clarity))
      dnew <- xgb.DMatrix(as.matrix(new_data))
      prediction = predict(loadedModel, dnew) 
      
      second <<- prediction
      
      output$output <- renderText({ 
        paste("Цена бриллианта: ", round(prediction), "$") 
      }) 
    }
    else{
      output$output <- renderText({ 
        paste("Ошибка.Неверно введены входные данные!") 
      }) 
    }
  }) 
  
  observeEvent(input$myButton3,  { 
    selectedCarat <- input$CaratInput 
    selectedDepth <- input$DepthInput 
    selectedTable <- input$TableInput 
    selectedX <- input$XInput 
    selectedY <- input$YInput 
    selectedZ <- input$ZInput 
    selectedCut <- input$CutValue 
    selectedClarity <- input$ClarityValue 
    selectedColor <- input$ColorValue 
    
    if(selectedCarat > 0 && selectedDepth > 0 && selectedTable > 0 && selectedX > 0 && selectedY > 0 && selectedZ > 0){
      means = read.xlsx("means.xlsx", sheetIndex = 1) 
      sds = read.xlsx("sds.xlsx", sheetIndex = 1) 
      diamonds = read.csv("diamonds.csv") 
      diamonds$cut = as.factor(diamonds$cut) 
      diamonds$color = as.factor(diamonds$color) 
      diamonds$clarity = as.factor(diamonds$clarity) 
      
      loadedModel <- readRDS("LinearRegression.rds") 
      new_data = data.frame(carat = selectedCarat, cut = selectedCut, color = selectedColor,
                            clarity = selectedClarity,	depth=selectedDepth,	table=selectedTable,
                            x=selectedX, y=selectedY,	z=selectedZ) 
      new_data$color = factor(new_data$color, levels = levels(diamonds$color)) 
      new_data$clarity = factor(new_data$clarity, levels = levels(diamonds$clarity)) 
      new_data$cut = factor(new_data$cut, levels = levels(diamonds$cut)) 
      prediction = predict(loadedModel, new_data) 
      
      third <<- prediction
      
      output$output <- renderText({ 
        paste("Цена бриллианта: ", round(prediction), "$") 
      }) 
    }
    else{
      output$output <- renderText({ 
        paste("Ошибка.Неверно введены входные данные!") 
      }) 
    }
  }) 
  
  observeEvent(input$myButton4,  { 
    selectedCarat <- input$CaratInput 
    selectedDepth <- input$DepthInput 
    selectedTable <- input$TableInput 
    selectedX <- input$XInput 
    selectedY <- input$YInput 
    selectedZ <- input$ZInput 
    selectedCut <- input$CutValue 
    selectedClarity <- input$ClarityValue 
    selectedColor <- input$ColorValue 
    
    if(selectedCarat > 0 && selectedDepth > 0 && selectedTable > 0 && selectedX > 0 && selectedY > 0 && selectedZ > 0){
    diamonds = read.csv("diamonds_clean.csv") 
    diamonds$carat = scale(diamonds$carat)
    diamonds$depth = scale(diamonds$depth)
    diamonds$table = scale(diamonds$table)
    diamonds$x = scale(diamonds$x)
    diamonds$y = scale(diamonds$y)
    diamonds$z = scale(diamonds$z)
    
    # Создание факторов для категориальных переменных
    diamonds$cut <- as.factor(diamonds$cut)
    diamonds$color <- as.factor(diamonds$color)
    diamonds$clarity <- as.factor(diamonds$clarity)
    
    # Определение обучающего и тестового наборов
    sample_size <- floor(0.7 * nrow(diamonds))
    train_indices <- sample(seq_len(nrow(diamonds)), size = sample_size)
    train <- diamonds[train_indices, ]
    test <- diamonds[-train_indices, ]
    
    # Построение модели k ближайших соседей
    k <- 1
    knn_model <- knn(train = train[, c("carat", "depth", "table", "x", "y", "z")],
                     test = test[, c("carat", "depth", "table", "x", "y", "z")],
                     cl = train$price,
                     k = k)
    
    means = read.xlsx("means.xlsx", sheetIndex = 1) 
    sds = read.xlsx("sds.xlsx", sheetIndex = 1) 
    
    # Оценка точности модели
    new_data = data.frame(carat = selectedCarat, cut = selectedCut, color = selectedColor,
                          clarity = selectedClarity,	depth=selectedDepth,	table=selectedTable,
                          x=selectedX, y=selectedY,	z=selectedZ) 
    new_data$carat = (new_data$carat - means$carat) / sds$carat 
    new_data$depth = (new_data$depth - means$depth) / sds$depth 
    new_data$table = (new_data$table - means$table) / sds$table 
    new_data$x = (new_data$x - means$x) / sds$x 
    new_data$y = (new_data$y - means$y) / sds$y 
    new_data$z = (new_data$z - means$z) / sds$z
    new_data$cut <- as.factor(new_data$cut)
    new_data$color <- as.factor(new_data$color)
    new_data$clarity <- as.factor(new_data$clarity)
    
    predicted_price <- knn(train = train[, c("carat", "depth", "table", "x", "y", "z")],
                           test = new_data[, c("carat", "depth", "table", "x", "y", "z")],
                           cl = train$price,
                           k = k)
    desired_value <- as.numeric(as.character(predicted_price))[1]
    fourth <<-  desired_value
  
    output$output <- renderText({ 
      paste("Цена бриллианта: ", predicted_price, "$") 
    }) 
    }
    else{
      output$output <- renderText({ 
        paste("Ошибка.Неверно введены входные данные!") 
      }) 
    }
  })
  
  observeEvent(input$myButton5,  { 
    selectedCarat <- input$CaratInput 
    selectedDepth <- input$DepthInput 
    selectedTable <- input$TableInput 
    selectedX <- input$XInput 
    selectedY <- input$YInput 
    selectedZ <- input$ZInput 
    selectedCut <- input$CutValue 
    selectedClarity <- input$ClarityValue 
    selectedColor <- input$ColorValue 
    
    price = (first + second + third + fourth)/4  
    
    output$output <- renderText({ 
      paste("Цена бриллианта: ", round(price), "$") 
    }) 
    
  })
} 

# Run the application  
shinyApp(ui = ui, server = server) 
