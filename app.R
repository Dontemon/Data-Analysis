#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages("xlsx", dep = T)
#install.packages("randomForest")
rsconnect::setAccountInfo(name='dontemon', token='B5AB2AC67AC54C9D8295BC9B12EFDEF5', secret='X+Vi02W5gQjBPNJHqQSc2u+cSS9mqyAJW2JaWZoQ')
library(shiny)
library(rsconnect)
library(xlsx)
library(randomForest)
# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel(h1("Предсказание цены бриллианта")),
  mainPanel(
    h4("1) Цена - цена в долларах США"),
    h4("2) Карат - вес бриллианта"),
    h4("3) Огранка - качество реза"),
    h4("4) Цвет - цвет бриллианта"),
    h4("5) Чистота – показатель того, насколько чист алмаз"),
    h4("6) Глубина - общий процент глубины"),
    h4("7) Площадка - самая большая грань бриллианта"),
    h4("8) х - длина в мм"),
    h4("9) у - ширина в мм"),width = 
    h4("10) z - глубина в мм"),
    numericInput("CaratInput", "Введите количество карат:", value = 0),
    numericInput("DepthInput", "Введите Depth:", value = 0),
    numericInput("TableInput", "Введите Table:", value = 0),
    numericInput("XInput", "Введите ширину:", value = 0),
    numericInput("YInput", "Введите высоту:", value = 0),
    numericInput("ZInput", "Введите длину:", value = 0),
    selectInput("CutValue", "Выберите огранку:",
                choices = c("Fair", "Good", "Very Good", "Premium", "Ideal")),
    selectInput("ClarityValue", "Выберите прозрачность:",
                choices = c("I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF")),
    selectInput("ColorValue", "Выберите цвет:",
                choices = c("D", "E", "F", "G", "H", "I", "J")),
    actionButton("myButton", "Получить значение"),
    actionButton("myButton2", "Получить значение2"),
    #img(src = "F:/Магистратура_1_семестр/Data-Analysis/diam.jpg", width = "300px", height = "200px"),
    textOutput("output")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  observeEvent(input$myButton,  {
    selectedCarat <- input$CaratInput
    selectedDepth <- input$DepthInput
    selectedTable <- input$TableInput
    selectedX <- input$XInput
    selectedY <- input$YInput
    selectedZ <- input$ZInput
    selectedCut <- input$CutValue
    selectedClarity <- input$ClarityValue
    selectedColor <- input$ColorValue
    
    means = read.xlsx("means.xlsx", sheetIndex = 1)
    sds = read.xlsx("sds.xlsx", sheetIndex = 1)
    diamonds = read.csv("diamonds.csv")
    diamonds$cut = as.factor(diamonds$cut)
    diamonds$color = as.factor(diamonds$color)
    diamonds$clarity = as.factor(diamonds$clarity)
    
    loadedModel <- readRDS("RandomForest.rds")
    new_data = data.frame(carat = selectedCarat, cut = selectedCut, color = selectedColor,	clarity = selectedClarity,	depth=selectedDepth,	table=selectedTable,	x=selectedX, y=selectedY,	z=selectedZ) #22520 строка Лучший результат 10581.99
    # Нормализуем новые данные с использованием средних и стандартных отклонений из таблицы diamonds
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
  
    
    
    
    # Используйте значение как угодно, например, выводим его в текстовом поле
    output$output <- renderText({
      paste("Цена бриллианта: ", round(prediction), "$")
    })
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
    
    
    # Используйте значение как угодно, например, выводим его в текстовом поле
    output$output <- renderText({
      paste("Выбрано число:", selectedDepth)
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
