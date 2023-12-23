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

  includeCSS("style.css"),
  titlePanel(tags$h1("Предсказание цены бриллианта")),
  mainPanel(
    tags$table(
      tags$tr(
        tags$td("Введите количество карат:"),
        tags$td(tags$input(id = "caratInput")),
        tags$td("Введите глубину:"),
        tags$td(tags$input(id = "depthInput")),
        tags$td("Введите площадку:"),
        tags$td(tags$input(id = "tableInput")),
        tags$td("Введите ширину:"),
        tags$td(tags$input(id = "xInput")),
        tags$td("Введите высоту:"),
        tags$td(tags$input(id = "yInput")),
      ),
      tags$tr(
        tags$td("Введите длину:"),
        tags$td(tags$input(id = "zInput")),
        tags$td("Выберите огранку:"),
        tags$td(tags$select(
          id = "cutValue",
          tags$option("Fair"),
          tags$option("Good"),
          tags$option("Very Good"),
          tags$option("Premium"),
          tags$option("Ideal"),
        )),
        tags$td("Выберите прозрачность:"),
        tags$td(tags$select(
          id = "clarityValue",
          tags$option("I1"),
          tags$option("SI2"),
          tags$option("SI1"),
          tags$option("VS2"),
          tags$option("VS1"),
          tags$option("VVS2"),
          tags$option("VVS1"),
          tags$option("IF"),
        )),
        tags$td("Выберите цвет:"),
        tags$td(tags$select(
          id = "colorValue",
          tags$option("D"),
          tags$option("E"),
          tags$option("F"),
          tags$option("G"),
          tags$option("H"),
          tags$option("I"),
          tags$option("J"),
        )),
      ),
    ),
    tags$div(
      tags$button(id = "firstButton", "First"),
      tags$button(id = "secondButton", "Second"),
      tags$button(id = "thirdButton", "Third"),
      tags$button(id = "fourthButton", "Fourth"),
    ),
    tags$article(
      tags$div(class ="text","1) Цена - цена в долларах США"),
      tags$div(class ="text","2) Карат - вес бриллианта"),
      tags$div(class ="text","3) Огранка - качество реза"),
      tags$div(class ="text","4) Цвет - цвет бриллианта"),
      tags$div(class ="text","5) Чистота – показатель того, насколько чист алмаз"),
      tags$div(class ="text","6) Глубина - общий процент глубины"),
      tags$div(class ="text","7) Площадка - самая большая грань бриллианта"),
      tags$div(class ="text","8) х - длина в мм"),
      tags$div(class ="text","9) у - ширина в мм"),
      tags$div(class ="text","z - глубина в мм"),
    ),
    uiOutput("output"),
    #numericInput("CaratInput", "Введите количество карат:", value = 0),
    #numericInput("DepthInput", "Введите глубина:", value = 0),
    #numericInput("TableInput", "Введите площадку:", value = 0),
   # numericInput("XInput", "Введите ширину:", value = 0),
  #  numericInput("YInput", "Введите высоту:", value = 0),
   # numericInput("ZInput", "Введите длину:", value = 0),
   # selectInput("CutValue", "Выберите огранку:",
   #             choices = c("Fair", "Good", "Very Good", "Premium", "Ideal")),
   # selectInput("ClarityValue", "Выберите прозрачность:",
    #            choices = c("I1", "SI2", "SI1", "VS2", "VS1", "VVS2", "VVS1", "IF")),
   # selectInput("ColorValue", "Выберите цвет:",
    #            choices = c("D", "E", "F", "G", "H", "I", "J")),
    #actionButton("myButton", "Получить значение"),
    #actionButton("myButton2", "Получить значение2"),
    #img(src = "F:/Магистратура_1_семестр/Data-Analysis/diam.jpg", width = "300px", height = "200px"),
  actionButton("myButton", "Получить значение"),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session ) {
  
  observeEvent(input$firstButton,  {
    selectedCarat <- input$caratInput
    selectedDepth <- input$depthInput
    selectedTable <- input$tableInput
    selectedX <- input$xInput
    selectedY <- input$yInput
    selectedZ <- input$zInput
    selectedCut <- input$cutValue
    selectedClarity <- input$clarityValue
    selectedColor <- input$colorValue
    
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
    output$output <- renderUI({
      tags$div(
        style = "font-size: 18px; color: blue;",
        paste("Цена бриллианта: ", result, "$")
      )
    })
  })
  
  observeEvent(input$myButton,  {
    selectedCarat <- input$caratInput
    # Используйте значение как угодно, например, выводим его в текстовом поле
    output$output <- renderUI({
      tags$div(
        style = "font-size: 18px; color: blue;",
        paste("Цена бриллианта: ", selectedCarat, "$")
      )
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
