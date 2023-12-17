# Библиотека ----

#Пакеты
install.packages("shiny")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("randomForest")
install.packages("xgboost")
install.packages("readr")
install.packages("dplyr")
install.packages("psych")
install.packages("writexl")

#Подключение библиотек
library(shiny)
library(corrplot)
library(ggplot2)
library(randomForest)
library(xgboost)
library(readr)
library(dplyr)
library(psych)
library(writexl)

# Очистка рабочего пространства ----

while (dev.cur() != 1) {
  dev.off()
}
rm(list=ls())
cat("\014")

# Загрузка данных ----

#price - price in US dollars (\$326--\$18,823)
#carat (0,2-5,01) - weight of the diamond (0.2--5.01)
#cut - quality of the cut (Fair, Good, Very Good, Premium, Ideal)
#color - diamond color, from J (worst) to D (best)
#clarity - a measurement of how clear the diamond is (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))
#depth - total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43--79)
#table - width of top of diamond relative to widest point (43--95)
#x - length in mm (0--10.74)
#y - width in mm (0--58.9)
#z - depth in mm (0--31.8)

setwd("D:/Data-Analysis/") #установка рабочей директории
diamonds = read.csv("diamonds.csv") #чтение данных из файла
write_xlsx(diamonds, "diamonds.xlsx") #переделал файл в xlsx и посмотрел все столбцы, имеются ли пустые значения
#Пустых значений не обнаружено
#Установлено, что присутствуют нулевые значения в полях X, Y, Z. Это необходимо устранить.
#Заполнение нулевых значений средними значениями
data = diamonds %>%
  mutate(x = ifelse(x == 0, mean(x[x != 0], na.rm = TRUE), x),
         y = ifelse(y == 0, mean(y[y != 0], na.rm = TRUE), y),
         z = ifelse(z == 0, mean(z[z != 0], na.rm = TRUE), z))


