# Библиотека ----

#Пакеты
#install.packages("shiny")
#install.packages("corrplot")
#install.packages("ggplot2")
#install.packages("randomForest")
#install.packages("xgboost")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("psych")

#Подключение библиотек
library(shiny)
library(corrplot)
library(ggplot2)
library(randomForest)
library(xgboost)
library(readr)
library(dplyr)
library(psych)

# Очистка рабочего пространства ----

while (dev.cur() != 1) {
  dev.off()
}
rm(list=ls())
cat("\014")

# Загрузка данных ----

setwd("D:/Data-Analysis/") # установка рабочей директории
diamonds = read.csv("diamonds.csv") # чтение данных из файла
diamonds # вывод данных в консоль