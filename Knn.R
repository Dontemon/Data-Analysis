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
#install.packages("writexl")
#install.packages("xlsx", dep = T) 
#install.packages("class")


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
library(xlsx)
library(class)

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

#переделал файл в xlsx и посмотрел все столбцы на наличие пустых и отсутствующих значений
write_xlsx(diamonds, "diamonds.xlsx") 
#Пустых значений не обнаружено
#Установлено, что присутствуют нулевые значения в полях X, Y, Z. Это необходимо устранить.

mean_carat=mean(diamonds$carat)
mean_depth=mean(diamonds$depth)
mean_table=mean(diamonds$table)
mean_x=mean(diamonds$x)
mean_y=mean(diamonds$y)
mean_z=mean(diamonds$y)

#Заполнение нулевых значений средними значениями
diamonds = diamonds %>%
  mutate(x = ifelse(x == 0, mean_x, x),
         y = ifelse(y == 0, mean_y, y),
         z = ifelse(z == 0, mean_z, z))

#удаление первого столбца (индексация)
diamonds = diamonds %>% select(-1)

#удаление дубликатов
diamonds = diamonds %>% filter(!duplicated(.))

replace_outliers_with_mean_exclude = function(column,lower,upper,mean) {
  outliers = column < lower | column > upper
  column[outliers] = mean
  return(column)
}

boxplot(diamonds$carat)
Q1 = quantile(diamonds$carat, 0.25)
Q3 = quantile(diamonds$carat, 0.75)
IQR = Q3 - Q1
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR
diamonds$carat=replace_outliers_with_mean_exclude(diamonds$carat,lower_bound,upper_bound,mean_carat); boxplot(diamonds$carat)
Q1 = quantile(diamonds$carat, 0.25)
Q3 = quantile(diamonds$carat, 0.75)
IQR = Q3 - Q1
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR
diamonds$carat=replace_outliers_with_mean_exclude(diamonds$carat,lower_bound,upper_bound,mean_carat); boxplot(diamonds$carat)

boxplot(diamonds$depth)
Q1 = quantile(diamonds$depth, 0.25)
Q3 = quantile(diamonds$depth, 0.75)
IQR = Q3 - Q1
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR
diamonds$depth=replace_outliers_with_mean_exclude(diamonds$depth,lower_bound,upper_bound,mean_depth); boxplot(diamonds$depth)
Q1 = quantile(diamonds$depth, 0.25)
Q3 = quantile(diamonds$depth, 0.75)
IQR = Q3 - Q1
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR
diamonds$depth=replace_outliers_with_mean_exclude(diamonds$depth,lower_bound,upper_bound,mean_depth); boxplot(diamonds$depth)
Q1 = quantile(diamonds$depth, 0.25)
Q3 = quantile(diamonds$depth, 0.75)
IQR = Q3 - Q1
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR
diamonds$depth=replace_outliers_with_mean_exclude(diamonds$depth,lower_bound,upper_bound,mean_depth); boxplot(diamonds$depth)
Q1 = quantile(diamonds$depth, 0.25)
Q3 = quantile(diamonds$depth, 0.75)
IQR = Q3 - Q1
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR
diamonds$depth=replace_outliers_with_mean_exclude(diamonds$depth,lower_bound,upper_bound,mean_depth); boxplot(diamonds$depth)

boxplot(diamonds$table)
Q1 = quantile(diamonds$table, 0.25)
Q3 = quantile(diamonds$table, 0.75)
IQR = Q3 - Q1
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR
diamonds$table=replace_outliers_with_mean_exclude(diamonds$table,lower_bound,upper_bound,mean_table); boxplot(diamonds$table)

boxplot(diamonds$x)
Q1 = quantile(diamonds$x, 0.25)
Q3 = quantile(diamonds$x, 0.75)
IQR = Q3 - Q1
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR
diamonds$x=replace_outliers_with_mean_exclude(diamonds$x,lower_bound,upper_bound,mean_x); boxplot(diamonds$x)

boxplot(diamonds$y)
Q1 = quantile(diamonds$y, 0.25)
Q3 = quantile(diamonds$y, 0.75)
IQR = Q3 - Q1
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR
diamonds$y=replace_outliers_with_mean_exclude(diamonds$y,lower_bound,upper_bound,mean_y); boxplot(diamonds$y)

boxplot(diamonds$z)
Q1 = quantile(diamonds$z, 0.25)
Q3 = quantile(diamonds$z, 0.75)
IQR = Q3 - Q1
lower_bound = Q1 - 1.5 * IQR
upper_bound = Q3 + 1.5 * IQR
diamonds$z=replace_outliers_with_mean_exclude(diamonds$z,lower_bound,upper_bound,mean_z); boxplot(diamonds$z)

diamonds$carat = scale(diamonds$carat)
diamonds$depth = scale(diamonds$depth)
diamonds$table = scale(diamonds$table)
diamonds$x = scale(diamonds$x)
diamonds$y = scale(diamonds$y)
diamonds$z = scale(diamonds$z)

#install.packages("caret")
#library(caret)

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
new_data = data.frame(carat = 1.26, cut = "Ideal", color = "G",	clarity = "VVS2",	depth=60.7,	table=56,	x=7.05, y=7.03,	z=4.27)
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
cat("Predicted Price:", predicted_price, "\n")
saveRDS(predicted_price, file = "Kmeans.rds")
