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

#переделал файл в xlsx и посмотрел все столбцы на наличие пустых и отсутствующих значений
#write_xlsx(diamonds, "diamonds.xlsx") 
#Пустых значений не обнаружено
#Установлено, что присутствуют нулевые значения в полях X, Y, Z. Это необходимо устранить.

#Заполнение нулевых значений средними значениями
diamonds = diamonds %>%
  mutate(x = ifelse(x == 0, mean(x[x != 0], na.rm = TRUE), x),
         y = ifelse(y == 0, mean(y[y != 0], na.rm = TRUE), y),
         z = ifelse(z == 0, mean(z[z != 0], na.rm = TRUE), z))

#удаление первого столбца (индексация)
diamonds = diamonds %>% 
  select(-1)

#удаление дубликатов
diamonds = diamonds %>% 
  filter(!duplicated(.))

#Вывод информации о типе и структуре данных
str(diamonds); summary(diamonds)

#Нормализация данных
diamonds$carat = scale(diamonds$carat)
diamonds$depth = scale(diamonds$depth)
diamonds$table = scale(diamonds$table)
diamonds$x = scale(diamonds$x)
diamonds$y = scale(diamonds$y)
diamonds$z = scale(diamonds$z)
diamonds$price = scale(diamonds$price)

#преобразование категориальных факторов для проверки корреляции
cut_levels = c("Fair" = 1, "Good" = 2, "Very Good" = 3, "Ideal" = 4, "Premium" = 5)
diamonds$cut = cut_levels[diamonds$cut]
color_levels = c("D" = 7, "E" = 6, "F" = 5, "G" = 4, "H" = 3, "I" = 2, "J" = 1)
diamonds$color = color_levels[diamonds$color]
clarity_levels = c("I1" = 1, "SI2" = 2, "SI1" = 3, "VS2" = 4, "VS1" = 5, "VVS2" = 6, "VVS1" = 7, "IF" = 8)
diamonds$clarity = clarity_levels[diamonds$clarity]

#корреляционная матрица
diamonds = subset(diamonds, select = c("price", "carat", "cut", "color", "clarity", "depth", "table", "x", "y","z"))
cor(diamonds) 
#график корреляций
corrplot(cor(diamonds), method = "color", type = "lower") 

#преобразование факторов
diamonds$cut = as.factor(diamonds$cut)
diamonds$color = as.factor(diamonds$color)
diamonds$clarity = as.factor(diamonds$clarity)

# Random Forest ----

#Создание обучающих и тестовых наборов
set.seed(123) # установка для повторяемости результатов
rownames(diamonds)=1:nrow(diamonds) #количество строк
rows = sample(x=1:nrow(diamonds),size=0.7 *  nrow(diamonds)) #разбиение выборки на две части случайным образом в отношении 7:3
train = diamonds[rows,] #выборка, на которой обучается модель
test = diamonds[! rownames(diamonds) %in% rows,] #выборка, на которой тестируется модель

#Исследовательские графики

#График, показывающий распределение цен в зависимости от качества огранки и цвета бриллиантов
ggplot(train)+
  geom_boxplot(aes(y=price,x=cut,fill=cut),outlier.size=0.1,notch=T,notchwidth=0.2)+
  facet_grid(~color,margins=T)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab(label='Price')+
  xlab('Cut')+
  ggtitle('Price distribution vs Cut for each colour')

#График, показывающий распределение цен в зависимости от веса бриллианта и качества огранки
ggplot(train)+
  geom_point(aes(y=price,x=carat,color=cut))+
  scale_x_log10()+scale_y_log10()+
  facet_grid(~cut)+
  ylab('Price')+
  xlab('Carat')+
  ggtitle('Price distribution vs Carat')

#График, показывающий распределение цен в зависимости от веса бриллианта
ggplot(train)+
  geom_point(aes(y=price,x=carat,color=cut))+
  ylab('Price')+
  xlab('Carat')+
  ggtitle('Price vs Carat')

#График плотности распределения цен в зависимости от качества огранки
ggplot(train)+
  geom_density(aes(x=price,fill=cut),alpha=0.2)+
  xlab('Price')+
  ylab('Density')+
  ggtitle('Price distribution')

#График, показывающий распределение цен в зависимости от глубины огранки и качества огранки
ggplot(train)+
  geom_point(aes(y=price,x=depth,color=cut))+
  xlab('Depth')+
  ylab('Price')

#создание случайного леса