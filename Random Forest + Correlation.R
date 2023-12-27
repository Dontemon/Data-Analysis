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

#Сохранение средних и среднекв.отклонений (для нормализации введенных данных в приложении)
means = colMeans(diamonds[, c("carat", "depth", "table", "x", "y", "z")])
sds = apply(diamonds[, c("carat", "depth", "table", "x", "y", "z")], 2, sd)
means_table = data.frame(
  carat = means["carat"],
  depth = means["depth"],
  table = means["table"],
  x = means["x"],
  y = means["y"],
  z = means["z"]
)
write_xlsx(means_table, "means.xlsx") 
sds_table = data.frame(
  carat = sds["carat"],
  depth = sds["depth"],
  table = sds["table"],
  x = sds["x"],
  y = sds["y"],
  z = sds["z"]
)
write_xlsx(sds_table, "sds.xlsx") 

#Нормализация данных
diamonds$carat = scale(diamonds$carat)
diamonds$depth = scale(diamonds$depth)
diamonds$table = scale(diamonds$table)
diamonds$x = scale(diamonds$x)
diamonds$y = scale(diamonds$y)
diamonds$z = scale(diamonds$z)

#Корреляционный анализ ----

data = diamonds

#преобразование категориальных факторов для проверки корреляции
cut_levels = c("Fair" = 1, "Good" = 2, "Very Good" = 3, "Ideal" = 4, "Premium" = 5)
data$cut = cut_levels[data$cut]
color_levels = c("D" = 7, "E" = 6, "F" = 5, "G" = 4, "H" = 3, "I" = 2, "J" = 1)
data$color = color_levels[data$color]
clarity_levels = c("I1" = 1, "SI2" = 2, "SI1" = 3, "VS2" = 4, "VS1" = 5, "VVS2" = 6, "VVS1" = 7, "IF" = 8)
data$clarity = clarity_levels[data$clarity]

#корреляционная матрица
data = subset(data, select = c("price", "carat", "cut", "color", "clarity", "depth", "table", "x", "y","z"))
cor(data) 
#график корреляций
corrplot(cor(data), method = "color", type = "lower") 

# Random Forest ----

diamonds = subset(diamonds, select = c("carat", "cut", "color", "clarity", "depth", "table", "price", "x", "y","z"))

#преобразование факторов
diamonds$cut = as.factor(diamonds$cut)
diamonds$color = as.factor(diamonds$color)
diamonds$clarity = as.factor(diamonds$clarity)

set.seed(123)

# Разделение данных на обучающий и тестовый наборы
sample_size = floor(0.7 * nrow(diamonds))
train_indices = sample(seq_len(nrow(diamonds)), size = sample_size)

train = diamonds[train_indices, ]
test = diamonds[-train_indices, ]

#Исследовательские графики ----

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

# Random Forest ----

# Определение зависимой переменной
response_variable <- "price"

# Обучение модели Random Forest
rf_model = randomForest(formula = as.formula(paste(response_variable, "~ .")), data = train, ntree = 100)
imp=importance(rf_model) # вычисление важности каждого признака в модели
vars=dimnames(imp)[[1]] # извлечение имени признаков
imp=data.frame(vars=vars,imp=as.numeric(imp[,1])) # создание DataFrame с именем признака и его важностью в модели
imp=imp[order(imp$imp,decreasing=T),] #сортировка по убыванию важности признаков
par(mfrow=c(1,2)) #для построения двух графиков
varImpPlot(rf_model,main='Variable Importance Plot: Base Model')
plot(rf_model,main='Error vs No. of trees plot: Base Model')
# Предсказание на тестовом наборе
predictions = predict(rf_model, newdata = test)
rmse1 = sqrt(mean((test$price - predictions)^2))
cat("RMSE: ", rmse1, "\n")

set.seed(123)
selected=c(as.character(imp[1:6,1]),'price') #берём первые 6 факторов
rf_model = randomForest(formula = as.formula(paste(response_variable, "~ .")), data = train[, selected], ntree = 100, mtry = 3, maxdepth = 10, nodesize = 5)
par(mfrow=c(1,2)) #для построения двух графиков
varImpPlot(rf_model,main='Variable Importance Plot: Base Model')
plot(rf_model,main='Error vs No. of trees plot: Base Model')
# Предсказание на тестовом наборе
predictions = predict(rf_model, newdata = test)
rmse2 = sqrt(mean((test$price - predictions)^2))
cat("RMSE: ", rmse2, "\n")

actual=test$price
result=data.frame(actual=actual,predicted=predictions)
ggplot(result)+
  geom_point(aes(x=actual,y=predicted,color=predicted-actual),alpha=0.7)+
  ggtitle('Plotting Error')
#это связано с тем, что количесво цен до 3000$ намного больше, поэтому модель достаточно хорошо предскажет цены для простых брилиантов

new_data = data.frame(carat = 1.26, cut = "Ideal", color = "G",	clarity = "VVS2",	depth=60.7,	table=56,	x=7.05, y=7.03,	z=4.27) #22520 строка Лучший результат 10581.99
# Нормализуем новые данные с использованием средних и стандартных отклонений из таблицы diamonds
new_data$carat = (new_data$carat - means["carat"]) / sds["carat"]
new_data$depth = (new_data$depth - means["depth"]) / sds["depth"]
new_data$table = (new_data$table - means["table"]) / sds["table"]
new_data$x = (new_data$x - means["x"]) / sds["x"]
new_data$y = (new_data$y - means["y"]) / sds["y"]
new_data$z = (new_data$z - means["z"]) / sds["z"]
new_data$color = factor(new_data$color, levels = levels(diamonds$color))
new_data$clarity = factor(new_data$clarity, levels = levels(diamonds$clarity))
new_data$cut = factor(new_data$cut, levels = levels(diamonds$cut))
prediction = predict(rf_model, new_data)
prediction

saveRDS(rf_model, file = "RandomForest.rds")

