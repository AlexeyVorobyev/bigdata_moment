# Импортируем данные из CSV файла
data <- read.csv("zzz.csv", header = TRUE)

# Смотрим на первые несколько строк
head(data)

# Получаем общее описание данных
summar <- summary(data)
print(summar)

# Выводим размер датафрейма
dim(data)

# Находим уникальные значения для каждого столбца
sapply(data, function(x) length(unique(x)))

# Проверяем наличие пропущенных значений
sum(is.na(data))

# Получаем основные статистики для числовых столбцов
summary(data[, sapply(data, is.numeric)])

# Сортируем данные по убыванию значения
data <- data[order(-data$доход), ]

# Выбираем топ 10 стран
top_10 <- head(data[, c("страна", "доход")], 10)

# Создаем столбчатую диаграмму
barplot(top_10$доход,
        names.arg = top_10$страна,
        xlab = "",
        ylab = "Доход",
        main = "Топ 10 стран по параметру дохода",
        col = "steelblue",
        las=2
        )

# Создаем новый датафрейм с нормализованными значениями
data_plot <- data[, c("страна", "длит_муж", "длит_жен")]

# Преобразуем данные из широкого формата в длинный
library(reshape2)
data_long <- melt(data_plot, id.vars = "страна")

# Создаем столбчатую диаграмму
library(ggplot2)
ggplot(data_long, aes(x = страна, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Страна", y = "Значение", title = "Средняя длительность мужской и женской жизни") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
