source('../bigdata_moment/bigdata_lab3/tasks/import.R')

data <- data_serials[,3:12]

# Возвращает датафрейм, но в каждом столбце только 6 первых элементов
head(data)

# основные статистические хар-ки
summary(data)