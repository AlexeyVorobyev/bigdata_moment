source('../bigdata_moment/bigdata_lab3/tasks/import.R')

data <- data_serials[,3:12]

# Сортировка всего набора данных по возрастанию оценки в сериале "Ради всего человечества"

data_sorted <- data[order(data$`Ради.всего.человечества`),]

data_sorted