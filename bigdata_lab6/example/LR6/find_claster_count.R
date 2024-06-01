# Импортируем данные из CSV файла
data <- read.csv("zzz.csv", header = TRUE)

head(data)
data <- data[, c( -8)]
head(data)

# Определение числовых и нечисловых столбцов
num_cols <- sapply(data, is.numeric)
num_cols

# Создание нового датафрейма с только числовыми столбцами
num_data <- subset(data, select = -num_cols)


dist.countries <- dist(num_data)
clust.countries <- hclust(dist.countries, "ward.D")

# install.packages("factoextra")
library (factoextra)
library (cluster)
# Метод локтя
fviz_nbclust(num_data, kmeans, method = "wss")

# Silhouette method
fviz_nbclust(num_data, kmeans, method = "silhouette") +
labs(subtitle = "Метод среднего силуэта")

# Метод статистики разрыва
#посчитать статистику разрыва,  базирующуюся на числе кластеров K.max = 6:
gap_stat <- clusGap(num_data, FUN = kmeans, nstart =4,K.max =8, B = 100)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

#Алгоритм на основе консенсуса
# install.packages('parameters')
# install.packages('NbClust')
# install.packages('mclust')

library(parameters)
library(NbClust)
library(mclust)

# Применение кластеризации методом консенсуса
# install.packages('parameters')
library(parameters)

n_clust <- n_clusters(data,
                      package = c("easystats", "NbClust", "mclust"),
                      standardize = FALSE)
n_clust
plot(n_clust)



