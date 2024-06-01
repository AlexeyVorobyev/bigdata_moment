# Импортируем данные из CSV файла
data <- read.csv("zzz.csv", header = TRUE)

head(data)
data_wthout_region <- data[, c(-8)]

# Определение числовых и нечисловых столбцов
num_cols <- sapply(data_wthout_region, is.numeric)

# Создание нового датафрейма с только числовыми столбцами
num_data <- data_wthout_region[, num_cols]
head(num_data)
dist.countries <- dist(num_data)
clust.countries <- hclust(dist.countries, "ward.D")

plot(clust.countries, data$регион, cex=0.5)
rect.hclust(clust.countries, k=4, border="red")


#  Вектор groups содержит номер кластера, в который попал классифицируемый объект 
groups <- cutree(clust.countries, k=4) 

#  Для каждой группы определяем средние значения характеристик и строим датафрейм.
#  в 1-ом кластере
g1<-colMeans(num_data[groups==1, 1:6])
#  во 2-ом кластере
g2<-colMeans(num_data[groups==2, 1:6])
#  в 3-ем кластере
g3<-colMeans(num_data[groups==3, 1:6])
#  в 4-ем кластере
g4<-colMeans(num_data[groups==4, 1:6])
df<-data.frame(g1,g2,g3, g4)
df1<-t(df)
df<-t(df1)


barplot(df, ylim=c(0,12),  
        main = "Группы стран", axes = FALSE, 
        col=c("red","green","blue","yellow", "purple", "orange"), beside=TRUE)
axis(2, at = 0:5, labels = 0:5)
legend("top", legend = rownames(df), col=c("red","green","blue","yellow", "purple", "orange"), lwd=10, bty = "n")


# КЛАСТЕРИЗАЦИЯ K-MEANS

km.res <- kmeans(num_data, 5, nstart = 10)
fviz_cluster(km.res, num_data, ellipse.type = "norm")
# Изменить палитру и темуы
fviz_cluster(km.res, num_data,
             palette = "Set2", ggtheme = theme_minimal())


library (lattice)
xyplot(деск_см ~ рождаем, 
       data = data, 
       group = groups,
       auto.key = TRUE
       )

data["Groups"] <- groups

boxplot(
  длит_муж ~ Groups, 
  data = data,
  ylab = "Доход в стране",
  frame = FALSE, 
  col = "lightgray"
  )

pairs(num_data)

pairs(
  num_data,
  main= "Уровень жизни в стране", 
  col = c("red","green","blue", "grey")
  )

my_cols <- c("#00AFBB", "#E7B800", "#FC4E07", "grey")  
pairs(
  num_data,
      main= "Данные по группам",
      pch = 19,  cex = 0.8,
      col = my_cols[data$Groups],
      lower.panel=NULL
      )


# Трёхмерный скаттерплот
# install.packages("scatterplot3d")
library("scatterplot3d")

# Трехмерная классификация ирисов:
colors <- c("#00AFBB", "#E7B800", "#FC4E07", "grey")
colors <- colors[as.numeric(data$Groups)]

head(num_data)
s3d <- scatterplot3d(
  num_data[1:3], 
  main= "Страны по группам уровня жизни", 
  pch = 16, 
  color=colors)

legend(
  s3d$xyz.convert(7.5, 3, 4.5), 
  legend = data$Groups,
  col = colors, 
  pch = 16)
