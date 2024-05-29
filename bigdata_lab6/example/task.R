library(lattice)
library(scatterplot3d)
library(dendextend)
library(farver)
library(labeling)
require(installr)
suppressPackageStartupMessages(library(installr))
require2(colorspace)
library(dplyr)
library(tidyr)
library(tibble)
library(vctrs)
library(tibble)
library(klaR)
library(party)
library(randomForest)
library(vctrs)
library(tibble)
library(ggplot2)

inf1 <- read.csv("../bigdata_moment/bigdata_lab6/example/countries_life_rating_1990.csv")
inf1 <- inf1[, -8]

inf1$income[inf1$income == -9999] <- NA
inf1 <- na.omit(inf1)

inf123 <- inf1
Countries <- inf1$country


inf2 <- scale(inf1[, 2:7], center = TRUE, scale = TRUE)

inf2

inf2 <- inf1[, -1]
maxs <- apply(inf2, 2, max)
mins <- apply(inf2, 2, min)

inf2 <- scale(inf2, center = mins, scale = maxs - mins)

inf2 <- data.frame(Countries, inf2)

dist.city <- dist(inf2[, 2:7])

clust.city <- hclust(dist.city, "ward.D")

plot(clust.city, labels = inf1$country,main="Dendrogram",ylab="Similarity",xlab="Countries")

k = 5
rect.hclust(clust.city, k, border = "red")
abline(h = 1.5, col = "blue", lwd = '3')

plot(1:90, clust.city$height, type = 'b', xlab = "Number of component", ylab = "Self value")

groups <- cutree(clust.city, k)

dend <- as.dendrogram(clust.city)
dend <- color_branches(dend, k)
plot(dend)


inf1[groups == 1, 1]
inf1[groups == 2, 1]
inf1[groups == 3, 1]
inf1[groups == 4, 1]
inf1[groups == 5, 1]

g1 <- colMeans(inf1[groups == 1, 2:7])
g2 <- colMeans(inf1[groups == 2, 2:7])
g3 <- colMeans(inf1[groups == 3, 2:7])
g4 <- colMeans(inf1[groups == 4, 2:7])
g5 <- colMeans(inf1[groups == 5, 2:7])

g11 <- colMeans(inf2[groups == 1, 2:7])
g12 <- colMeans(inf2[groups == 2, 2:7])
g13 <- colMeans(inf2[groups == 3, 2:7])
g14 <- colMeans(inf2[groups == 4, 2:7])
g15 <- colMeans(inf2[groups == 5, 2:7])


df2 <- data.frame(g11, g12, g13, g14, g15)
df <- data.frame(g1, g2, g3, g4, g5)
df1 <- t(df2)

barplot(as.matrix(df2), col = c("magenta", "red", "yellow", "blue", "green", "orange"))
legend("topleft", cex = 0.6, rownames(df2), fill = c("magenta", "red", "yellow", "blue", "green", "orange"))

barplot(df1[, 1], ylim = range(pretty(c(0, max(df1[, 1])))),
		main = "Birth rate",
		col = c("magenta", "red", "yellow", "blue", "green"), legend = rownames(df1))

barplot(df1[, 2], ylim = range(pretty(c(0, max(df1[, 2])))),
		main = "Death rate",
		col = c("magenta", "red", "yellow", "blue", "green"), legend = rownames(df1))

barplot(df1[, 3], ylim = range(pretty(c(0, max(df1[, 3])))),
		main = "Child mortality rate",
		col = c("magenta", "red", "yellow", "blue", "green"), legend = rownames(df1))

barplot(df1[, 4], ylim = range(pretty(c(0, max(df1[, 4])))),
		main = "Man lifespan",
		col = c("magenta", "red", "yellow", "blue", "green"), legend = rownames(df1))

barplot(df1[, 5], ylim = range(pretty(c(0, max(df1[, 5])))),
		main = "Woman lifespan",
		col = c("magenta", "red", "yellow", "blue", "green"), legend = rownames(df1))

barplot(df1[, 6], ylim = range(pretty(c(0, max(df1[, 6])))),
		main = "income",
		col = c("magenta", "red", "yellow", "blue", "green"), legend = rownames(df1))

inf123["Group"] <- groups

xyplot(birth_rate ~ death_rate, group = Group, data = inf123, auto.key = TRUE, pch = 20, cex = 1.5)

boxplot(death_rate ~ Group, data = inf123, ylab = "Mortality", frame = FALSE, col = rainbow(3))

xyplot(death_rate ~ man_lifespan + woman_lifespan | Group, data = inf123, grid = T, auto.key = TRUE, pch = 20, cex = 1.5)

cloud(birth_rate ~ death_rate * child_death_rate, group = Group, data = inf123, auto.key = TRUE, pch = 20, cex = 1.5)

inf123 %>%
	ggplot(aes(birth_rate, death_rate, color = Group)) + geom_point()

# city.01 <- read.csv("countries_life_rating_1990.csv")
# city.01
# city.01 <- city.01[, -8]
#
# city.01$income[city.01$income == -9999] <- NA
# city.01 <- na.omit(city.01)
#
# my_data <- city.01[, -8]
# my_data
# groups
# my_data$Group <- as.factor(groups)

# naive_df <- NaiveBayes(my_data$Group ~ ., data = my_data)
# naive_df$tables
# naive_df$tables$Work
# naive_df
#
# opar = par()
# opar
# layout(matrix(c(1, 2, 3, 4), 2, 2))
# plot(naive_df, lwd = 2, legendplot = FALSE)
# legend("topleft", lty = 1:3, cex = 0.5)
# par = opar


# set.seed(1234)
# ind <- sample(2, nrow(my_data), replace = TRUE, prob = c(0.7, 0.3))
# trainData <- my_data[ind == 1,]
# testData <- my_data[ind == 2,]
# nrow(trainData)
# nrow(testData)
# nrow(my_data)
#
# my_data
# myFormula <- Group ~ birth_rate +
# 	death_rate +
# 	child_death_rate +
# 	man_lifespan +
# 	woman_lifespan +
# 	income
# df_ctree <- ctree(myFormula, data = trainData)
# df_ctree
# table(predict(df_ctree), trainData$Group)
# predict(df_ctree)
# plot(df_ctree)
#
# rf <- randomForest(Group ~ ., data = trainData, ntree = 100, proximity = TRUE)
# table(predict(rf), trainData$Group)
# print(rf)