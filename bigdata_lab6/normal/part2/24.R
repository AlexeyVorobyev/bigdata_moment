library(klaR)
library(randomForest)
library(party)



data <- read.csv('../bigdata_moment/bigdata_lab6/normal/data/prepared/customers.csv')

data <- data[,2:6]

data

k_24 = 4
dist.data <- dist(data)
clust.data <- hclust(dist.data, "ward.D")
groups <- cutree(clust.data, k_24)
my_data <- data
my_data$Group<- as.factor(groups)

# naive_df <- NaiveBayes(my_data$Group ~ ., data = my_data)
# naive_df$tables
# naive_df$tables$Annual.Income
# naive_df
#
# #делаем графики по байсу
# # opar=par()
# # opar
# # layout(matrix(c(1,2,3,4), 2, 2))
# # plot(naive_df, lwd = 2, legendplot = FALSE)
# # legend("topleft",lty=1:3, cex=0.5)
# # #восстановление графика
# # par=opar
#
# # Предсказание - результаты по Байесу
# pred <- predict(naive_df, my_data)$class
#
# (table(Fact = my_data$Group, Predict = pred))

# naive_df <- NaiveBayes(df_24$Group ~ ., data = df_24)
# naive_df$tables
# naive_df$tables$Work
# naive_df
#
# opar = par()
# opar
# layout(matrix(c(1, 2, 3, 4), 2, 2))
# plot(naive_df, lwd = 2, legendplot = FALSE)
# # legend("topleft", lty = 1:3, cex = 0.5)
# par = opar


set.seed(1234)
ind <- sample(2, nrow(my_data), replace = TRUE, prob = c(0.7, 0.3))
trainData <- my_data[seq(1, 1999, by = 2),]
testData <- my_data[seq(2, 1999, by = 2),]

print("what1")
trainData
print("what2")
testData

nrow(trainData)
nrow(testData)
nrow(my_data)

myFormula <- Group ~
	Spending.Score +
	Family.Size +
	Age
df_ctree <- ctree(myFormula, data = trainData)
df_ctree
table(predict(df_ctree,newdata=testData), testData$Group)
predict(df_ctree)
plot(df_ctree)
plot(df_ctree, type="simple")

rf <- randomForest(Group ~ ., data = trainData, ntree = 100, proximity = TRUE)
table(predict(rf, newData=testData), testData$Group)
print(rf)
