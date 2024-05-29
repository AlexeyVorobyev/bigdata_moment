library(klaR)
library(randomForest)
library(party)



df_24 <- read.csv('../bigdata_moment/bigdata_lab6/my/data/prepared/countries_24.csv')

dist.city <- dist(df_24[, 2:7])

dist.df_24 <- dist(df_24[, 2:7])
clust.df_24 <- hclust(dist.df_24, "ward.D")
k_24 = 6
groups <- cutree(clust.df_24, k_24)

df_24$Group <- as.factor(groups)

# naive_df <- NaiveBayes(df_24$Group ~ ., data = df_24)
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


set.seed(1234)
ind <- sample(2, nrow(df_24), replace = TRUE, prob = c(0.7, 0.3))
trainData <- df_24[ind == 1,]
testData <- df_24[ind == 2,]
nrow(trainData)
nrow(testData)
nrow(df_24)

myFormula <- Group ~ Safety.Index +
	Purchasing.Power.Index +
	Health.Care.Index +
	Cost.of.Living.Index +
	Traffic.Commute.Time.Index +
	Property.Price.to.Income.Ratio +
    Pollution.Index
df_ctree <- ctree(myFormula, data = trainData)
df_ctree
table(predict(df_ctree), trainData$Group)
predict(df_ctree)
plot(df_ctree)

rf <- randomForest(Group ~ ., data = trainData, ntree = 100, proximity = TRUE)
table(predict(rf), trainData$Group)
print(rf)