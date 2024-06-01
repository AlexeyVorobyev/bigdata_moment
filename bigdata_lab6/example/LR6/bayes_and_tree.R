#install.packages("klaR")
library(klaR)

#ЧАСТЬ 2 

data <- read.csv("zzz.csv", header = TRUE)

data <- data[,-8]

groups
my_data <- data
my_data$Group<- c(as.factor(groups))

naive_df <- NaiveBayes(my_data$Group ~ ., data = my_data)
naive_df$tables
naive_df$tables$доход
naive_df

#делаем графики по байсу
opar=par() 
opar
layout(matrix(c(1,2,3,4), 2, 2)) 
plot(naive_df, lwd = 2, legendplot = FALSE)
legend("topleft",lty=1:3, cex=0.5)
#восстановление графика
par=opar

# Предсказание - результаты по Байесу
pred <- predict(naive_df, my_data)$class

(table(Факт = my_data$Group, Прогноз = pred)) 

# Классификация Decision Tree

#install.packages("party")
library(party)

# Разделяем данные на два датасета
set.seed(1234)
ind <- sample(2, nrow(my_data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- my_data[ind==1,]
testData <- my_data[ind==2,] 
nrow(trainData)
nrow(testData)
nrow(my_data)

# Строим модель

my_data
myFormula <- Group ~ рождаем + смертн + деск_см + длит_муж + длит_жен + доход
df_ctree <- ctree(myFormula, data=trainData)
df_ctree
predict(df_ctree)
table(predict(df_ctree, newdata=testData), testData$Group) 
predict(df_ctree)
plot(df_ctree)

# Алгоритм Random Forest
# install.packages("randomForest")
library(randomForest)

rf <- randomForest(Group ~ .,data=trainData, ntree=100, proximity=TRUE)
table(predict(rf, newdata=testData), testData$Group)
print(rf)
