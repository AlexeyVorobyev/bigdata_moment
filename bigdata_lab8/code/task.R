library(car)
library(ellipse)
library(ggplot2)

df1_initial <- read.csv("../bigdata_moment/bigdata_lab8/data/LR8_dataset.csv", sep = ",", header = TRUE)
years <- 1989:2018

data_set <- df1_initial[df1_initial$Country.Code == "USA",]


for (i in 1:23) {
  for (j in 5:34) {
    if (data_set[i, j] != "..") {
      data_set[i, j] <- as.numeric(data_set[i, j])
    }
    else {
      data_set[i, j] <- NA
    }

  }
}

VVP <- rep(0, 30)

for (i in 5:34) {
  VVP[i - 4] =  as.numeric(data_set[2, i])
}

Population_growth <- rep(0, 30)

for (i in 5:34) {
  Population_growth[i - 4] = as.numeric(data_set[14, i])
}

VVP_growth <- rep(0, 30)

for (i in 5:34) {
  VVP_growth[i - 4] = as.numeric(data_set[1, i])
}


VVP_growth <- VVP_growth / 1000000


for (i in 1:30) {
  if (is.na(VVP[i])) {
    VVP[i] = median(VVP, na.rm = TRUE)
  }
  if (is.na(Population_growth[i])) {
    Population_growth[i] = median(Population_growth, na.rm = TRUE)
  }

}

Changes_medical_cost <- rep(0, 30)

for (i in 5:34) {
  Changes_medical_cost[i - 4] = as.numeric(data_set[12, i])
}

Average_life <- rep(0, 30)

for (i in 5:34) {
  Average_life[i - 4] = as.numeric(data_set[13, i])
}

Death_rate <- rep(0, 30)

for (i in 5:34) {
  Death_rate[i - 4] = as.numeric(data_set[18, i])
}

for (i in 1:30) {
  if (is.na(Changes_medical_cost[i])) {
    Changes_medical_cost[i] = median(Changes_medical_cost, na.rm = TRUE)
  }
  if (is.na(Average_life[i])) {
    Average_life[i] = median(Average_life, na.rm = TRUE)
  }
  if (is.na(Death_rate[i])) {
    Death_rate[i] = median(Death_rate, na.rm = TRUE)
  }
}

Higher_education_growth <- rep(0, 30)

for (i in 5:34) {
  Higher_education_growth[i - 4] = as.numeric(data_set[19, i])
}

Growth_exports_goods <- rep(0, 30)

for (i in 5:34) {
  Growth_exports_goods[i - 4] = as.numeric(data_set[17, i])
}

Growth_technical_production <- rep(0, 30)

for (i in 5:34) {
  Growth_technical_production[i - 4] = as.numeric(data_set[21, i])
}

for (i in 1:30) {
  if (is.na(Higher_education_growth[i])) {
    Higher_education_growth[i] = median(Higher_education_growth, na.rm = TRUE)
  }
  if (is.na(Growth_exports_goods[i])) {
    Growth_exports_goods[i] = median(Growth_exports_goods, na.rm = TRUE)
  }
  if (is.na(Growth_technical_production[i])) {
    Growth_technical_production[i] = median(Growth_technical_production, na.rm = TRUE)
  }
}


Edication_cost <- rep(0, 30)

for (i in 5:34) {
  Edication_cost[i - 4] = as.numeric(data_set[15, i])
}

Female_bachelors <- rep(0, 30)

for (i in 5:34) {
  Female_bachelors[i - 4] = as.numeric(data_set[20, i])
}

for (i in 1:30) {
  if (is.na(Edication_cost[i])) {
    Edication_cost[i] = median(Edication_cost, na.rm = TRUE)
  }
  if (is.na(Female_bachelors[i])) {
    Female_bachelors[i] = median(Female_bachelors, na.rm = TRUE)
  }
}

Scientific_Articles <- rep(0, 30)

for (i in 5:34) {
  Scientific_Articles[i - 4] = as.numeric(data_set[23, i])
}

for (i in 1:30) {
  if (is.na(Scientific_Articles[i])) {
    Scientific_Articles[i] = median(Scientific_Articles, na.rm = TRUE)
  }
}

df2 = data.frame(VVP, Population_growth, Growth_exports_goods, Scientific_Articles, Growth_technical_production, Female_bachelors, Changes_medical_cost, Death_rate, Average_life)

plotcorr(cor(df2))

print("Average_life")
shapiro.test(Average_life)
print("population_growth")
shapiro.test(Population_growth)

cor(Average_life, Population_growth, method = "spearman")
cor.test(Average_life, Population_growth)

plot(
    years,
    Average_life,
    xlab = 'Year',
    ylab = '%',
    main = 'Average_life, population_growth',
    col = 'blue',
    type = 'b',
    lty = 1,
    pch = 1,
    lwd = 2,
    ylim = c(-4,100)
)
lines(years, Population_growth, type='b', col='green', lty=1, pch=1, lwd=2)

print("Growth_technical_production")
shapiro.test(Growth_technical_production)
print("population_growth")
shapiro.test(Population_growth)

cor(Growth_technical_production, Population_growth, method = "spearman")
cor.test(Growth_technical_production, Population_growth)

plot(
    years,
    Growth_technical_production,
    xlab = 'Year',
    ylab = '%',
    main = 'Growth_technical_production, population_growth',
    col = 'blue',
    type = 'b',
    lty = 1,
    pch = 1,
    lwd = 2,
    ylim = c(-4,100)
)
lines(years, Population_growth, type='b', col='green', lty=1, pch=1, lwd=2)

print("Average_life")
shapiro.test(Average_life)
print("Growth_technical_production")
shapiro.test(Growth_technical_production)

cor(Average_life, Growth_technical_production, method = "spearman")
cor.test(Average_life, Growth_technical_production)

plot(
    years,
    Average_life,
    xlab = 'Year',
    ylab = '%',
    main = 'Average_life, Growth_technical_production',
    col = 'blue',
    type = 'b',
    lty = 1,
    pch = 1,
    lwd = 2,
    ylim = c(-4,100)
)
lines(years, Growth_technical_production, type='b', col='green', lty=1, pch=1, lwd=2)

print("Average_life")
shapiro.test(Average_life)
print("Death_rate")
shapiro.test(Death_rate)

cor(Average_life, Death_rate, method = "spearman")
cor.test(Average_life, Death_rate)

plot(
    years,
    Average_life,
    xlab = 'Year',
    ylab = '%',
    main = 'Average_life, Death_rate',
    col = 'blue',
    type = 'b',
    lty = 1,
    pch = 1,
    lwd = 2,
    ylim = c(-4,100)
)
lines(years, Death_rate, type='b', col='green', lty=1, pch=1, lwd=2)


states <- as.data.frame(df2[, c('VVP', 'Population_growth',
                                'Growth_exports_goods', 'Scientific_Articles',
                                'Growth_technical_production',
                                'Changes_medical_cost',
                                'Death_rate', 'Average_life')])


fit <- lm(Death_rate ~ Average_life, data = states)
fit
plot(
    states$`Average_life`,
    states$`Death_rate`,
    xaxt = "n",
    xlab = "Average_life",
    ylab = "Death_rate",
    col = "blue",
    main = "Real data"
)
abline(fit, col = "red")

fit <- lm(Population_growth ~ Average_life, data = states)
fit
plot(
    states$`Average_life`,
    states$Population_growth,
    xaxt = "n",
    xlab = "Average_life",
    ylab = "Population_growth",
    col = "blue",
    main = "Real data"
)
abline(fit, col = "red")

fit <- lm(Population_growth ~ Growth_technical_production, data = states)
fit
plot(
    states$Growth_technical_production,
    states$Population_growth,
    xaxt = "n",
    xlab = "Growth_technical_production",
    ylab = "Population_growth",
    col = "blue",
    main = "Real data"
)
abline(fit, col = "red")


fit <- lm(Average_life ~ Growth_technical_production, data = states)
fit
plot(
    states$Growth_technical_production,
    states$Average_life,
    xaxt = "n",
    xlab = "Growth_technical_production",
    ylab = "Average_life",
    col = "blue",
    main = "Real data"
)
abline(fit, col = "red")



pred <- predict(fit, states)
plot(
    pred,
    xaxt = "n",
    xlab = "Growth_technical_production",
    ylab = "Average_life",
    col = "blue",
    main = "Predict"
)