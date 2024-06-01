# Загрузка библиотек
library(readxl)
library(dplyr)
library(tidyr)
library(scales)

df <- read_excel("zzz.xls")

# Удаление строк с пропущенными значениями
df <- df[!(df$страна == 9999 | df$рождаем == 9999 | df$рождаем == -9999 | 
             df$смертн == 9999 | df$смертн == -9999 | df$деск_см == 9999 |
             df$деск_см == -9999 | df$длит_муж == 9999 | df$длит_муж == -9999 |
             df$длит_жен == 9999 | df$длит_жен == -9999 | df$доход == 9999 |
             df$доход == -9999 | df$регион == 9999 | df$регион == -9999), ]

summary(df)

# Нормализация числовых переменных
df$рождаем <- rescale(df$рождаем, to = c(0, 1))
df$смертн <- rescale(df$смертн, to = c(0, 1))
df$деск_см <- rescale(df$деск_см, to = c(0, 1))
df$длит_муж <- rescale(df$длит_муж, to = c(0, 1))
df$длит_жен <- rescale(df$длит_жен, to = c(0, 1))
df$доход <- rescale(df$доход, to = c(0, 1))

# Запись преобразованного DataFrame в файл "zzz.csv"
write.csv(df, "zzz.csv", row.names = FALSE)
