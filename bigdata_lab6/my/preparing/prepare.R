library(dplyr)

df_24 <- read.csv('../bigdata_moment/bigdata_lab6/my/data/raw/countries_24.csv')
df_12 <- read.csv('../bigdata_moment/bigdata_lab6/my/data/raw/countries_12.csv')

df_24_prepared_columns <- df_24 %>% select(- c("Quality.of.Life.Index", "Climate.Index"))
df_12_prepared_columns <- df_12 %>% select(- c("Quality.of.Life.Index", "Climate.Index"))

df_24_prepared_columns_filtered <- filter(df_24_prepared_columns, Country %in% df_12_prepared_columns$Country)

Countries_24 <- df_24_prepared_columns_filtered$Country

tmp1_24 <- scale(df_24_prepared_columns_filtered[, 2:8], center = TRUE, scale = TRUE)
maxs_24 <- apply(tmp1_24, 2, max)
mins_24 <- apply(tmp1_24, 2, min)
tmp1_24 <- scale(tmp1_24, center = mins_24, scale = maxs_24 - mins_24)
df_24_prepared_columns_filtered_normalized <- data.frame(Countries_24, tmp1_24)

Countries_12 <- df_12_prepared_columns$Country

tmp1_12 <- scale(df_12_prepared_columns[, 2:8], center = TRUE, scale = TRUE)
maxs_12 <- apply(tmp1_12, 2, max)
mins_12 <- apply(tmp1_12, 2, min)
tmp1_12 <- scale(tmp1_12, center = mins_12, scale = maxs_12 - mins_12)
df_12_prepared_columns_normalized <- data.frame(Countries_12, tmp1_12)

write.csv(df_24_prepared_columns_filtered_normalized, "../bigdata_moment/bigdata_lab6/my/data/prepared/countries_24.csv", row.names=FALSE)
write.csv(df_12_prepared_columns_normalized, "../bigdata_moment/bigdata_lab6/my/data/prepared/countries_12.csv", row.names=FALSE)