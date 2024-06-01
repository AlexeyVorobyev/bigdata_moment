library(dplyr)

df <- read.csv('../bigdata_moment/bigdata_lab6/normal/data/raw/customers.csv')

df_prepared_columns <- df %>% select(-c("CustomerID","Profession"))

df_24_prepared_columns_filtered <- df_prepared_columns %>% mutate(Gender = ifelse(Gender == "Male",1,0))

CustomerID <- df$CustomerID

tmp1_24 <- scale(df_24_prepared_columns_filtered[, 2:6], center = TRUE, scale = TRUE)
maxs_24 <- apply(tmp1_24, 2, max)
mins_24 <- apply(tmp1_24, 2, min)
tmp1_24 <- scale(tmp1_24, center = mins_24, scale = maxs_24 - mins_24)
df_24_prepared_columns_filtered_normalized <- data.frame(CustomerID,tmp1_24)

write.csv(df_24_prepared_columns_filtered_normalized, '../bigdata_moment/bigdata_lab6/normal/data/prepared/customers.csv', row.names=FALSE)
