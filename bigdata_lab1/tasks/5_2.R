df <- data.frame(
	var1 = c(11, 21, 31),
	var2 = c(12, 22, 32),
	var3 = c(13, 23, 33),
	var4 = c(14, 24, 34),
	row.names = c("case1", "case2", "case3")
)
df
df["case1", c("var1", "var2", "var3")]

Filter(function(item) item > 22, as.numeric(df["case2",]))

colnames(df)[c(1, 3)]

df$Y <- c(-1, 0, 1)
df

df <- df[-which(rownames(df) == "case2"),]
df

df$var2 <- df$var2^3
df