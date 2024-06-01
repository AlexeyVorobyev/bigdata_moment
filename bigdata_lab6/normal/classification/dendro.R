library(dendextend)
library(lattice)
library (factoextra)



df <- read.csv('../bigdata_moment/bigdata_lab6/normal/data/prepared/customers.csv')


dist.df <- dist(df[, 2:6])
clust.df <- hclust(dist.df, "ward.D")
plot(clust.df, labels = df$CustomerID, main = "Dendrogram", ylab = "Similarity", xlab = "Countries")

# length(clust.df$height)
# plot(1:1999, clust.df$height, type = 'b',main = "Scree diagram", xlab = "Number of component", ylab = "Self value")
# abline(h = k_24, col = "blue", lwd = '3')

# Метод локтя
fviz_nbclust(df[, 2:6], kmeans, method = "wss")

# Silhouette method
fviz_nbclust(df[, 2:6], kmeans, method = "silhouette") +
	labs(subtitle = "MeanMiddleSilouette")

k_24 = 4
rect.hclust(clust.df, k_24, border = "red")
abline(h = k_24, col = "blue", lwd = '3')
groups <- cutree(clust.df, k_24)
dend <- as.dendrogram(clust.df)
dend <- color_branches(dend, k_24)
plot(dend)


df[groups == 1, 1]
df[groups == 2, 1]
df[groups == 3, 1]
df[groups == 4, 1]

g1_24 <- colMeans(df[groups == 1, 2:6])
g2_24 <- colMeans(df[groups == 2, 2:6])
g3_24 <- colMeans(df[groups == 3, 2:6])
g4_24 <- colMeans(df[groups == 4, 2:6])

df_groups <- data.frame(g1_24, g2_24, g3_24, g4_24)
df1_groups <- t(df_groups)

barplot(as.matrix(df_groups), col = c("magenta", "red", "yellow", "blue", "green", "orange", "black"), main = 'Stack diagram on all')
legend("top", cex = 1, rownames(df_groups), fill = c("magenta", "red", "yellow", "blue", "green", "orange", "black"))

barplot(df1_groups[, 1], ylim = range(pretty(c(0, max(df1_groups[, 1])))),
        main = "Age",
        col = c("magenta", "red", "yellow", "blue", "green","black"), legend = rownames(df1_groups))

barplot(df1_groups[, 2], ylim = range(pretty(c(0, max(df1_groups[, 2])))),
        main = "Income",
        col = c("magenta", "red", "yellow", "blue", "green","black"), legend = rownames(df1_groups))

barplot(df1_groups[, 3], ylim = range(pretty(c(0, max(df1_groups[, 3])))),
        main = "Spending",
        col = c("magenta", "red", "yellow", "blue", "green","black"), legend = rownames(df1_groups))

barplot(df1_groups[, 4], ylim = range(pretty(c(0, max(df1_groups[, 4])))),
        main = "Work expirience",
        col = c("magenta", "red", "yellow", "blue", "green","black"), legend = rownames(df1_groups))

barplot(df1_groups[, 5], ylim = range(pretty(c(0, max(df1_groups[, 5])))),
        main = "Family size",
        col = c("magenta", "red", "yellow", "blue", "green","black"), legend = rownames(df1_groups))

df["Group"] <- groups

# xyplot(
# 	Work.Experience ~ Annual.Income,
#   group = Group, data = df,
#   auto.key = TRUE,
#   pch = 20,
#   cex = 1.5,
#   panel=function(x, y, ...) {
#     panel.xyplot(x, y, ...);
#     # ltext(x=x, y=y, labels=df$Countries_24, pos=1, offset=1, cex=0.8)
#   }
# )

xyplot(
	Spending.Score ~ Age,
	group = Group, data = df,
	auto.key = TRUE,
	pch = 20,
	cex = 1.5,
	panel=function(x, y, ...) {
		panel.xyplot(x, y, ...);
		# ltext(x=x, y=y, labels=df$Countries_24, pos=1, offset=1, cex=0.8)
	}
)

xyplot(
	Spending.Score ~ Annual.Income,
	group = Group, data = df,
	auto.key = TRUE,
	pch = 20,
	cex = 1.5,
	panel=function(x, y, ...) {
		panel.xyplot(x, y, ...);
		# ltext(x=x, y=y, labels=df$Countries_24, pos=1, offset=1, cex=0.8)
	}
)

xyplot(
	Spending.Score ~ Work.Experience,
	group = Group, data = df,
	auto.key = TRUE,
	pch = 20,
	cex = 1.5,
	panel=function(x, y, ...) {
		panel.xyplot(x, y, ...);
		# ltext(x=x, y=y, labels=df$Countries_24, pos=1, offset=1, cex=0.8)
	}
)

xyplot(
	Spending.Score ~ Family.Size,
	group = Group, data = df,
	auto.key = TRUE,
	pch = 20,
	cex = 1.5,
	panel=function(x, y, ...) {
		panel.xyplot(x, y, ...);
		# ltext(x=x, y=y, labels=df$Countries_24, pos=1, offset=1, cex=0.8)
	}
)

# cloud(
#   Annual.Income ~ Work.Experience  * Age ,
#   group = Group,
#   data = df,
#   auto.key = TRUE,
#   pch = 20,
#   cex = 1.5,
# )
#
# cloud(
# 	Spending.Score ~ Annual.Income  * Family.Size ,
# 	group = Group,
# 	data = df,
# 	auto.key = TRUE,
# 	pch = 20,
# 	cex = 1.5,
# )

