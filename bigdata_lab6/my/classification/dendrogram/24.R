library(dendextend)
library(lattice)


df_24 <- read.csv('../bigdata_moment/bigdata_lab6/my/data/prepared/countries_24.csv')


dist.df_24 <- dist(df_24[, 2:7])
clust.df_24 <- hclust(dist.df_24, "ward.D")
plot(clust.df_24, labels = df_24$Countries, main = "Dendrogram for 24 year", ylab = "Similarity", xlab = "Countries")

k_24 = 6
rect.hclust(clust.df_24, k_24, border = "red")
abline(h = 1.65, col = "blue", lwd = '3')
plot(1:50, clust.df_24$height, type = 'b',main = "Scree diagram for 24 year", xlab = "Number of component", ylab = "Self value")
abline(h = 1.65, col = "blue", lwd = '3')
groups <- cutree(clust.df_24, k_24)
dend <- as.dendrogram(clust.df_24)
dend <- color_branches(dend, k_24)
plot(dend)


df_24[groups == 1, 1]
df_24[groups == 2, 1]
df_24[groups == 3, 1]
df_24[groups == 4, 1]
df_24[groups == 5, 1]
df_24[groups == 6, 1]

g1_24 <- colMeans(df_24[groups == 1, 2:8])
g2_24 <- colMeans(df_24[groups == 2, 2:8])
g3_24 <- colMeans(df_24[groups == 3, 2:8])
g4_24 <- colMeans(df_24[groups == 4, 2:8])
g5_24 <- colMeans(df_24[groups == 5, 2:8])
g6_24 <- colMeans(df_24[groups == 6, 2:8])

df_24_groups <- data.frame(g1_24, g2_24, g3_24, g4_24, g5_24,g6_24)
df1_groups <- t(df_24_groups)

barplot(as.matrix(df_24_groups), col = c("magenta", "red", "yellow", "blue", "green", "orange", "black"), main = 'Stack diagram on all')
legend("top", cex = 1, rownames(df_24_groups), fill = c("magenta", "red", "yellow", "blue", "green", "orange", "black"))

barplot(df1_groups[, 1], ylim = range(pretty(c(0, max(df1_groups[, 1])))),
        main = "Purchasing Power Index",
        col = c("magenta", "red", "yellow", "blue", "green","black"), legend = rownames(df1_groups))

barplot(df1_groups[, 2], ylim = range(pretty(c(0, max(df1_groups[, 2])))),
        main = "Safety index",
        col = c("magenta", "red", "yellow", "blue", "green","black"), legend = rownames(df1_groups))

barplot(df1_groups[, 3], ylim = range(pretty(c(0, max(df1_groups[, 3])))),
        main = "Health care index",
        col = c("magenta", "red", "yellow", "blue", "green","black"), legend = rownames(df1_groups))

barplot(df1_groups[, 4], ylim = range(pretty(c(0, max(df1_groups[, 4])))),
        main = "Cost of living index",
        col = c("magenta", "red", "yellow", "blue", "green","black"), legend = rownames(df1_groups))

barplot(df1_groups[, 5], ylim = range(pretty(c(0, max(df1_groups[, 5])))),
        main = "Property price to income ratio",
        col = c("magenta", "red", "yellow", "blue", "green","black"), legend = rownames(df1_groups))

barplot(df1_groups[, 6], ylim = range(pretty(c(0, max(df1_groups[, 6])))),
        main = "Traffic commute time index",
        col = c("magenta", "red", "yellow", "blue", "green","black"), legend = rownames(df1_groups))

barplot(df1_groups[, 7], ylim = range(pretty(c(0, max(df1_groups[, 7])))),
        main = "Pollution index",
        col = c("magenta", "red", "yellow", "blue", "green","black"), legend = rownames(df1_groups))

df_24["Group"] <- groups

xyplot(
  Health.Care.Index ~ Safety.Index,
  group = Group, data = df_24,
  auto.key = TRUE,
  pch = 20,
  cex = 1.5,
  panel=function(x, y, ...) {
    panel.xyplot(x, y, ...);
    ltext(x=x, y=y, labels=df_24$Countries_24, pos=1, offset=1, cex=0.8)
  }
)

xyplot(
  Traffic.Commute.Time.Index ~ Pollution.Index,
  group = Group, data = df_24,
  auto.key = TRUE,
  pch = 20,
  cex = 1.5,
  panel=function(x, y, ...) {
    panel.xyplot(x, y, ...);
    ltext(x=x, y=y, labels=df_24$Countries_24, pos=1, offset=1, cex=0.8)
  }
)

df_24_inv <- df_24
df_24_inv$Pollution.Index <- abs(1 - df_24_inv$Pollution.Index)

xyplot(
  Health.Care.Index ~ Safety.Index + Pollution.Index | Group,
  data = df_24_inv,
  auto.key = TRUE,
  pch = 20,
  cex = 1.5,
  grid = T,
  panel=function(x, y, ...) {
    panel.xyplot(x, y, ...);
    ltext(x=x, y=y, labels=df_24$Countries_24, pos=1, offset=1, cex=0.8)
  }
)

cloud(
  Health.Care.Index ~ Safety.Index * Pollution.Index,
  group = Group,
  data = df_24_inv,
  auto.key = TRUE,
  pch = 20,
  cex = 1.5,
)
