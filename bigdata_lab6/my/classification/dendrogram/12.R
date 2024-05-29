library(dendextend)

df_12 <- read.csv('../bigdata_moment/bigdata_lab6/my/data/prepared/countries_12.csv')


dist.df_12 <- dist(df_12[, 2:7])
clust.df_12 <- hclust(dist.df_12, "ward.D")
plot(clust.df_12, labels = df_12$Countries, main = "Dendrogram for 12 year", ylab = "Similarity", xlab = "Countries")

k_12 = 4
rect.hclust(clust.df_12, k_12, border = "red")
abline(h = 1.6, col = "blue", lwd = '3')
plot(1:50, clust.df_12$height, type = 'b',main = "Scree diagram for 12 year", xlab = "Number of component", ylab = "Self value")
abline(h = 1.6, col = "blue", lwd = '3')
groups <- cutree(clust.df_12, k_12)
dend <- as.dendrogram(clust.df_12)
dend <- color_branches(dend, k_12)
plot(dend)

df_12[groups == 1, 1]
df_12[groups == 2, 1]
df_12[groups == 3, 1]
df_12[groups == 4, 1]
df_12[groups == 5, 1]
df_12[groups == 6, 1]

g1_12 <- colMeans(df_12[groups == 1, 2:8])
g2_12 <- colMeans(df_12[groups == 2, 2:8])
g3_12 <- colMeans(df_12[groups == 3, 2:8])
g4_12 <- colMeans(df_12[groups == 4, 2:8])

df_12_groups <- data.frame(g1_12, g2_12, g3_12, g4_12)

barplot(as.matrix(df_12_groups), col = c("magenta", "red", "yellow", "blue", "green", "orange","black"), main = 'Stack diagram on all')
legend("top", cex = 1, rownames(df_12_groups), fill = c("magenta", "red", "yellow", "blue", "green", "orange","black"))

df1_groups <- t(df_12_groups)

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

df_12["Group"] <- groups

xyplot(
  Health.Care.Index ~ Safety.Index,
  group = Group, data = df_12,
  auto.key = TRUE,
  pch = 20,
  cex = 1.5,
  panel=function(x, y, ...) {
    panel.xyplot(x, y, ...);
    ltext(x=x, y=y, labels=df_12$Countries_12, pos=1, offset=1, cex=0.8)
  }
)

xyplot(
  Traffic.Commute.Time.Index ~ Pollution.Index,
  group = Group, data = df_12,
  auto.key = TRUE,
  pch = 20,
  cex = 1.5,
  panel=function(x, y, ...) {
    panel.xyplot(x, y, ...);
    ltext(x=x, y=y, labels=df_12$Countries_12, pos=1, offset=1, cex=0.8)
  }
)

df_12_inv <- df_12
df_12_inv$Pollution.Index <- abs(1 - df_12_inv$Pollution.Index)

xyplot(
  Health.Care.Index ~ Safety.Index + Pollution.Index | Group,
  data = df_12_inv,
  auto.key = TRUE,
  pch = 20,
  cex = 1.5,
  grid = T,
  panel=function(x, y, ...) {
    panel.xyplot(x, y, ...);
    ltext(x=x, y=y, labels=df_12$Countries_12, pos=1, offset=1, cex=0.8)
  }
)

cloud(
  Health.Care.Index ~ Safety.Index * Pollution.Index,
  group = Group,
  data = df_12_inv,
  auto.key = TRUE,
  pch = 20,
  cex = 1.5,
)