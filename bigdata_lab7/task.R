# Задание 1.

df <- read.table("../bigdata_moment/bigdata_lab7/athlete_events.csv", sep = ",", header = TRUE)

swimming <- df[which(df[, "Sport"] == "Swimming"), c("Name", "Sex", "Weight", "Sport")]

swimming <- swimming[-which(is.na(swimming[, "Weight"])),]

swimming <- unique(swimming)

x <- swimming[1:nrow(swimming), "Weight"]


shapiro.test(x)

x2 <- seq(min(x), max(x), length = length(x))
fun <- dnorm(x2, mean = mean(x), sd = sd(x))
hist(x, freq = FALSE, col = "gray", main = "Weight histogram", xlab = "Weight value", ylab = "Frequency")
lines(x2, fun, col = 2, lwd = 2)

qqnorm(x)
qqline(x, col = 4, lwd = 2)

t.test(x, mu = 70, conf.int = TRUE)

wilcox.test(x, mu = mean(x), conf.int = TRUE)


wrest_swimming <- df[which(df[, "Sport"] %in% c("Wrestling", "Swimming")), c("Name", "Sex", "Weight", "Sport")]

wrest_swimming <- wrest_swimming[-which(is.na(wrest_swimming[, "Weight"])),]

wrest_swimming <- wrest_swimming[which(wrest_swimming[, "Sex"] == "F"),]

wrest_swimming$Sport <- factor(wrest_swimming$Sport)
wrest_swimming$Sport <- droplevels(wrest_swimming$Sport)
wrest_swimming <- unique(wrest_swimming)

x4 <- wrest_swimming[1:nrow(wrest_swimming), "Weight"]

shapiro.test(x4)

qqnorm(x4)
qqline(x4, col = 4, lwd = 2)

fligner.test(wrest_swimming$Weight ~ wrest_swimming$Sport, wrest_swimming)

t.test(wrest_swimming$Weight ~ wrest_swimming$Sport, paired = FALSE, var.equal = TRUE)