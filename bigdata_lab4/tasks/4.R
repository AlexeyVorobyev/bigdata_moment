data_frame_male <- read.csv('../bigdata_moment/bigdata_lab4/resources/ger_swim_m.csv')
data_frame_female <- read.csv('../bigdata_moment/bigdata_lab4/resources/ger_swim_f.csv')

prize_m <- data.frame(Год=data_frame_male$year, Призовых=rowSums(data_frame_male[, 2:4]))
prize_f <- data.frame(Год=data_frame_female$year, Призовых=rowSums(data_frame_female[, 2:4]))

prize_6_m <- tail(prize_m, 6)
prize_6_f <- tail(prize_f, 6)

par(
  mfrow=c(3,1)

)
plot(prize_6_m, type="b", pch=19, col="navyblue", xaxt="n", ylim=c(0,7), main="Призовые места Германии по плаванью\nза последние 6 ОИ")
lines(prize_6_f, type="o", pch=11, col="hotpink")
legend(min(prize_6_f$Год), 7.2, cex=0.7 ,c("Мужчины", "Женщины"), fill=c("navyblue", "hotpink"))
axis(side=1, at=prize_m$Год)

prize_grouped <- data.frame(Призовых_М=prize_6_m$Призовых, Призовых_Ж=prize_6_f$Призовых)
barplot(height=t(as.matrix(prize_grouped)), beside=TRUE, xlab="Год", ylab="Количество", names.arg=prize_6_f$Год, col=c("navyblue", "hotpink"), main="Количество призовых мест Германии\nпо плаванью за последние 6 ОИ")

prize_6_sum <- sapply(prize_grouped, sum)
pie(prize_6_sum, labels=c(prize_6_sum["Призовых_М"], prize_6_sum["Призовых_Ж"]), col=c("navyblue", "hotpink"), main="Всего призовых мест у М и Ж из Германии\nпо плаванью за последние 6 ОИ")