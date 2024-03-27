# 2. функциональные графики - тенденции изменения количества призовых мест отдельно по мужчинам и женщинам за последние 30 лет.

data_frame_male <- read.csv('../bigdata_moment/bigdata_lab4/resources/ger_swim_m.csv')
data_frame_female <- read.csv('../bigdata_moment/bigdata_lab4/resources/ger_swim_f.csv')

#выделим призовые места
data_prize_male <- data.frame(year=data_frame_male$year, award=rowSums(data_frame_male[, 2:4]))
data_frame_female <- data.frame(year=data_frame_female$year, award=rowSums(data_frame_female[, 2:4]))

data_prize_male

plot(
  data_prize_male,
  type="b",
  pch=19,
  col="navyblue",
  xaxt="n",
  ylim=c(0,4),
  xlab = 'Год',
  ylab = 'Количество наград',
  main="Германия, Плавание, Мужчины"
)
axis(
  side=1,
  at=data_prize_male$year
)

plot(
  data_frame_female,
  type="b",
  pch=19,
  col="navyblue",
  xaxt="n",
  ylim=c(0,10),
  xlab = 'Год',
  ylab = 'Количество наград',
  main="Германия, Плавание, Женщины"
)
axis(
  side=1,
  at=data_frame_female$year
)