# Вывести графики изменения спортивных достижений 1) по золотым медалям

data_gold <- read.csv('../bigdata_moment/bigdata_lab4/resources/gold_medals.csv')

plot(
  data_gold$year,
  data_gold$USA, 
  type="b", 
  pch=19, 
  col="#3be8b0", 
  xaxt="n", 
  ylim=c(0,50), 
  xlab="year", 
  ylab="Золотых медалей", 
  main="Золотые медали за 6 последних олимпиад"
)

lines(
  data_gold$year,
  data_gold$China, 
  type="o", 
  pch=19, 
  col="#1aafd0"
)

lines(
  data_gold$year,
  data_gold$Japan, 
  type="o", 
  pch=19, 
  col="#6a67ce"
)

lines(
  data_gold$year,
  data_gold$GreatBritain,
  type="o",
  pch=19,
  col="#ffb900"
)

lines(
  data_gold$year,
  data_gold$Russia,
  type="o",
  pch=19,
  col="gray70"
)

lines(
  data_gold$year,
  data_gold$Australia,
  type="o",
  pch=19,
  col="#2e3c54"
)

lines(
  data_gold$year,
  data_gold$Netherlands,
  type="o",
  pch=19,
  col="brown"
)

axis(
  side=1,
  at=data_gold$year
)

legend(
  max(data_gold$year) - 1.5,
  y = 46,
  x = 2013,
  bty = 'n',
  col = c(2, 3),
  legend=c("США", "Китай", "Япония", "Великобритания", "Россия", "Австралия", "Нидерланды"),
  fill=c("#3be8b0", "#1aafd0", "#6a67ce", "#ffb900", "gray70", "#2e3c54", "brown")
)

