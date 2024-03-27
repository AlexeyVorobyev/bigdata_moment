# Столбчатая диаграмма по количеству мест 1-8 (спортсменов заданных стран) по каждой Олимпиаде по назначенному виду спорта за всё время

data_frame_male <- read.csv('../bigdata_moment/bigdata_lab4/resources/ger_swim_m.csv')
data_frame_female <- read.csv('../bigdata_moment/bigdata_lab4/resources/ger_swim_f.csv')

# Сумма всех медалей по каждому месту

sum_medals_by_places_male <- sapply(data_frame_male[,-1], sum)

barplot(
  sum_medals_by_places_male,
  names.arg = 1:8,
  col="blue",
  xlab="Место",
  ylab="Количество",
  main="Мужчины (за все время)"
)

sum_medals_by_places_female <- sapply(data_frame_female[,-1], sum)

barplot(
  sum_medals_by_places_female,
  names.arg = 1:8,
  col="blue",
  xlab="Место",
  ylab="Количество",
  main="Женщины (за все время)"
)

