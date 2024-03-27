# 2. Круговую диаграмму по количеству первых мест в каждой из олимпиад

"%+%" <- function(...){
  paste0(...)
}

data_frame_male <- read.csv('../bigdata_moment/bigdata_lab4/resources/ger_swim_m.csv')
data_frame_female <- read.csv('../bigdata_moment/bigdata_lab4/resources/ger_swim_f.csv')

# Первые места по каждому году

library(RColorBrewer)
myPalette <- brewer.pal(5, "Set2")

first_places_male <- data_frame_male$X1
names(first_places_male) <- data_frame_male$year
first_places_male <- Filter(function (item) unname(item) != 0, first_places_male)

first_places_female <- data_frame_female$X1
names(first_places_female) <- data_frame_female$year
first_places_female <- Filter(function (item) unname(item) != 0, first_places_female)

pie(
  unname(first_places_male),
  labels = names(first_places_male) %+% Map(function (item) '(' %+% item %+% ')',unname(first_places_male)),
  border="white",
  col=myPalette
)

pie(
  unname(first_places_female),
  labels = names(first_places_female) %+% Map(function (item) '(' %+% item %+% ')',unname(first_places_female)),
  border="white",
  col=myPalette
)

