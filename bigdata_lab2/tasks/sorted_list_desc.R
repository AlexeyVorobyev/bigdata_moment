source('../bigdata_moment/bigdata_lab2/utils/transform-data-to-named-list.R')

data <- read.csv('../bigdata_moment/bigdata_lab2/resources/form/serials.csv')

data_named_list <- transform_data_to_named_list(data)

mean_data <- Map(
  function (item) mean(item),
  data_named_list
)

sorted_data <- mean_data[order(unlist(mean_data), decreasing=TRUE)]

sorted_data