source('../bigdata_moment/bigdata_lab2/utils/transform-data-to-named-list.R')

data <- read.csv('../bigdata_moment/bigdata_lab2/resources/form/serials.csv')

data_named_list <- transform_data_to_named_list(data)

max_data <- Map(
  function (item) max(item),
  data_named_list
)

min_data <- Map(
  function (item) min(item),
  data_named_list
)

mean_data <- Map(
  function (item) mean(item),
  data_named_list
)

print('Максимальные значения:')
print('----------------------')
print(max_data)
print('----------------------')

print('Минимальные значения:')
print('----------------------')
print(min_data)
print('----------------------')

print('Средние значения:')
print('----------------------')
print(mean_data)
