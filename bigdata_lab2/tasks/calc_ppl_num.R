source('../bigdata_moment/bigdata_lab2/utils/transform-data-to-named-list.R')

data <- read.csv('../bigdata_moment/bigdata_lab2/resources/form/serials.csv')

data_named_list <- transform_data_to_named_list(data)

num_ppl_between <- Map(
  function (item) length(Filter(
    function (item) item > 7 || item < 3,
    item
  )),
  data_named_list
)

num_ppl_between