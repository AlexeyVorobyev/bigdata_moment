source('../bigdata_moment/bigdata_lab2/tasks/sorted_list_desc.R')

par(mar=c(5,20,4,4))

source('../bigdata_moment/bigdata_lab2/utils/transform-data-to-named-list.R')

data <- read.csv('../bigdata_moment/bigdata_lab2/resources/form/serials.csv')

data_named_list <- transform_data_to_named_list(data)
boxplot(data_named_list, las = 2, xlab = "", ylab = "", col = rainbow(6))