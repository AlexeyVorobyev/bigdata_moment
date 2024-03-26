source('../bigdata_moment/bigdata_lab2/tasks/sorted_list_desc.R')

par(mar=c(5,20,4,4))

barplot(
  unlist(sorted_data, use.names = FALSE),
  xlab = "Оценка",
  names.arg = names(sorted_data),
  col = "steelblue",
  ylim=c(0,11),
  horiz = TRUE,
  las=1,
  beside = TRUE,
)
