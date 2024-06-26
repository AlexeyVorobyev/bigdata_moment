source('../bigdata_moment/bigdata_lab3/tasks/import.R')

data <- data_serials[,3:12]

# ������� subdataset �� ���, ��� �� ������� ������ ������� ������ � ������
love_death_robots_ns <- subset(data, "������..������.�.������" == 0)

love_death_robots_ns

# ����������� �����������
dim(love_death_robots_ns)

hist(love_death_robots_ns$`������.�������`, main="rus", xlab="�����", ylab="�������")

library(dplyr)
compare_them <- function(data1,data2) {
  sum1 <- apply(data1,2,summary) %>% data.frame()
  sum2 <- apply(data2,2,summary) %>% data.frame()

  names(sum1) <- paste0(names(sum1),"1")
  names(sum2) <- paste0(names(sum2),"2")

  final <- cbind(sum1,sum2)

  final1 <- t(final)

  final2 <- final1[order(row.names(final1)), ]

  final_1 <- t(final2) %>% data.frame()
  final_1
}

compare_them(love_death_robots_ns,data) %>% View()