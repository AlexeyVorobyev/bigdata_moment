source('../bigdata_moment/bigdata_lab3/tasks/import.R')

data <- data_serials[,3:12]

# ���������� ���������, �� � ������ ������� ������ 6 ������ ���������
head(data)

# �������� �������������� ���-��
summary(data)