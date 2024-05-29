library(rvest)

url_24<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2024')
url_12<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2012-Q1')

nodes_24<-html_nodes(url_24, 'table')
nodes_12<-html_nodes(url_12, 'table')

df_24<-html_table(nodes_24[[2]])%>%as.data.frame()
df_12<-html_table(nodes_12[[2]])%>%as.data.frame()

rownames(df_24)<-df_24[, 2]
rownames(df_12)<-df_12[, 2]

df_24<-df_24[, 2:11]
df_12<-df_12[, 2:11]

write.csv(df_24, "../bigdata_moment/bigdata_lab6/my/data/raw/countries_24.csv", row.names=FALSE)
write.csv(df_12, "../bigdata_moment/bigdata_lab6/my/data/raw/countries_12.csv", row.names=FALSE)