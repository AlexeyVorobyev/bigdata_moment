library(rvest)

url_21<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2021')
url_20<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2020')
url_19<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2019')
url_18<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2018')
url_17<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2017')
url_16<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2016')
url_15<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2015')
url_14<-read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2014')

nodes_21<-html_nodes(url_21, 'table')
nodes_20<-html_nodes(url_20, 'table')
nodes_19<-html_nodes(url_19, 'table')
nodes_18<-html_nodes(url_18, 'table')
nodes_17<-html_nodes(url_17, 'table')
nodes_16<-html_nodes(url_16, 'table')
nodes_15<-html_nodes(url_15, 'table')
nodes_14<-html_nodes(url_14, 'table')

df_21<-html_table(nodes_21[[2]])%>%as.data.frame()
df_20<-html_table(nodes_20[[2]])%>%as.data.frame()
df_19<-html_table(nodes_19[[2]])%>%as.data.frame()
df_18<-html_table(nodes_18[[2]])%>%as.data.frame()
df_17<-html_table(nodes_17[[2]])%>%as.data.frame()
df_16<-html_table(nodes_16[[2]])%>%as.data.frame()
df_15<-html_table(nodes_15[[2]])%>%as.data.frame()
df_14<-html_table(nodes_14[[2]])%>%as.data.frame()

rownames(df_21)<-df_21[, 2]
rownames(df_20)<-df_20[, 2]
rownames(df_19)<-df_19[, 2]
rownames(df_18)<-df_18[, 2]
rownames(df_17)<-df_17[, 2]
rownames(df_16)<-df_16[, 2]
rownames(df_15)<-df_15[, 2]
rownames(df_14)<-df_14[, 2]

df_21<-df_21[, 3:11]
df_20<-df_20[, 3:11]
df_19<-df_19[, 3:11]
df_18<-df_18[, 3:11]
df_17<-df_17[, 3:11]
df_16<-df_16[, 3:11]
df_15<-df_15[, 3:11]
df_14<-df_14[, 3:11]

country<-c('Georgia', 'Mexico', 'Poland', 'Italy', 'Cyprus')

evaluation_of<-'Quality of Life Index'
QLI<-as.data.frame(
	rbind(
		df_14[country, evaluation_of],
		df_15[country, evaluation_of],
		df_16[country, evaluation_of],
		df_17[country, evaluation_of],
		df_18[country, evaluation_of],
		df_19[country, evaluation_of],
		df_20[country, evaluation_of],
		df_21[country, evaluation_of]
	),
	row.names<-2014:2021
)
colnames(QLI)<-country

mn<-min(QLI, na.rm=TRUE)
mx<-max(QLI, na.rm=TRUE)

plot(2014:2021, QLI$'Georgia', xlab='Years', ylab='Quality of life Index', ylim=c(mn-13,mx+13),
	 main='Rating for Quality of Life Index',col='blue',type='b',lty=1,pch=1, lwd=2)

lines(2014:2021, QLI$'Mexico', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, QLI$'Poland', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, QLI$'Italy', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, QLI$'Cyprus', type='b', col='gold', lty=1, pch=1, lwd=2)
legend('bottomright', cex=0.7,country, fill= c('blue', 'green', 'red', 'purple', 'gold'))

evaluation_of<-'Purchasing Power Index'
PPI<-as.data.frame(
	rbind(
		df_14[country, evaluation_of],
		df_15[country, evaluation_of],
		df_16[country, evaluation_of],
		df_17[country, evaluation_of],
		df_18[country, evaluation_of],
		df_19[country, evaluation_of],
		df_20[country, evaluation_of],
		df_21[country, evaluation_of]
	),
	row.names<-2014:2021
)
colnames(PPI)<-country

mn<-min(PPI, na.rm=TRUE)
mx<-max(PPI, na.rm=TRUE)

plot( 2014:2021, PPI$'Georgia', xlab='Years', ylab='Index of purchase power', ylim=c(mn-13,mx+13),
	  main='Rating for Index of purchase power', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, PPI$'Mexico', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, PPI$'Poland', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, PPI$'Italy', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, PPI$'Cyprus', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.7, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

evaluation_of<-'Safety Index'
SI<-as.data.frame(
	rbind(
		df_14[country, evaluation_of],
		df_15[country, evaluation_of],
		df_16[country, evaluation_of],
		df_17[country, evaluation_of],
		df_18[country, evaluation_of],
		df_19[country, evaluation_of],
		df_20[country, evaluation_of],
		df_21[country, evaluation_of]
	),
	row.names<-2014:2021
)
colnames(SI)<-country

mn<-min(SI, na.rm=TRUE)
mx<-max(SI, na.rm=TRUE)

plot( 2014:2021, SI$'Georgia', xlab='Years', ylab='Safety index', ylim=c(mn-13,mx+13),
	  main='Rating for safety index', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, SI$'Mexico', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, SI$'Poland', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, SI$'Italy', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, SI$'Cyprus', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

evaluation_of<-'Health Care Index'
HCI<-as.data.frame(
	rbind(
		df_14[country, evaluation_of],
		df_15[country, evaluation_of],
		df_16[country, evaluation_of],
		df_17[country, evaluation_of],
		df_18[country, evaluation_of],
		df_19[country, evaluation_of],
		df_20[country, evaluation_of],
		df_21[country, evaluation_of]
	),
	row.names<-2014:2021
)
colnames(HCI)<-country

mn<-min(HCI, na.rm=TRUE)
mx<-max(HCI, na.rm=TRUE)

plot(2014:2021, HCI$'Georgia', xlab='Years', ylab='Index of Medical infrastructure', ylim=c(mn-13,mx+13),
	 main='Rating for Index of Medical infrastructure ', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, HCI$'Mexico', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, HCI$'Poland', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, HCI$'Italy', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, HCI$'Cyprus', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('bottomright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

evaluation_of<-'Cost of Living Index'
CLI<-as.data.frame(
	rbind(
		df_14[country, evaluation_of],
		df_15[country, evaluation_of],
		df_16[country, evaluation_of],
		df_17[country, evaluation_of],
		df_18[country, evaluation_of],
		df_19[country, evaluation_of],
		df_20[country, evaluation_of],
		df_21[country, evaluation_of]
	),
	row.names<-2014:2021
)
colnames(CLI)<-country

mn<-min(CLI, na.rm=TRUE)
mx<-max(CLI, na.rm=TRUE)

plot(2014:2021, CLI$'Georgia', xlab='Years', ylab='Index of living level treshold', ylim=c(mn-13,mx+13),
	 main='Rating of Index of living level treshold', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, CLI$'Mexico', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, CLI$'Poland', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, CLI$'Italy', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, CLI$'Cyprus', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.7, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

evaluation_of<-'Property Price to Income Ratio'
PPIR<-as.data.frame(
	rbind(
		df_14[country, evaluation_of],
		df_15[country, evaluation_of],
		df_16[country, evaluation_of],
		df_17[country, evaluation_of],
		df_18[country, evaluation_of],
		df_19[country, evaluation_of],
		df_20[country, evaluation_of],
		df_21[country, evaluation_of]
	),
	row.names<-2014:2021
)
colnames(PPIR)<-country

mn<-min(PPIR, na.rm=TRUE)
mx<-max(PPIR, na.rm=TRUE)
plot( 2014:2021, PPIR$'Georgia', xlab='Years', ylab='Property Price to Income Ratio', ylim=c(mn-13,mx+13),
	  main='Rating for Property Price to Income Ratio', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, PPIR$'Mexico', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, PPIR$'Poland', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, PPIR$'Italy', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, PPIR$'Cyprus', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.7, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

evaluation_of<-'Traffic Commute Time Index'
TCTI<-as.data.frame(
	rbind(
		df_14[country, evaluation_of],
		df_15[country, evaluation_of],
		df_16[country, evaluation_of],
		df_17[country, evaluation_of],
		df_18[country, evaluation_of],
		df_19[country, evaluation_of],
		df_20[country, evaluation_of],
		df_21[country, evaluation_of]
	),
	row.names<-2014:2021
)
colnames(TCTI)<-country

mn<-min(TCTI, na.rm=TRUE)
mx<-max(TCTI, na.rm=TRUE)

plot( 2014:2021, TCTI$'Georgia', xlab='Years', ylab='Traffic Commute Time Index', ylim=c(mn-13,mx+13),
	  main='Rating for Traffic Commute Time Index', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, TCTI$'Mexico', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, TCTI$'Poland', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, TCTI$'Italy', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, TCTI$'Cyprus', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.7, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

evaluation_of<-'Pollution Index'
PI<-as.data.frame(
	rbind(
		df_14[country, evaluation_of],
		df_15[country, evaluation_of],
		df_16[country, evaluation_of],
		df_17[country, evaluation_of],
		df_18[country, evaluation_of],
		df_19[country, evaluation_of],
		df_20[country, evaluation_of],
		df_21[country, evaluation_of]
	),
	row.names<-2014:2021
)
colnames(PI)<-country

mn<-min(PI, na.rm=TRUE)
mx<-max(PI, na.rm=TRUE)

plot( 2014:2021, PI$'Georgia', xlab='Years', ylab='Pollution Index', ylim=c(mn-13,mx+13),
	  main='Rating for Pollution Index', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2014:2021, PI$'Mexico', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2014:2021, PI$'Poland', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2014:2021, PI$'Italy', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2014:2021, PI$'Cyprus', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('topright', cex=0.6, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))


evaluation_of<-'Climate Index'
CI<-as.data.frame(
	rbind(
		df_16[country, evaluation_of],
		df_17[country, evaluation_of],
		df_18[country, evaluation_of],
		df_19[country, evaluation_of],
		df_20[country, evaluation_of],
		df_21[country, evaluation_of]
	),
	row.names<-2016:2021
)
colnames(CI)<-country

mn<-min(CI, na.rm=TRUE)
mx<-max(CI, na.rm=TRUE)

plot( 2016:2021, CI$'Georgia', xlab='Years', ylab='Climate Index', ylim=c(mn-13,mx+13),
	  main='Rating for Climate Index', col='blue', type='b', lty=1, pch=1,  lwd=2)

lines(2016:2021, CI$'Mexico', type='b', col='green', lty=1, pch=1, lwd=2)
lines(2016:2021, CI$'Poland', type='b', col='red', lty=1, pch=1, lwd=2)
lines(2016:2021, CI$'Italy', type='b', col='purple', lty=1, pch=1, lwd=2)
lines(2016:2021, CI$'Cyprus', type='b', col='gold', lty=1, pch=1, lwd=2)

legend('bottomright', cex=0.7, country,fill=c('blue', 'green', 'red', 'purple', 'gold'))

