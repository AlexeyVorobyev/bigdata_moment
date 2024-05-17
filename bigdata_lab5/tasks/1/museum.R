library(rvest)
library(stringr)

url<-read_html('https://kudago.com/spb/list/33-luchshih-muzeya-peterburga/')

selector_name<-'a.post-list-item-title-link'
fnames<-sapply(
	html_nodes(url, selector_name)%>%html_text()%>%as.vector(),
	function (item) trimws(str_replace_all(item, "\n", ""))
)

selector_name<-'address.post-list-item-info'
fnames2<-sapply(
	html_nodes(url, selector_name)%>%html_text()%>%as.vector(),
	function (item) trimws(str_replace_all(item, "\n", ""))
)

selector_name<-'.post-list-item-title-link'
fnames_addr<-sapply(
	html_nodes(url, selector_name)%>%html_attr('href'),
	function (item) trimws(str_replace_all(item, "\n", ""))
)
museums<-data.frame(fnames[1:37], fnames2, fnames_addr[1:37])

colnames(museums)<-c('Name', 'Address', 'Link to image')

museums