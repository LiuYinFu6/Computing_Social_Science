library(rvest)
library(magrittr)
url<-'https://www.americanrhetoric.com/gwbushspeeches.htm'
web<-read_html(url)


###From here the program work!

titles<-web%>%html_nodes('table')%>%.[[2]]%>%html_table()
titles

link<-web%>%html_nodes('a')%>%html_attr("href")
link

clean<-data.frame(titles)
clean<-clean[-c(1:30),]
clean<-clean[,-c(4:34)]
#clean<-clean[-c(1:5),]
library(tidyverse)
library(dplyr)

clean1<-clean%>%slice(1:5)
clean1<-clean1[,-1]
names(clean1)[1] <-"time"
names(clean1)[2] <-"title"
clean2<-clean%>%slice(6:34)
clean2<-clean2[,-3]
names(clean2)[1] <-"time"
names(clean2)[2] <-"title"
clean3<-clean%>%slice(35:108)
clean3<-clean3[,-1]
names(clean3)[1] <-"time"
names(clean3)[2] <-"title"
clean11<-rbind(clean1,clean2)
clean12<-rbind(clean11,clean3)

link<-web%>%html_nodes('a')%>%html_attr("href")
link
link<-data.frame(link)
names(link)[1] <-"link"
link1<-link[-grep("pdf", link$link),]
link1<-data.frame(link1)

link2<-link1[!grepl("mp3", link1$link),]
link2<-data.frame(link2)
link2<-link2[-c(1:22),]
link2<-data.frame(link2)
link2<-link2[-c(109:117),]
clean123<-cbind(clean12,link2)
clean123<-data.frame(clean123)
clean4<-clean123[-grep("http", clean123$link),]
clean4<-data.frame(clean4)
link3<-clean4$link
link3<-data.frame(link3)

clean4$link<-paste('https://www.americanrhetoric.com/',clean4$link2,sep="")
link4<-clean4$link
link4[90]

#link4<-data.frame(link4)
content <- c(1:99)


for(i in 1:99){
  content[i]<-read_html(link4[i])%>%html_node('td')%>%html_text()
}
content<-data.frame(content)
#content[1]

result<- c(1:99)

for(i in 1:99){
  result[i] = substring(content[i], 700)
}


result<-data.frame(result)
BushData<-cbind(clean4,result)
BushData<-BushData[,-3]
BushData<-BushData[,-3]
names(BushData)[1] <-"date"
names(BushData)[2] <-"title"
names(BushData)[3]<-"content"
BushData$Party<-'R'
