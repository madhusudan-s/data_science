library(XML)
setwd("C:\\DS Full stack\\Graded Assignments\\R for Data Sciecne\\Data Visualisation with R")
u=c("The World's Most Valuable Brands List - Forbes.html")
tables=readHTMLTable(u)
tables$the_list
data<-tables$the_list
data

class(tables)
length(tables)
str(data)
unique(data$`Company Advertising`)

nrow(data[data$`Company Advertising`=="*B",])
summary(data)
nrow(data[data$`Company Advertising`== "-",])
data$`Brand Revenue`
nrow(data[grep("M$",data$`Company Advertising`),])

data$`Company Advertising`<-if (data$`Company Advertising` == "-") ? NA: data$`Company Advertising`

library(dplyr)
data1<-data


data1[data1=="-"] <-NA
summary(data1)

data2<-na.omit(data1)
data2$Rank<-gsub("#","",data2$Rank)
data2$`Brand Value`<-gsub('B$',"",data2$`Brand Value`)

data2$`Brand Value`<-substring(data2$`Brand Value`,2)

data2$`Brand Value`<-as.numeric(data2$`Brand Value`)


data2$Rank<-gsub("#","",data2$Rank)
data2$`Brand Revenue`<-gsub('B$',"",data2$`Brand Revenue`)

data2$`Brand Revenue`<-substring(data2$`Brand Revenue`,2)

data2$`Brand Revenue`<-as.numeric(data2$`Brand Revenue`)

summary(data2)
