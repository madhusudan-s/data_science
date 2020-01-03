setwd("C:/Jig19668")
library(dplyr)
library(lubridate)

campaign<-read.table("C:/DS Full stack/Graded Assignments/Data Wrangling and EDA with R/Campaign_File.txt",sep = "\t",header = TRUE)
customer<-read.table("C:/DS Full stack/Graded Assignments/Data Wrangling and EDA with R/Customers_File.txt",sep = "\t", header = TRUE)
product<-read.table("C:/DS Full stack/Graded Assignments/Data Wrangling and EDA with R/Products_File.txt",sep = "\t",header = TRUE)
transaction<-read.table("C:/DS Full stack/Graded Assignments/Data Wrangling and EDA with R/Transactions_File.txt",sep = "\t",header = TRUE)
transaction$Timestamp<-ymd_hms(transaction$Timestamp)
tr_cust<-merge(x=transaction,y=customer)
tr_cust$Timestamp<-ymd_hms(tr_cust$Timestamp)
tr_cust$Birth_Date<-ymd(tr_cust$Birth_Date)
tr_cust$Age<-round(as.numeric(difftime("2017-01-01",tr_cust$Birth_Date))/365)
group_age<-paste(seq(25,100,by=15),seq(25+14,115,by=15),sep = "-")
group_age
tr_cust$Age_Group<-cut(tr_cust$Age,breaks=c(seq(25,114,by=15),Inf),labels = group_age,right = FALSE)
tr_cust%>%group_by(Age_Group,Gender)%>%summarize(sum(Items_Amount))

cc<-merge(x=customer,y=campaign)
str(cc)
cc$Campaign_Responce
round(nrow(cc[cc$Campaign_Responce ==TRUE,])/nrow(cc),3)

cc$Registration_Date<-ymd(cc$Registration_Date)
cc$tenure<-round(as.numeric(difftime("2004-12-31",cc$Registration_Date))/365,1)
group_tenure<-paste(seq(3,6,by=1.5),seq(3+1.5-0.1,7.5,by=1.5),sep = "-")
group_tenure
cc$Tenure_Group<-cut(cc$tenure,breaks=c(seq(3,7.4,by=1.5),Inf),labels = group_tenure,right = FALSE)
cc%>%group_by(Tenure_Group)%>%summarize(n())
