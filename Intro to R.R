Name<-c("Ramya","Ali","Jim")
Age<-c(25,30,35)
Telephone_bill_rs<-c(600,400,200)
Month<-c("Aug","Aug","Aug")
customer_details<-data.frame(Name,Age,Telephone_bill_rs,Month)

last_name<-c("Potter", "Riddle", "Dumbledore")
first_name<-c("Harry", "Tom", "Albus")
age<-c(18,50,120)
profession<-c("Student", "Magician", "Headmaster")
names<-list(last_name,first_name,age,profession)

names[1]
names[[3]][3]


library(ggplot2)
library(msleep)
?msleep
names(msleep)
names(msleep)[3]<-"type"
names(msleep)
msleep[1:10,3]
mymsleep<-msleep[,c(1,2,3,6)]


write.csv(mymsleep,"C:\\Jig19668\\mymsleep.csv")

library(readxl)
data1<-read_excel("C:\\DS Full stack\\Non Graded Assignments\\R for Data Science\\Intro to R\\retail.xlsx",sheet="data1")
data2<-read_excel("C:\\DS Full stack\\Non Graded Assignments\\R for Data Science\\Intro to R\\retail.xlsx",sheet="data2")

