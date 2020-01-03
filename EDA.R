con<-read.csv("C:/DS Full stack/Non Graded Assignments/Data Wrangling and EDA with R/contribution.csv")

options(scipen = 999)
library(dplyr)

sum(con$totalContribution)

con$totalContribution<-con$FY04Giving+con$FY03Giving+con$FY02Giving+con$FY01Giving+con$FY00Giving
con%>%group_by(Gender)%>%summarize(n(), n()*100/nrow(con), sum(totalContribution), sum(totalContribution)/sum(con$totalContribution), mean(totalContribution))

con%>%group_by(Class.Year)%>%summarize(n(), n()/nrow(con), sum(totalContribution),sum(totalContribution)/sum(con$totalContribution), mean(totalContribution))

con%>%group_by(Marital.Status)%>%summarize(n(), n()/nrow(con), sum(totalContribution),sum(totalContribution)/sum(con$totalContribution), mean(totalContribution))

con%>%group_by(Major)%>%summarize(n(), n()/nrow(con), sum(totalContribution),sum(totalContribution)/sum(con$totalContribution), mean(totalContribution))

con%>%group_by(Next.Degree)%>%summarize(n(), n()/nrow(con), sum(totalContribution),sum(totalContribution)/sum(con$totalContribution), mean(totalContribution))
unique(con$Next.Degree)

con%>%group_by(AttendenceEvent)%>%summarize(n(), n()/nrow(con), sum(totalContribution),sum(totalContribution)/sum(con$totalContribution), mean(totalContribution))
