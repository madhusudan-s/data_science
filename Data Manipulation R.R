setwd("C:\\DS Full stack\\Non Graded Assignments\\R for Data Science\\Data Visulaization\\")
fd<-read.csv("C:\\DS Full stack\\Non Graded Assignments\\R for Data Science\\Data Visulaization\\FlightDelays.csv")

#Could not read file with above statements, hence using workaround
f<-file.choose()
fd<-read.csv(f)
library(dplyr)

fd$date<- as.Date(fd$date, "%m-%d-%y")
fd%>%filter(dayweek <=5, delay=="delayed")%>%nrow()

fd%>%filter(dayweek ==5, delay=="delayed")%>%summarize(mean(distance), sum(distance), length(distance))

fd%>%filter(dayweek <=5, delay=="ontime")%>%nrow()
fd%>%filter(dayweek >5, delay=="ontime")%>%nrow()

fd%>%filter(dayweek <=5)%>%group_by(dest)%>%summarize(length(flightnumber))

fd%>%filter(dayweek <=5, weather==1)%>%nrow()
