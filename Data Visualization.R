dataF<-read.csv("C:/DS Full stack/Non Graded Assignments/R for Data Science/Data Visulaization/dataF.csv")

library(ggplot2)
p<-ggplot(dataF[dataF$Industry=="Technology",],aes(x=Company.Advertising, y=Brand.Revenue))
p+geom_point(aes(cex=Brand.Value, color=Brand))+
  geom_text(aes(label=Brand,vjust=1))+ xlab("Company Advertising in Billions of $") + ylab("Brand Revenue in Billions of $")+
  guides(color=FALSE) + scale_radius(name="Brand.Value $(Billions)",breaks = c(30,60,100))


p<-ggplot(dataF[dataF$Industry=="Luxury",],aes(x=Company.Advertising, y=Brand.Revenue))
p+geom_point(aes(cex=Brand.Value, color=Brand))+
  scale_x_continuous( breaks=seq(0.0,4.8,0.1))+
  geom_text(aes(label=Brand,vjust=1.5, hjust=0.6))+ 
  xlab("Company Advertising in Billions of $") + ylab("Brand Revenue in Billions of $")+
  guides(color=FALSE) + scale_radius(name="Brand.Value $(Billions)",breaks = c(10.0,28.1))
  
dataF[dataF$Industry=="Financial Services",]
unique(dataF$Industry)
seq(10,90,10)


p<-ggplot(dataF[dataF$Industry=="Financial Services",],aes(x=Company.Advertising, y=Brand.Revenue))
p+geom_point(aes(cex=Brand.Value, color=Brand))+
  scale_x_continuous( breaks=seq(0.6,3.4,0.1))+
  scale_y_continuous( breaks=seq(10,90,10))+
  geom_text(aes(label=Brand,vjust=1.5, hjust=0.8))+ 
  xlab("Company Advertising in Billions of $") + ylab("Brand Revenue in Billions of $")+
  guides(color=FALSE)+ scale_radius(name="Brand.Value $(Billions)",breaks = c(7.2,12.0,23.4))


p<-ggplot(dataF[dataF$Industry=="Automotive",],aes(x=Company.Advertising, y=Brand.Revenue))
p+geom_point(aes(cex=Brand.Value, color=Brand))+
  scale_x_continuous( breaks=seq(0.8,5.4,0.1))+
  scale_y_continuous( breaks=seq(40,170,10))+
  geom_text(aes(label=Brand,vjust=1.5, hjust=0.7))+ 
  xlab("Company Advertising in Billions of $") + ylab("Brand Revenue in Billions of $")+
  guides(color=FALSE)+ scale_radius(name="Brand.Value $(Billions)",breaks = c(6.2,20.0,37.8))

