library(dplyr)
library(car)
library(ggplot2)
setwd("C:/Jig19668")
prices<-read.csv("C:/DS Full stack/Non Graded Assignments/Predective Analytics with R/Clustering/boston_prices.csv",header = TRUE)

summary(prices)
str(prices)
boxplot(prices$CRIM)
which(is.na(prices$MEDV))
prices$MEDV[is.na(prices$MEDV)]<-mean(prices$MEDV,na.rm = TRUE)

summary(prices$MEDV)

par(mfrow=c(2,7))
list<-names(prices)
length(list)
list<-list[-4]

class(list)

for (i in list) {
  boxplot(prices[,i],main=i)
}
dev.off()
x<-boxplot(prices$CRIM)
which(x$out)
head(prices$CRIM)
-which(prices$CRIM %in% x$out)

for (i in 1:length(list)) {
  x<-boxplot(prices[,list[i]])
  index<-which(prices[,list[i]] %in% x$out)
  prices[index,list[i]]<-mean(prices[,list[i]])
  rm(x)
}

hist(prices$MEDV)
hist(log(prices$MEDV))
list1<-list[-13]
prices$CRIM

for (i in 1:length(list1)) {
  x<-cor(prices$MEDV,prices[list1[i]])
  print(x)
}

prices$log_CRIM<-log(prices$CRIM)
prices$log_ZN<-log(prices$ZN)
prices$log_INDUS<-log(prices$INDUS)
prices$log_nitric.oxides.concentration<-log(prices$nitric.oxides.concentration)
prices$log_X.rooms.dwelling<-log(prices$X.rooms.dwelling)
prices$log_AGE<-log(prices$AGE)
prices$log_DIS<-log(prices$DIS)
prices$log_RAD<-log(prices$RAD)
prices$log_TAX<-log(prices$TAX)
prices$log_PTRATIO<-log(prices$PTRATIO)
prices$log_B<-log(prices$B)
prices$log_LSTAT<-log(prices$LSTAT)
prices$log_MEDV<-log(prices$MEDV)

list_log<-names(prices)[c(15:25,27)]

for (i in 1:length(list_log)) {
  xlog<-cor(prices$log_MEDV,prices[list_log[i]])
  print(xlog)
}

list_log_DV<-names(prices)[1:13]
list_log_DV<-list_log_DV[-4]

for (i in 1:length(list_log_DV)) {
  xlogdv<-cor(prices$log_MEDV,prices[list_log_DV[i]])
  print(xlogdv)
}

sampling<-sort(sample(nrow(prices),nrow(prices)*.7))
train<-prices[sampling,]
test<-prices[-sampling,]

reg<-lm(log_MEDV~CRIM+INDUS+RAD+TAX+B+
          Charles.River.dummy.variable+
          DIS+ZN+PTRATIO+LSTAT+AGE+X.rooms.dwelling+nitric.oxides.concentration,
        data=train)

summary(reg)
step(reg)
reg1<-lm(log_MEDV~
          Charles.River.dummy.variable+
          DIS+PTRATIO+LSTAT+AGE+X.rooms.dwelling+nitric.oxides.concentration,
        data=train)
summary(reg1)

reg2<-lm(log_MEDV~CRIM+INDUS+RAD+TAX+B+
           Charles.River.dummy.variable+
           DIS+ZN+PTRATIO+LSTAT+X.rooms.dwelling+nitric.oxides.concentration,
         data=train)
summary(reg2)

reg3<-lm(log_MEDV~CRIM+RAD+
           Charles.River.dummy.variable+
           DIS+ZN+PTRATIO+LSTAT+nitric.oxides.concentration,
         data=train)
summary(reg3)

reg4<-lm(log_MEDV~INDUS+CRIM+
           Charles.River.dummy.variable+
           ZN+LSTAT+X.rooms.dwelling,
         data=train)
summary(reg4)

predicted<-predict(reg3)
plot(predicted)

residuals<-resid(reg3)
plot(residuals)

plot(predicted,residuals, abline(0,0))

predicted<-predict(reg3,newdata = test)
test$p<-predicted

test$error<-(test$log_MEDV-test$p)/test$log_MEDV
mean(test$error)*100

plot(test$p,col="blue",type="b")
lines(test$log_MEDV,col="red",type="b")

dataBrand<-read.csv("C:/DS Full stack/Graded Assignments/Predicitve Analytics with R/goodforu-class12.csv")
summary(dataBrand$X23)
str(dataBrand)

dataBrand$X23<-as.factor(dataBrand$X23)
dataBrand$X2<-as.factor(dataBrand$X2)
dataBrand$X9<-as.factor(dataBrand$X9)
dataBrand$X16<-as.factor(dataBrand$X16)
dataBrand$X30<-as.numeric(dataBrand$X30)
dataBrand$X22<-as.factor(dataBrand$X22)
dataBrand$X9ind1<-as.factor(dataBrand$X9ind1)
dataBrand$X9ind2<-as.factor(dataBrand$X9ind2)
12160
24114

summary(dataBrand$X9)
summary(dataBrand$X22)
summary(dataBrand$X16)
summary(dataBrand$X30)
summary(dataBrand$X23)
summary(dataBrand$X2ind1)
summary(dataBrand$X2ind2)

mod=glm(X23~X2ind1+X2ind2+X9ind1+X9ind2+X16ind1+X16ind2+X30,family=binomial,data=dataBrand)
dataBrand$X16ind1<-ifelse(dataBrand$X16 == 1,1,0)
dataBrand$X16ind2<-ifelse(dataBrand$X16 == 2,1,0)
dataBrand$X9ind1<-ifelse(dataBrand$X9 == 1,1,0)
dataBrand$X9ind2<-ifelse(dataBrand$X9 == 2,1,0)
summary(mod)

mod=glm(X23~X2ind2+X9ind2+X16ind2+X30,family=binomial,data=dataBrand)

cor(prices$log_MEDV,prices[list_log_DV[3]])
