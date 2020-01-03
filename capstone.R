setwd("C:/Jig19668")

telecom<- read.csv("C:/DS Full stack/Graded Assignments/09 - Capstone Project  and Certitication/telecomfinal.csv", stringsAsFactors = FALSE)
# Make sure we have only numeric and char values 
#so that quality report can be generated
str(telecom) 

#Generate data quality report
library(dataQualityR)
checkDataQuality(telecom,out.file.num = "numeric.csv", out.file.cat = "character.csv")

#Ignoring income,numbcars,retdays, dwlltype, dwllsize, mailordr, occu1
#wrkwoman, solflag, proptype, mailresp, cartype, children, div_type
# as they have high missing percentage

cols<-c(12,52,53,46,47,48,49,55,61,62,63,64,66,72)

tel<-telecom[,-cols]

library(dplyr)
library(rpart)
library(gains)
library(ROCR)

#Consider totrev as it is significant
tel%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->totrev
totrev$N<-unclass(tel%>%mutate(dec=ntile(totrev,n=10))%>%count(dec))[[2]]
totrev$churn_perc<-totrev$n/totrev$N
totrev$GreaterThan<-unclass(tel%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
totrev$LessThan<-unclass(tel%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
totrev$varname<-rep("totrev",nrow(totrev))

#Consider mou_mean as its values decreases churn percentage increases
tel%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->mou_Mean
mou_Mean$N<-unclass(tel%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec))[[2]]
mou_Mean$churn_perc<-mou_Mean$n/mou_Mean$N
mou_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
mou_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
mou_Mean$varname<-rep("mou_Mean",nrow(mou_Mean))

#Missing  value imputation
# It has high churnpercentage which matches the first decile
tel[which(is.na(tel$mou_Mean)),"mou_Mean"]<-0

#Do not consider totmrc_Mean as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->totmrc_Mean
totmrc_Mean$N<-unclass(tel%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec))[[2]]
totmrc_Mean$churn_perc<-totmrc_Mean$n/totmrc_Mean$N
totmrc_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
totmrc_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
totmrc_Mean$varname<-rep("totmrc_Mean",nrow(totmrc_Mean))

#Do  not consider rev_Range as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->rev_Range
rev_Range$N<-unclass(tel%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec))[[2]]
rev_Range$churn_perc<-rev_Range$n/rev_Range$N
rev_Range$GreaterThan<-unclass(tel%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
rev_Range$LessThan<-unclass(tel%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
rev_Range$varname<-rep("rev_Range",nrow(rev_Range))

#Do  not consider mou_Range as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->mou_Range
mou_Range$N<-unclass(tel%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec))[[2]]
mou_Range$churn_perc<-mou_Range$n/mou_Range$N
mou_Range$GreaterThan<-unclass(tel%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
mou_Range$LessThan<-unclass(tel%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
mou_Range$varname<-rep("mou_Range",nrow(mou_Range))

#Do  not consider change_mou as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->change_mou
change_mou$N<-unclass(tel%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec))[[2]]
change_mou$churn_perc<-change_mou$n/change_mou$N
change_mou$GreaterThan<-unclass(tel%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
change_mou$LessThan<-unclass(tel%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
change_mou$varname<-rep("change_mou",nrow(change_mou))

#Consider drop_blk_Mean as it is significant
tel%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->drop_blk_Mean
drop_blk_Mean$N<-unclass(tel%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec))[[2]]
drop_blk_Mean$churn_perc<-drop_blk_Mean$n/drop_blk_Mean$N
drop_blk_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
drop_blk_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
drop_blk_Mean$varname<-rep("drop_blk_Mean",nrow(drop_blk_Mean))

#Do not consider drop_vce_Range  as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->drop_vce_Range
drop_vce_Range$N<-unclass(tel%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec))[[2]]
drop_vce_Range$churn_perc<-drop_vce_Range$n/drop_vce_Range$N
drop_vce_Range$GreaterThan<-unclass(tel%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
drop_vce_Range$LessThan<-unclass(tel%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
drop_vce_Range$varname<-rep("drop_vce_Range",nrow(drop_vce_Range))

#Do not consider owylis_vce_Range  as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->owylis_vce_Range
owylis_vce_Range$N<-unclass(tel%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec))[[2]]
owylis_vce_Range$churn_perc<-owylis_vce_Range$n/owylis_vce_Range$N
owylis_vce_Range$GreaterThan<-unclass(tel%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
owylis_vce_Range$LessThan<-unclass(tel%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
owylis_vce_Range$varname<-rep("owylis_vce_Range",nrow(owylis_vce_Range))

#Do not consider mou_opkv_Range  as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->mou_opkv_Range
mou_opkv_Range$N<-unclass(tel%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec))[[2]]
mou_opkv_Range$churn_perc<-mou_opkv_Range$n/mou_opkv_Range$N
mou_opkv_Range$GreaterThan<-unclass(tel%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
mou_opkv_Range$LessThan<-unclass(tel%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
mou_opkv_Range$varname<-rep("mou_opkv_Range",nrow(mou_opkv_Range))

#Consider months  as it might be significant
tel%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->months
months$N<-unclass(tel%>%mutate(dec=ntile(months,n=10))%>%count(dec))[[2]]
months$churn_perc<-months$n/months$N
months$GreaterThan<-unclass(tel%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
months$LessThan<-unclass(tel%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
months$varname<-rep("months",nrow(months))

#Consider totcalls  as it might be significant
tel%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->totcalls
totcalls$N<-unclass(tel%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec))[[2]]
totcalls$churn_perc<-totcalls$n/totcalls$N
totcalls$GreaterThan<-unclass(tel%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
totcalls$LessThan<-unclass(tel%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
totcalls$varname<-rep("totcalls",nrow(totcalls))

#Consider eqpdays  as it might be significant
tel%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==0)->eqpdays
eqpdays$N<-unclass(tel%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%filter(dec%in%eqpdays$dec))[[2]]
eqpdays$churn_perc<-eqpdays$n/eqpdays$N
eqpdays$GreaterThan<-unclass(tel%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(min(eqpdays))%>%filter(dec%in%eqpdays$dec))[[2]]
eqpdays$LessThan<-unclass(tel%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(max(eqpdays))%>%filter(dec%in%eqpdays$dec))[[2]]
eqpdays$varname<-rep("eqpdays",nrow(eqpdays))

index<-which(is.na(tel$eqpdays))
tel<-tel[-index,]

#Do not consider custcare_Mean as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->custcare_Mean
custcare_Mean$N<-unclass(tel%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(dec)%>%filter(dec%in%custcare_Mean$dec))[[2]]
custcare_Mean$churn_perc<-custcare_Mean$n/custcare_Mean$N
custcare_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(custcare_Mean,n=10))%>%group_by(dec)%>%summarise(min(custcare_Mean))%>%filter(dec%in%custcare_Mean$dec))[[2]]
custcare_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(custcare_Mean,n=10))%>%group_by(dec)%>%summarise(max(custcare_Mean))%>%filter(dec%in%custcare_Mean$dec))[[2]]
custcare_Mean$varname<-rep("custcare_Mean",nrow(custcare_Mean))

#Do not consider callwait_Mean as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(callwait_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->callwait_Mean
callwait_Mean$N<-unclass(tel%>%mutate(dec=ntile(callwait_Mean,n=10))%>%count(dec)%>%filter(dec%in%callwait_Mean$dec))[[2]]
callwait_Mean$churn_perc<-callwait_Mean$n/callwait_Mean$N
callwait_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(callwait_Mean,n=10))%>%group_by(dec)%>%summarise(min(callwait_Mean))%>%filter(dec%in%callwait_Mean$dec))[[2]]
callwait_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(callwait_Mean,n=10))%>%group_by(dec)%>%summarise(max(callwait_Mean))%>%filter(dec%in%callwait_Mean$dec))[[2]]
callwait_Mean$varname<-rep("callwait_Mean",nrow(callwait_Mean))

#Do not consider iwylis_vce_Mean as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->iwylis_vce_Mean
iwylis_vce_Mean$N<-unclass(tel%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(dec)%>%filter(dec%in%iwylis_vce_Mean$dec))[[2]]
iwylis_vce_Mean$churn_perc<-iwylis_vce_Mean$n/iwylis_vce_Mean$N
iwylis_vce_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean))%>%filter(dec%in%iwylis_vce_Mean$dec))[[2]]
iwylis_vce_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean))%>%filter(dec%in%iwylis_vce_Mean$dec))[[2]]
iwylis_vce_Mean$varname<-rep("iwylis_vce_Mean",nrow(iwylis_vce_Mean))

#Do not consider callwait_Range as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(callwait_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->callwait_Range
callwait_Range$N<-unclass(tel%>%mutate(dec=ntile(callwait_Range,n=10))%>%count(dec)%>%filter(dec%in%callwait_Range$dec))[[2]]
callwait_Range$churn_perc<-callwait_Range$n/callwait_Range$N
callwait_Range$GreaterThan<-unclass(tel%>%mutate(dec=ntile(callwait_Range,n=10))%>%group_by(dec)%>%summarise(min(callwait_Range))%>%filter(dec%in%callwait_Range$dec))[[2]]
callwait_Range$LessThan<-unclass(tel%>%mutate(dec=ntile(callwait_Range,n=10))%>%group_by(dec)%>%summarise(max(callwait_Range))%>%filter(dec%in%callwait_Range$dec))[[2]]
callwait_Range$varname<-rep("callwait_Range",nrow(callwait_Range))

#Do not consider ccrndmou_Range as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->ccrndmou_Range
ccrndmou_Range$N<-unclass(tel%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%count(dec)%>%filter(dec%in%ccrndmou_Range$dec))[[2]]
ccrndmou_Range$churn_perc<-ccrndmou_Range$n/ccrndmou_Range$N
ccrndmou_Range$GreaterThan<-unclass(tel%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%group_by(dec)%>%summarise(min(ccrndmou_Range))%>%filter(dec%in%ccrndmou_Range$dec))[[2]]
ccrndmou_Range$LessThan<-unclass(tel%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%group_by(dec)%>%summarise(max(ccrndmou_Range))%>%filter(dec%in%ccrndmou_Range$dec))[[2]]
ccrndmou_Range$varname<-rep("ccrndmou_Range",nrow(ccrndmou_Range))

#Do not consider adjqty as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->adjqty
adjqty$N<-unclass(tel%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%filter(dec%in%adjqty$dec))[[2]]
adjqty$churn_perc<-adjqty$n/adjqty$N
adjqty$GreaterThan<-unclass(tel%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty))%>%filter(dec%in%adjqty$dec))[[2]]
adjqty$LessThan<-unclass(tel%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty))%>%filter(dec%in%adjqty$dec))[[2]]
adjqty$varname<-rep("adjqty",nrow(adjqty))

#Consider ovrrev_Mean as it might be significant
tel%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->ovrrev_Mean
ovrrev_Mean$N<-unclass(tel%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(dec)%>%filter(dec%in%ovrrev_Mean$dec))[[2]]
ovrrev_Mean$churn_perc<-ovrrev_Mean$n/ovrrev_Mean$N
ovrrev_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%group_by(dec)%>%summarise(min(ovrrev_Mean))%>%filter(dec%in%ovrrev_Mean$dec))[[2]]
ovrrev_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%group_by(dec)%>%summarise(max(ovrrev_Mean))%>%filter(dec%in%ovrrev_Mean$dec))[[2]]
ovrrev_Mean$varname<-rep("ovrrev_Mean",nrow(ovrrev_Mean))

#Missing  value imputation
# It has high churnpercentage which matches the last decile
tel[which(is.na(tel$ovrrev_Mean)),"ovrrev_Mean"]<-890

#Do not consider rev_Mean as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->rev_Mean
rev_Mean$N<-unclass(tel%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%filter(dec%in%rev_Mean$dec))[[2]]
rev_Mean$churn_perc<-rev_Mean$n/rev_Mean$N
rev_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean))%>%filter(dec%in%rev_Mean$dec))[[2]]
rev_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean))%>%filter(dec%in%rev_Mean$dec))[[2]]
rev_Mean$varname<-rep("rev_Mean",nrow(rev_Mean))

#Consider ovrmou_Mean as it is significant
tel%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->ovrmou_Mean
ovrmou_Mean$N<-unclass(tel%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(dec)%>%filter(dec%in%ovrmou_Mean$dec))[[2]]
ovrmou_Mean$churn_perc<-ovrmou_Mean$n/ovrmou_Mean$N
ovrmou_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%group_by(dec)%>%summarise(min(ovrmou_Mean))%>%filter(dec%in%ovrmou_Mean$dec))[[2]]
ovrmou_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%group_by(dec)%>%summarise(max(ovrmou_Mean))%>%filter(dec%in%ovrmou_Mean$dec))[[2]]
ovrmou_Mean$varname<-rep("ovrmou_Mean",nrow(ovrmou_Mean))

#Missing  value imputation
# It has high churnpercentage which matches the last decile
tel[which(is.na(tel$ovrmou_Mean)),"ovrmou_Mean"]<-200

#Consider comp_vce_Mean as there is a pattern in churn percentage
tel%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->comp_vce_Mean
comp_vce_Mean$N<-unclass(tel%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%filter(dec%in%comp_vce_Mean$dec))[[2]]
comp_vce_Mean$churn_perc<-comp_vce_Mean$n/comp_vce_Mean$N
comp_vce_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean))%>%filter(dec%in%comp_vce_Mean$dec))[[2]]
comp_vce_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean))%>%filter(dec%in%comp_vce_Mean$dec))[[2]]
comp_vce_Mean$varname<-rep("comp_vce_Mean",nrow(comp_vce_Mean))

#Consider plcd_vce_Mean as there is a pattern in churn percentage
tel%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->plcd_vce_Mean
plcd_vce_Mean$N<-unclass(tel%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%filter(dec%in%plcd_vce_Mean$dec))[[2]]
plcd_vce_Mean$churn_perc<-plcd_vce_Mean$n/plcd_vce_Mean$N
plcd_vce_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean))%>%filter(dec%in%plcd_vce_Mean$dec))[[2]]
plcd_vce_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean))%>%filter(dec%in%plcd_vce_Mean$dec))[[2]]
plcd_vce_Mean$varname<-rep("plcd_vce_Mean",nrow(plcd_vce_Mean))

#Do not consider avg3mou as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->avg3mou
avg3mou$N<-unclass(tel%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%filter(dec%in%avg3mou$dec))[[2]]
avg3mou$churn_perc<-avg3mou$n/avg3mou$N
avg3mou$GreaterThan<-unclass(tel%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou))%>%filter(dec%in%avg3mou$dec))[[2]]
avg3mou$LessThan<-unclass(tel%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou))%>%filter(dec%in%avg3mou$dec))[[2]]
avg3mou$varname<-rep("avg3mou",nrow(avg3mou))

#Do not consider avgmou as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->avgmou
avgmou$N<-unclass(tel%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%filter(dec%in%avgmou$dec))[[2]]
avgmou$churn_perc<-avgmou$n/avgmou$N
avgmou$GreaterThan<-unclass(tel%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou))%>%filter(dec%in%avgmou$dec))[[2]]
avgmou$LessThan<-unclass(tel%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou))%>%filter(dec%in%avgmou$dec))[[2]]
avgmou$varname<-rep("avgmou",nrow(avgmou))

#Do not consider avg3qty as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->avg3qty
avg3qty$N<-unclass(tel%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%filter(dec%in%avg3qty$dec))[[2]]
avg3qty$churn_perc<-avg3qty$n/avg3qty$N
avg3qty$GreaterThan<-unclass(tel%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty))%>%filter(dec%in%avg3qty$dec))[[2]]
avg3qty$LessThan<-unclass(tel%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty))%>%filter(dec%in%avg3qty$dec))[[2]]
avg3qty$varname<-rep("avg3qty",nrow(avg3qty))

#Do not consider avgqty as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->avgqty
avgqty$N<-unclass(tel%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%filter(dec%in%avgqty$dec))[[2]]
avgqty$churn_perc<-avgqty$n/avgqty$N
avgqty$GreaterThan<-unclass(tel%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty))%>%filter(dec%in%avgqty$dec))[[2]]
avgqty$LessThan<-unclass(tel%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty))%>%filter(dec%in%avgqty$dec))[[2]]
avgqty$varname<-rep("avgqty",nrow(avgqty))

#Consider avg6mou as there is a pattern in churn percentage
tel%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->avg6mou
avg6mou$N<-unclass(tel%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%filter(dec%in%avg6mou$dec))[[2]]
avg6mou$churn_perc<-avg6mou$n/avg6mou$N
avg6mou$GreaterThan<-unclass(tel%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou))%>%filter(dec%in%avg6mou$dec))[[2]]
avg6mou$LessThan<-unclass(tel%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou))%>%filter(dec%in%avg6mou$dec))[[2]]
avg6mou$varname<-rep("avg6mou",nrow(avg6mou))

#Missing  value imputation
# It has low churnpercentage which matches the last decile
tel[which(is.na(tel$avg6mou)),"avg6mou"]<-2000

#Do not consider avg6qty as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)->avg6qty
avg6qty$N<-unclass(tel%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%filter(dec%in%avg6qty$dec))[[2]]
avg6qty$churn_perc<-avg6qty$n/avg6qty$N
avg6qty$GreaterThan<-unclass(tel%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty))%>%filter(dec%in%avg6qty$dec))[[2]]
avg6qty$LessThan<-unclass(tel%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty))%>%filter(dec%in%avg6qty$dec))[[2]]
avg6qty$varname<-rep("avg6qty",nrow(avg6qty))

#Consider crclscod as this is a categorical variable
tel%>%count(churn,levels=crclscod)%>%filter(churn==1)->crclscod_dat
crclscod_dat$N<-unclass(tel%>%filter(crclscod%in%crclscod_dat$levels)%>%count(crclscod))[[2]]
crclscod_dat$ChurnPerc<-crclscod_dat$n/crclscod_dat$N
crclscod_dat$Var.Name<-rep("crclscod",nrow(crclscod_dat))

mod<-rpart(churn~crclscod,data = tel,method = "class")
unique(mod$where)

#With event_rate > 0.3 considered as high
high_level<-c("IF","EM","EF","P1","A2","A3","TP")
tel$crclscod_dummy<-ifelse(tel$crclscod%in%high_level,"High","Low")
tel$crclscod_dummy<-as.factor(tel$crclscod_dummy)
  
#Consider asl_flag as this is a categorical variable
tel$asl_flag<-as.factor(tel$asl_flag)

#Consider prizm_social_one as this is a categorical variable
tel%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->prizm_social_one_dat
prizm_social_one_dat$N<-unclass(tel%>%filter(prizm_social_one%in%prizm_social_one_dat$levels)%>%count(prizm_social_one))[[2]]
prizm_social_one_dat$ChurnPerc<-prizm_social_one_dat$n/prizm_social_one_dat$N
prizm_social_one_dat$Var.Name<-rep("prizm_social_one",nrow(prizm_social_one_dat))

mod<-rpart(churn~prizm_social_one,data = tel,method = "class")
unique(mod$where)

#With event_rate > 0.25 considered as low
high_level<-c("T","R")
tel$prizm_social_one_dummy<-ifelse(tel$prizm_social_one%in%high_level,"High","Low")
tel$prizm_social_one_dummy<-as.factor(tel$prizm_social_one_dummy)
unique(tel$prizm_social_one_dummy)

#Consider area as this is a categorical variable
tel%>%count(churn,levels=area)%>%filter(churn==1)->area_dat
area_dat$N<-unclass(tel%>%filter(area%in%area_dat$levels)%>%count(area))[[2]]
area_dat$ChurnPerc<-area_dat$n/area_dat$N
area_dat$Var.Name<-rep("area",nrow(area_dat))

mod<-rpart(churn~area,data = tel,method = "class")
unique(mod$where)

#With event_rate > 0.25 considered as high
high_level<-unclass(area_dat[(area_dat$ChurnPerc > 0.25),"levels"])[[1]]
tel$area_dummy<-ifelse(tel$area%in%high_level,"High","Low")
tel$area_dummy<-as.factor(tel$area_dummy)
unique(tel$area_dummy)

#Consider refurb_new as this is a categorical variable
tel%>%count(churn,levels=refurb_new)%>%filter(churn==1)->refurb_new_dat
refurb_new_dat$N<-unclass(tel%>%filter(refurb_new%in%refurb_new_dat$levels)%>%count(refurb_new))[[2]]
refurb_new_dat$ChurnPerc<-refurb_new_dat$n/refurb_new_dat$N
refurb_new_dat$Var.Name<-rep("refurb_new",nrow(refurb_new_dat))
#Missing value imputation
index<-which(is.na(tel$refurb_new))
tel[index,"refurb_new"]<-"N"
tel$refurb_new<-as.factor(tel$refurb_new)
unique(tel$refurb_new)

#Consider hnd_webcap as this is a categorical variable
tel%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->hnd_webcap_dat
hnd_webcap_dat$N<-unclass(tel%>%filter(hnd_webcap%in%hnd_webcap_dat$levels)%>%count(hnd_webcap))[[2]]
hnd_webcap_dat$ChurnPerc<-hnd_webcap_dat$n/hnd_webcap_dat$N
hnd_webcap_dat$Var.Name<-rep("hnd_webcap",nrow(hnd_webcap_dat))
#Missing value imputation
index<-which(is.na(tel$hnd_webcap))
tel[index,"hnd_webcap"]<-"WC"
tel$hnd_webcap<-as.factor(tel$hnd_webcap)
unique(tel$hnd_webcap)

#Consider marital as this is a categorical variable
tel%>%count(churn,levels=marital)%>%filter(churn==1)->marital_dat
marital_dat$N<-unclass(tel%>%filter(marital%in%marital_dat$levels)%>%count(marital))[[2]]
marital_dat$ChurnPerc<-marital_dat$n/marital_dat$N
marital_dat$Var.Name<-rep("marital",nrow(marital_dat))

mod<-rpart(churn~marital,data = tel,method = "class")
unique(mod$where)

#With event_rate > 0.24 considered as high
high_level<-c("A","B","U")
tel$marital_dummy<-ifelse(tel$marital%in%high_level,"High","Low")
tel$marital_dummy<-as.factor(tel$marital_dummy)
unique(tel$marital_dummy)

#Consider ethnic as this is a categorical variable
tel%>%count(churn,levels=ethnic)%>%filter(churn==1)->ethnic_dat
ethnic_dat$N<-unclass(tel%>%filter(ethnic%in%ethnic_dat$levels)%>%count(ethnic))[[2]]
ethnic_dat$ChurnPerc<-ethnic_dat$n/ethnic_dat$N
ethnic_dat$Var.Name<-rep("ethnic",nrow(ethnic_dat))

mod<-rpart(churn~ethnic,data = tel,method = "class")
unique(mod$where)

#With event_rate > 0.27 considered as high
high_level<-c("D","B","O")
tel$ethnic_dummy<-ifelse(tel$ethnic%in%high_level,"High","Low")
tel$ethnic_dummy<-as.factor(tel$ethnic_dummy)
unique(tel$ethnic_dummy)

#Consider age1 as this is a categorical variable
tel%>%count(churn,levels=age1)%>%filter(churn==1)->age1_dat
age1_dat$N<-unclass(tel%>%filter(age1%in%age1_dat$levels)%>%count(age1))[[2]]
age1_dat$ChurnPerc<-age1_dat$n/age1_dat$N
age1_dat$Var.Name<-rep("age1",nrow(age1_dat))

tel[(tel$age1==0),"age1"]<-24
tel[which(is.na(tel$age1)),"age1"]<-50
mod<-rpart(churn~age1,data = tel,method = "class")
unique(mod$where)


#With age 18-30 considered as high
tel$age1_dummy<-ifelse(tel$age1>=18 & tel$age1<=30,"High","Low")
tel$age1_dummy<-as.factor(tel$age1_dummy)
unique(tel$age1_dummy)

#Consider age2 as this is a categorical variable
tel%>%count(churn,levels=age2)%>%filter(churn==1)->age2_dat
age2_dat$N<-unclass(tel%>%filter(age2%in%age2_dat$levels)%>%count(age2))[[2]]
age2_dat$ChurnPerc<-age2_dat$n/age2_dat$N
age2_dat$Var.Name<-rep("age2",nrow(age2_dat))

tel[(tel$age2==0),"age2"]<-28
tel[which(is.na(tel$age2)),"age2"]<-78

mod<-rpart(churn~age2,data = tel,method = "class")
unique(mod$where)

#With age 18-30 considered as high
tel$age2_dummy<-ifelse(tel$age2>=18 & tel$age2<=30,"High","Low")
tel$age2_dummy<-as.factor(tel$age2_dummy)
unique(tel$age2_dummy)

#Consider models as this is a categorical variable
tel%>%count(churn,levels=models)%>%filter(churn==1)->models_dat
models_dat$N<-unclass(tel%>%filter(models%in%models_dat$levels)%>%count(models))[[2]]
models_dat$ChurnPerc<-models_dat$n/models_dat$N
models_dat$Var.Name<-rep("models",nrow(models_dat))

mod<-rpart(churn~models,data = tel,method = "class")
unique(mod$where)

#With event_rate >= 0.25 considered as high
high_level<-unclass(models_dat[(models_dat$ChurnPerc >= 0.25),"levels"])[[1]]
tel$models_dummy<-ifelse(tel$models%in%high_level,"High","Low")
tel$models_dummy<-as.factor(tel$models_dummy)
unique(tel$models_dummy)

#Consider hnd_price as this is a categorical variable
tel%>%count(churn,levels=hnd_price)%>%filter(churn==1)->hnd_price_dat
hnd_price_dat$N<-unclass(tel%>%filter(hnd_price%in%hnd_price_dat$levels)%>%count(hnd_price))[[2]]
hnd_price_dat$ChurnPerc<-hnd_price_dat$n/hnd_price_dat$N
hnd_price_dat$Var.Name<-rep("hnd_price",nrow(hnd_price_dat))

mod<-rpart(churn~hnd_price,data = tel,method = "class")
unique(mod$where)

#With event_rate >= 0.29 considered as high
high_level<-unclass(hnd_price_dat[(hnd_price_dat$ChurnPerc >= 0.29),"levels"])[[1]]
tel$hnd_price_dummy<-ifelse(tel$hnd_price%in%high_level,"High","Low")
tel$hnd_price_dummy<-as.factor(tel$hnd_price_dummy)
unique(tel$hnd_price_dummy)

#Consider actvsubs as this is a categorical variable
tel%>%count(churn,levels=actvsubs)%>%filter(churn==1)->actvsubs_dat
actvsubs_dat$N<-unclass(tel%>%filter(actvsubs%in%actvsubs_dat$levels)%>%count(actvsubs))[[2]]
actvsubs_dat$ChurnPerc<-actvsubs_dat$n/actvsubs_dat$N
actvsubs_dat$Var.Name<-rep("actvsubs",nrow(actvsubs_dat))

mod<-rpart(churn~actvsubs,data = tel,method = "class")
unique(mod$where)

#With event_rate >= 0.26 considered as high
high_level<-unclass(actvsubs_dat[(actvsubs_dat$ChurnPerc >= 0.26),"levels"])[[1]]
tel$actvsubs_dummy<-ifelse(tel$actvsubs%in%high_level,"High","Low")
tel$actvsubs_dummy<-as.factor(tel$actvsubs_dummy)
unique(tel$actvsubs_dummy)

#Consider uniqsubs as this is a categorical variable
tel%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->uniqsubs_dat
uniqsubs_dat$N<-unclass(tel%>%filter(uniqsubs%in%uniqsubs_dat$levels)%>%count(uniqsubs))[[2]]
uniqsubs_dat$ChurnPerc<-uniqsubs_dat$n/uniqsubs_dat$N
uniqsubs_dat$Var.Name<-rep("uniqsubs",nrow(uniqsubs_dat))

mod<-rpart(churn~uniqsubs,data = tel,method = "class")
unique(mod$where)

#With event_rate >= 0.27 considered as high
high_level<-unclass(uniqsubs_dat[(uniqsubs_dat$ChurnPerc >= 0.27),"levels"])[[1]]
tel$uniqsubs_dummy<-ifelse(tel$uniqsubs%in%high_level,"High","Low")
tel$uniqsubs_dummy<-as.factor(tel$uniqsubs_dummy)
unique(tel$uniqsubs_dummy)

#Consider forgntvl as this is a categorical variable
tel%>%count(churn,levels=forgntvl)%>%filter(churn==1)->forgntvl_dat
forgntvl_dat$N<-unclass(tel%>%filter(forgntvl%in%forgntvl_dat$levels)%>%count(forgntvl))[[2]]
forgntvl_dat$ChurnPerc<-forgntvl_dat$n/forgntvl_dat$N
forgntvl_dat$Var.Name<-rep("forgntvl",nrow(forgntvl_dat))

#Missing value imputation
index<-which(is.na(tel$forgntvl))
tel[index,"forgntvl"]<-1
tel$forgntvl<-as.factor(tel$forgntvl)
unique(tel$forgntvl)

#Consider opk_dat_Mean as it should be considered as categorical
tel%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->opk_dat_Mean
opk_dat_Mean$N<-unclass(tel%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%count(dec)%>%filter(dec%in%opk_dat_Mean$dec))[[2]]
opk_dat_Mean$churn_perc<-opk_dat_Mean$n/opk_dat_Mean$N
opk_dat_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%group_by(dec)%>%summarise(min(opk_dat_Mean))%>%filter(dec%in%opk_dat_Mean$dec))[[2]]
opk_dat_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%group_by(dec)%>%summarise(max(opk_dat_Mean))%>%filter(dec%in%opk_dat_Mean$dec))[[2]]
opk_dat_Mean$varname<-rep("opk_dat_Mean",nrow(opk_dat_Mean))

#With opk_dat_Mean >0 and <308 considered as low
tel$opk_dat_Mean_dummy<-ifelse(tel$opk_dat_Mean>0 & tel$opk_dat_Mean<=308,"Low","High")
tel$opk_dat_Mean_dummy<-as.factor(tel$opk_dat_Mean_dummy)
unique(tel$opk_dat_Mean_dummy)

#Consider mtrcycle as this is a categorical variable
tel%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->mtrcycle_dat
mtrcycle_dat$N<-unclass(tel%>%filter(mtrcycle%in%mtrcycle_dat$levels)%>%count(mtrcycle))[[2]]
mtrcycle_dat$ChurnPerc<-mtrcycle_dat$n/mtrcycle_dat$N
mtrcycle_dat$Var.Name<-rep("mtrcycle",nrow(mtrcycle_dat))

mod<-rpart(churn~mtrcycle,data = tel,method = "class")
unique(mod$where)

#Missing value imputation
index<-which(is.na(tel$mtrcycle))
tel[index,"mtrcycle"]<-"Missing"
tel$mtrcycle<-as.factor(tel$mtrcycle)
unique(tel$mtrcycle)

#Consider truck as this is a categorical variable
tel%>%count(churn,levels=truck)%>%filter(churn==1)->truck_dat
truck_dat$N<-unclass(tel%>%filter(truck%in%truck_dat$levels)%>%count(truck))[[2]]
truck_dat$ChurnPerc<-truck_dat$n/truck_dat$N
truck_dat$Var.Name<-rep("truck",nrow(truck_dat))

mod<-rpart(churn~truck,data = tel,method = "class")
unique(mod$where)

#Missing value imputation
index<-which(is.na(tel$truck))
tel[index,"truck"]<-"Missing"
tel$truck<-as.factor(tel$truck)
unique(tel$truck)

#Do not consider roam_Mean as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->roam_Mean
roam_Mean$N<-unclass(tel%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(dec)%>%filter(dec%in%roam_Mean$dec))[[2]]
roam_Mean$churn_perc<-roam_Mean$n/roam_Mean$N
roam_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(roam_Mean,n=10))%>%group_by(dec)%>%summarise(min(roam_Mean))%>%filter(dec%in%roam_Mean$dec))[[2]]
roam_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(roam_Mean,n=10))%>%group_by(dec)%>%summarise(max(roam_Mean))%>%filter(dec%in%roam_Mean$dec))[[2]]
roam_Mean$varname<-rep("roam_Mean",nrow(roam_Mean))

#Consider recv_sms_Mean as it should be considered as categorical
tel%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->recv_sms_Mean
recv_sms_Mean$N<-unclass(tel%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%count(dec)%>%filter(dec%in%recv_sms_Mean$dec))[[2]]
recv_sms_Mean$churn_perc<-recv_sms_Mean$n/recv_sms_Mean$N
recv_sms_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%group_by(dec)%>%summarise(min(recv_sms_Mean))%>%filter(dec%in%recv_sms_Mean$dec))[[2]]
recv_sms_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%group_by(dec)%>%summarise(max(recv_sms_Mean))%>%filter(dec%in%recv_sms_Mean$dec))[[2]]
recv_sms_Mean$varname<-rep("recv_sms_Mean",nrow(recv_sms_Mean))

#With recv_sms_Mean >0 and <518 considered as low
tel$recv_sms_Mean_dummy<-ifelse(tel$recv_sms_Mean>0 & tel$recv_sms_Mean<=518,"Low","High")
tel$recv_sms_Mean_dummy<-as.factor(tel$recv_sms_Mean_dummy)
unique(tel$recv_sms_Mean_dummy)

#Consider blck_dat_Mean as it should be considered as categorical
tel%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->blck_dat_Mean
blck_dat_Mean$N<-unclass(tel%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%count(dec)%>%filter(dec%in%blck_dat_Mean$dec))[[2]]
blck_dat_Mean$churn_perc<-blck_dat_Mean$n/blck_dat_Mean$N
blck_dat_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%group_by(dec)%>%summarise(min(blck_dat_Mean))%>%filter(dec%in%blck_dat_Mean$dec))[[2]]
blck_dat_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%group_by(dec)%>%summarise(max(blck_dat_Mean))%>%filter(dec%in%blck_dat_Mean$dec))[[2]]
blck_dat_Mean$varname<-rep("blck_dat_Mean",nrow(blck_dat_Mean))

#With blck_dat_Mean >0 and <414 considered as low
tel$blck_dat_Mean_dummy<-ifelse(tel$blck_dat_Mean>0 & tel$blck_dat_Mean<=414,"Low","High")
tel$blck_dat_Mean_dummy<-as.factor(tel$blck_dat_Mean_dummy)
unique(tel$blck_dat_Mean_dummy)

#Consider mou_pead_Mean as it should be considered as categorical
tel%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->mou_pead_Mean
mou_pead_Mean$N<-unclass(tel%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%count(dec)%>%filter(dec%in%mou_pead_Mean$dec))[[2]]
mou_pead_Mean$churn_perc<-mou_pead_Mean$n/mou_pead_Mean$N
mou_pead_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_pead_Mean))%>%filter(dec%in%mou_pead_Mean$dec))[[2]]
mou_pead_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_pead_Mean))%>%filter(dec%in%mou_pead_Mean$dec))[[2]]
mou_pead_Mean$varname<-rep("mou_pead_Mean",nrow(mou_pead_Mean))

#With mou_pead_Mean >0 and <1037 considered as low
tel$mou_pead_Mean_dummy<-ifelse(tel$mou_pead_Mean>0 & tel$mou_pead_Mean<=1037,"Low","High")
tel$mou_pead_Mean_dummy<-as.factor(tel$mou_pead_Mean_dummy)
unique(tel$mou_pead_Mean_dummy)

#Consider car_buy as this is a categorical variable
tel%>%count(churn,levels=car_buy)%>%filter(churn==1)->car_buy_dat
car_buy_dat$N<-unclass(tel%>%filter(car_buy%in%car_buy_dat$levels)%>%count(car_buy))[[2]]
car_buy_dat$ChurnPerc<-car_buy_dat$n/car_buy_dat$N
car_buy_dat$Var.Name<-rep("car_buy",nrow(car_buy_dat))

mod<-rpart(churn~car_buy,data = tel,method = "class")
unique(mod$where)

#Missing value imputation
index<-which(is.na(tel$car_buy))
tel[index,"car_buy"]<-"Missing"
tel$car_buy<-as.factor(tel$car_buy)
unique(tel$car_buy)

#Consider csa as this is a categorical variable
tel%>%count(churn,levels=csa)%>%filter(churn==1)->csa_dat
csa_dat$N<-unclass(tel%>%filter(csa%in%csa_dat$levels)%>%count(csa))[[2]]
csa_dat$ChurnPerc<-csa_dat$n/csa_dat$N
csa_dat$Var.Name<-rep("csa",nrow(csa_dat))

mod<-rpart(churn~csa,data = tel,method = "class")
unique(mod$where)

#With event_rate >= 0.5 considered as high
high_level<-unclass(csa_dat[(csa_dat$ChurnPerc >= 0.5),"levels"])[[1]]
tel$csa_dummy<-ifelse(tel$csa%in%high_level,"High","Low")
tel$csa_dummy<-as.factor(tel$csa_dummy)
unique(tel$csa_dummy)

#Do not consider da_Mean as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(da_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->da_Mean
da_Mean$N<-unclass(tel%>%mutate(dec=ntile(da_Mean,n=10))%>%count(dec)%>%filter(dec%in%da_Mean$dec))[[2]]
da_Mean$churn_perc<-da_Mean$n/da_Mean$N
da_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(da_Mean,n=10))%>%group_by(dec)%>%summarise(min(da_Mean))%>%filter(dec%in%da_Mean$dec))[[2]]
da_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(da_Mean,n=10))%>%group_by(dec)%>%summarise(max(da_Mean))%>%filter(dec%in%da_Mean$dec))[[2]]
da_Mean$varname<-rep("da_Mean",nrow(da_Mean))

#Do not consider da_Range as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(da_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->da_Range
da_Range$N<-unclass(tel%>%mutate(dec=ntile(da_Range,n=10))%>%count(dec)%>%filter(dec%in%da_Range$dec))[[2]]
da_Range$churn_perc<-da_Range$n/da_Range$N
da_Range$GreaterThan<-unclass(tel%>%mutate(dec=ntile(da_Range,n=10))%>%group_by(dec)%>%summarise(min(da_Range))%>%filter(dec%in%da_Range$dec))[[2]]
da_Range$LessThan<-unclass(tel%>%mutate(dec=ntile(da_Range,n=10))%>%group_by(dec)%>%summarise(max(da_Range))%>%filter(dec%in%da_Range$dec))[[2]]
da_Range$varname<-rep("da_Range",nrow(da_Range))

#Consider datovr_Mean as it should be cosidered as categorical
tel%>%mutate(dec=ntile(datovr_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->datovr_Mean
datovr_Mean$N<-unclass(tel%>%mutate(dec=ntile(datovr_Mean,n=10))%>%count(dec)%>%filter(dec%in%datovr_Mean$dec))[[2]]
datovr_Mean$churn_perc<-datovr_Mean$n/datovr_Mean$N
datovr_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(datovr_Mean,n=10))%>%group_by(dec)%>%summarise(min(datovr_Mean))%>%filter(dec%in%datovr_Mean$dec))[[2]]
datovr_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(datovr_Mean,n=10))%>%group_by(dec)%>%summarise(max(datovr_Mean))%>%filter(dec%in%datovr_Mean$dec))[[2]]
datovr_Mean$varname<-rep("datovr_Mean",nrow(datovr_Mean))

mod<-rpart(churn~datovr_Mean,data = tel,method = "class")
unique(mod$where)

#Missing value imputation
index<-which(is.na(tel$datovr_Mean))
tel[index,"datovr_Mean"]<-0
#With datovr_Mean >0 and <243 considered as low
tel$datovr_Mean_dummy<-ifelse(tel$datovr_Mean>0 & tel$datovr_Mean<=243,"Low","High")
tel$datovr_Mean_dummy<-as.factor(tel$datovr_Mean_dummy)
unique(tel$datovr_Mean_dummy)

#Do not consider datovr_Range as it looks similar to datovr_Mean
tel%>%mutate(dec=ntile(datovr_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->datovr_Range
datovr_Range$N<-unclass(tel%>%mutate(dec=ntile(datovr_Range,n=10))%>%count(dec)%>%filter(dec%in%datovr_Range$dec))[[2]]
datovr_Range$churn_perc<-datovr_Range$n/datovr_Range$N
datovr_Range$GreaterThan<-unclass(tel%>%mutate(dec=ntile(datovr_Range,n=10))%>%group_by(dec)%>%summarise(min(datovr_Range))%>%filter(dec%in%datovr_Range$dec))[[2]]
datovr_Range$LessThan<-unclass(tel%>%mutate(dec=ntile(datovr_Range,n=10))%>%group_by(dec)%>%summarise(max(datovr_Range))%>%filter(dec%in%datovr_Range$dec))[[2]]
datovr_Range$varname<-rep("datovr_Range",nrow(datovr_Range))

#Consider drop_dat_Mean as it shuold be considered as categorical 
tel%>%mutate(dec=ntile(drop_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->drop_dat_Mean
drop_dat_Mean$N<-unclass(tel%>%mutate(dec=ntile(drop_dat_Mean,n=10))%>%count(dec)%>%filter(dec%in%drop_dat_Mean$dec))[[2]]
drop_dat_Mean$churn_perc<-drop_dat_Mean$n/drop_dat_Mean$N
drop_dat_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(drop_dat_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_dat_Mean))%>%filter(dec%in%drop_dat_Mean$dec))[[2]]
drop_dat_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(drop_dat_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_dat_Mean))%>%filter(dec%in%drop_dat_Mean$dec))[[2]]
drop_dat_Mean$varname<-rep("drop_dat_Mean",nrow(drop_dat_Mean))

mod<-rpart(churn~drop_dat_Mean,data = tel,method = "class")
unique(mod$where)

#With drop_dat_Mean >0 and <208 considered as low
tel$drop_dat_Mean_dummy<-ifelse(tel$drop_dat_Mean>0 & tel$drop_dat_Mean<=208,"Low","High")
tel$drop_dat_Mean_dummy<-as.factor(tel$drop_dat_Mean_dummy)
unique(tel$drop_dat_Mean_dummy)

#Consider drop_vce_Mean as it may be significant
tel%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->drop_vce_Mean
drop_vce_Mean$N<-unclass(tel%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%filter(dec%in%drop_vce_Mean$dec))[[2]]
drop_vce_Mean$churn_perc<-drop_vce_Mean$n/drop_vce_Mean$N
drop_vce_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean))%>%filter(dec%in%drop_vce_Mean$dec))[[2]]
drop_vce_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean))%>%filter(dec%in%drop_vce_Mean$dec))[[2]]
drop_vce_Mean$varname<-rep("drop_vce_Mean",nrow(drop_vce_Mean))

#Do not consider adjmou as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->adjmou
adjmou$N<-unclass(tel%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%filter(dec%in%adjmou$dec))[[2]]
adjmou$churn_perc<-adjmou$n/adjmou$N
adjmou$GreaterThan<-unclass(tel%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou))%>%filter(dec%in%adjmou$dec))[[2]]
adjmou$LessThan<-unclass(tel%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou))%>%filter(dec%in%adjmou$dec))[[2]]
adjmou$varname<-rep("adjmou",nrow(adjmou))

#Consider adjrev as it may be significant
tel%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->adjrev
adjrev$N<-unclass(tel%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%filter(dec%in%adjrev$dec))[[2]]
adjrev$churn_perc<-adjrev$n/adjrev$N
adjrev$GreaterThan<-unclass(tel%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev))%>%filter(dec%in%adjrev$dec))[[2]]
adjrev$LessThan<-unclass(tel%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev))%>%filter(dec%in%adjrev$dec))[[2]]
adjrev$varname<-rep("adjrev",nrow(adjrev))

#Do not consider avgrev as there is no pattern in churn percentage
tel%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->avgrev
avgrev$N<-unclass(tel%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%filter(dec%in%avgrev$dec))[[2]]
avgrev$churn_perc<-avgrev$n/avgrev$N
avgrev$GreaterThan<-unclass(tel%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev))%>%filter(dec%in%avgrev$dec))[[2]]
avgrev$LessThan<-unclass(tel%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev))%>%filter(dec%in%avgrev$dec))[[2]]
avgrev$varname<-rep("avgrev",nrow(avgrev))


#Consider comp_dat_Mean as it should be considered as categorical
tel%>%mutate(dec=ntile(comp_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->comp_dat_Mean
comp_dat_Mean$N<-unclass(tel%>%mutate(dec=ntile(comp_dat_Mean,n=10))%>%count(dec)%>%filter(dec%in%comp_dat_Mean$dec))[[2]]
comp_dat_Mean$churn_perc<-comp_dat_Mean$n/comp_dat_Mean$N
comp_dat_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(comp_dat_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_dat_Mean))%>%filter(dec%in%comp_dat_Mean$dec))[[2]]
comp_dat_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(comp_dat_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_dat_Mean))%>%filter(dec%in%comp_dat_Mean$dec))[[2]]
comp_dat_Mean$varname<-rep("comp_dat_Mean",nrow(comp_dat_Mean))

mod<-rpart(churn~comp_dat_Mean,data = tel,method = "class")
unique(mod$where)

#With comp_dat_Mean >0 and <560 considered as low
tel$comp_dat_Mean_dummy<-ifelse(tel$comp_dat_Mean>0 & tel$comp_dat_Mean<=560,"Low","High")
tel$comp_dat_Mean_dummy<-as.factor(tel$comp_dat_Mean_dummy)
unique(tel$comp_dat_Mean_dummy)

#Consider plcd_dat_Mean as it should be considered as categorical
tel%>%mutate(dec=ntile(plcd_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->plcd_dat_Mean
plcd_dat_Mean$N<-unclass(tel%>%mutate(dec=ntile(plcd_dat_Mean,n=10))%>%count(dec)%>%filter(dec%in%plcd_dat_Mean$dec))[[2]]
plcd_dat_Mean$churn_perc<-plcd_dat_Mean$n/plcd_dat_Mean$N
plcd_dat_Mean$GreaterThan<-unclass(tel%>%mutate(dec=ntile(plcd_dat_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_dat_Mean))%>%filter(dec%in%plcd_dat_Mean$dec))[[2]]
plcd_dat_Mean$LessThan<-unclass(tel%>%mutate(dec=ntile(plcd_dat_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_dat_Mean))%>%filter(dec%in%plcd_dat_Mean$dec))[[2]]
plcd_dat_Mean$varname<-rep("plcd_dat_Mean",nrow(plcd_dat_Mean))

mod<-rpart(churn~plcd_dat_Mean,data = tel,method = "class")
unique(mod$where)

#With plcd_dat_Mean >0 and <734 considered as low
tel$plcd_dat_Mean_dummy<-ifelse(tel$plcd_dat_Mean>0 & tel$plcd_dat_Mean<=734,"Low","High")
tel$plcd_dat_Mean_dummy<-as.factor(tel$plcd_dat_Mean_dummy)
unique(tel$plcd_dat_Mean_dummy)

# Need to create following derived variables
# cost_call : It is the cost per call which is TOTREV/TOTCALLS
# call_completion_percentage : It is for network issues which is COMP_VCE_MEAN/PLCD_VCE_MEAN
# completion_dat_percentage : It is for data connectivity issue which is COMP_DAT_MEAN/PLCD_DAT_MEAN
# non_opt_rate_plan : It is for non optimal rate plan which is OVRREV_MEAN/TOTREV

tel$cost_call<-ifelse(tel$totcalls==0,0,tel$totrev/tel$totcalls)
tel$call_completion_percentage<-ifelse(tel$comp_vce_Mean==0 | tel$plcd_vce_Mean==0,0,tel$comp_vce_Mean/tel$plcd_vce_Mean)
tel$completion_dat_percentage<-ifelse(tel$comp_dat_Mean==0 | tel$plcd_dat_Mean==0,0,tel$comp_dat_Mean/tel$plcd_dat_Mean)
tel$non_opt_rate_plan<-tel$ovrrev_Mean/tel$totrev

#Consider cost_call as it might be significant
tel%>%mutate(dec=ntile(cost_call,n=10))%>%count(churn,dec)%>%filter(churn==1)->cost_call
cost_call$N<-unclass(tel%>%mutate(dec=ntile(cost_call,n=10))%>%count(dec)%>%filter(dec%in%cost_call$dec))[[2]]
cost_call$churn_perc<-cost_call$n/cost_call$N
cost_call$GreaterThan<-unclass(tel%>%mutate(dec=ntile(cost_call,n=10))%>%group_by(dec)%>%summarise(min(cost_call))%>%filter(dec%in%cost_call$dec))[[2]]
cost_call$LessThan<-unclass(tel%>%mutate(dec=ntile(cost_call,n=10))%>%group_by(dec)%>%summarise(max(cost_call))%>%filter(dec%in%cost_call$dec))[[2]]
cost_call$varname<-rep("cost_call",nrow(cost_call))

#Consider call_completion_percentage as it might be significant
tel%>%mutate(dec=ntile(call_completion_percentage,n=10))%>%count(churn,dec)%>%filter(churn==1)->call_completion_percentage
call_completion_percentage$N<-unclass(tel%>%mutate(dec=ntile(call_completion_percentage,n=10))%>%count(dec)%>%filter(dec%in%call_completion_percentage$dec))[[2]]
call_completion_percentage$churn_perc<-call_completion_percentage$n/call_completion_percentage$N
call_completion_percentage$GreaterThan<-unclass(tel%>%mutate(dec=ntile(call_completion_percentage,n=10))%>%group_by(dec)%>%summarise(min(call_completion_percentage))%>%filter(dec%in%call_completion_percentage$dec))[[2]]
call_completion_percentage$LessThan<-unclass(tel%>%mutate(dec=ntile(call_completion_percentage,n=10))%>%group_by(dec)%>%summarise(max(call_completion_percentage))%>%filter(dec%in%call_completion_percentage$dec))[[2]]
call_completion_percentage$varname<-rep("call_completion_percentage",nrow(call_completion_percentage))

#Consider completion_dat_percentage as it might be significant
tel%>%mutate(dec=ntile(completion_dat_percentage,n=10))%>%count(churn,dec)%>%filter(churn==1)->completion_dat_percentage
completion_dat_percentage$N<-unclass(tel%>%mutate(dec=ntile(completion_dat_percentage,n=10))%>%count(dec)%>%filter(dec%in%completion_dat_percentage$dec))[[2]]
completion_dat_percentage$churn_perc<-completion_dat_percentage$n/completion_dat_percentage$N
completion_dat_percentage$GreaterThan<-unclass(tel%>%mutate(dec=ntile(completion_dat_percentage,n=10))%>%group_by(dec)%>%summarise(min(completion_dat_percentage))%>%filter(dec%in%completion_dat_percentage$dec))[[2]]
completion_dat_percentage$LessThan<-unclass(tel%>%mutate(dec=ntile(completion_dat_percentage,n=10))%>%group_by(dec)%>%summarise(max(completion_dat_percentage))%>%filter(dec%in%completion_dat_percentage$dec))[[2]]
completion_dat_percentage$varname<-rep("completion_dat_percentage",nrow(completion_dat_percentage))

#Consider non_opt_rate_plan as it might be significant
tel%>%mutate(dec=ntile(non_opt_rate_plan,n=10))%>%count(churn,dec)%>%filter(churn==1)->non_opt_rate_plan
non_opt_rate_plan$N<-unclass(tel%>%mutate(dec=ntile(non_opt_rate_plan,n=10))%>%count(dec)%>%filter(dec%in%non_opt_rate_plan$dec))[[2]]
non_opt_rate_plan$churn_perc<-non_opt_rate_plan$n/non_opt_rate_plan$N
non_opt_rate_plan$GreaterThan<-unclass(tel%>%mutate(dec=ntile(non_opt_rate_plan,n=10))%>%group_by(dec)%>%summarise(min(non_opt_rate_plan))%>%filter(dec%in%non_opt_rate_plan$dec))[[2]]
non_opt_rate_plan$LessThan<-unclass(tel%>%mutate(dec=ntile(non_opt_rate_plan,n=10))%>%group_by(dec)%>%summarise(max(non_opt_rate_plan))%>%filter(dec%in%non_opt_rate_plan$dec))[[2]]
non_opt_rate_plan$varname<-rep("non_opt_rate_plan",nrow(non_opt_rate_plan))

names(tel)
data<-tel[,c(1,6,10:12,19,21:23,28,31,34,35,44,46:47,52:53,60,62,63,65,68:91)]
names(data)

#Random sampling
set.seed(100)
sampling<-sort(sample(nrow(data),nrow(data)*.8))
train<-data[sampling,]
test<-data[-sampling,]

model<-glm(churn ~ ., data = train[,-22], family = binomial)

summary(model)

step(model,direction = "both")


model2<-glm(formula = churn ~ mou_Mean +  months + totcalls + 
      eqpdays + ovrrev_Mean + ovrmou_Mean + asl_flag + refurb_new + 
      drop_vce_Mean + totrev + adjrev + crclscod_dummy + prizm_social_one_dummy + 
      area_dummy + marital_dummy + ethnic_dummy + age1_dummy + 
      age2_dummy + models_dummy + hnd_price_dummy + actvsubs_dummy + 
      uniqsubs_dummy + csa_dummy + datovr_Mean_dummy + cost_call + 
      call_completion_percentage + non_opt_rate_plan, family = binomial, 
    data = train[, -22])

#Create dummies
train$asl_flagY<-ifelse(train$asl_flag=="Y",1,0)
test$asl_flagY<-ifelse(test$asl_flag=="Y",1,0)

train$refurb_newR<-ifelse(train$refurb_new=="R",1,0)
test$refurb_newR<-ifelse(test$refurb_new=="R",1,0)

train$crclscod_dummyLow<-ifelse(train$crclscod_dummy=="Low",1,0)
test$crclscod_dummyLow<-ifelse(test$crclscod_dummy=="Low",1,0)

train$prizm_social_one_dummyLow<-ifelse(train$prizm_social_one_dummy=="Low",1,0)
test$prizm_social_one_dummyLow<-ifelse(test$prizm_social_one_dummy=="Low",1,0)

train$area_dummyLow<-ifelse(train$area_dummy=="Low",1,0)
test$area_dummyLow<-ifelse(test$area_dummy=="Low",1,0)

train$marital_dummyLow<-ifelse(train$marital_dummy=="Low",1,0)
test$marital_dummyLow<-ifelse(test$marital_dummy=="Low",1,0)

train$ethnic_dummyLow<-ifelse(train$ethnic_dummy=="Low",1,0)
test$ethnic_dummyLow<-ifelse(test$ethnic_dummy=="Low",1,0)

train$age1_dummyLow<-ifelse(train$age1_dummy=="Low",1,0)
test$age1_dummyLow<-ifelse(test$age1_dummy=="Low",1,0)

train$age2_dummyLow<-ifelse(train$age2_dummy=="Low",1,0)
test$age2_dummyLow<-ifelse(test$age2_dummy=="Low",1,0)

train$models_dummyLow<-ifelse(train$models_dummy=="Low",1,0)
test$models_dummyLow<-ifelse(test$models_dummy=="Low",1,0)

train$hnd_price_dummyLow<-ifelse(train$hnd_price_dummy=="Low",1,0)
test$hnd_price_dummyLow<-ifelse(test$hnd_price_dummy=="Low",1,0)

train$actvsubs_dummyLow<-ifelse(train$actvsubs_dummy=="Low",1,0)
test$actvsubs_dummyLow<-ifelse(test$actvsubs_dummy=="Low",1,0)

train$uniqsubs_dummyLow<-ifelse(train$uniqsubs_dummy=="Low",1,0)
test$uniqsubs_dummyLow<-ifelse(test$uniqsubs_dummy=="Low",1,0)

train$csa_dummyLow<-ifelse(train$csa_dummy=="Low",1,0)
test$csa_dummyLow<-ifelse(test$csa_dummy=="Low",1,0)

train$datovr_Mean_dummyLow<-ifelse(train$datovr_Mean_dummy=="Low",1,0)
test$datovr_Mean_dummyLow<-ifelse(test$datovr_Mean_dummy=="Low",1,0)

model2<-glm(formula = churn ~ mou_Mean +  months + totcalls + 
              eqpdays + ovrrev_Mean + ovrmou_Mean + asl_flagY + refurb_newR + 
              drop_vce_Mean + totrev + adjrev + crclscod_dummyLow + prizm_social_one_dummyLow + 
              area_dummyLow + marital_dummyLow + ethnic_dummyLow + age1_dummyLow + 
              age2_dummyLow + models_dummyLow + hnd_price_dummyLow + actvsubs_dummyLow + 
              uniqsubs_dummyLow + csa_dummyLow + datovr_Mean_dummyLow + cost_call + 
              call_completion_percentage + non_opt_rate_plan, family = binomial, 
            data = train[, -22])

summary(model2)

prob<-predict(model2,type="response",newdata=test)
pred<-prediction(prob,test$churn)
auc<-performance(pred,"auc")

#Top 30% probability corresponds to 41% of customers to churn
#Top 40% probability corresponds to 53% of customers to churn
gains(test$churn,predict(model2,type="response",newdata=test),groups = 10)

test$prob<-predict(model2,type="response",newdata=test)
quantile(test$prob,prob=seq(1,10)/10)

#40% of top probability to churn lies between 0.2513008 & 0.8057382
#Target the customers with above mentioned probability to churn
targetted<-test[test$prob>0.2513008 & test$prob<=0.8057382,"Customer_ID"]
