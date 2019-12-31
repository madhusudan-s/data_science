setwd("C:\\Rproj\\R-Project")

mmWoes<-read.csv("MMwoes.csv", header = TRUE, skip = 1)

mmWoes$loan_amnt<-as.integer(mmWoes$loan_amnt)
summary(mmWoes$next_pymnt_d)
str(mmWoes)

?write.csv

candidates<-read.csv("candidates.csv")
contributions<-read.csv("contributions.csv")
can_r<-candidates[candidates$party=="R",]
can_r$candidate_id<-can_r$id
names(can_r)[1]<-"candidate_id"
