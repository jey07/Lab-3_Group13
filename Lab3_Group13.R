

#Reading SWedish file
swedfile<-read.csv("Swedish_Household.csv")

levels(swedfile$age)<-c("Young","Adult","Senior")
colnames(swedfile)<-c("Region","Age","Mean_Value(SEK thousands)")

z<-strsplit(levels(swedfile$Region)," ")
levels(swedfile$Region)<-sapply(z,function(y) y[[2]])

